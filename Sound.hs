{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Main where

import      Data.Int
import      Data.WAVE

-- Parameters for creating sounds
smpPerFrame, framePerSec, bitRate :: Int
smpPerFrame = 1
framePerSec = 16000
bitRate  = 32
smpPerSec = smpPerFrame * framePerSec

-- (time -> sound at that time, length of Sound a)
type WAVEFunc a = (Int -> a, Int)
type Sample     = [Int32]
type Samples    = [Sample]

data Sound a where
  SSolid  :: a -> Sound a
  STime   :: Sound Double
  SSound  :: Sound Double -> Int -> Int32 -> Sound Sample
  SStart  :: Int -> Sound a -> Sound a
  SConcat :: Sound Sample -> Sound Sample -> Sound Sample
  STrans  :: Int -> Sound Sample -> Sound Sample -> Sound Sample
  SSpeed  :: Double -> Sound a -> Sound a
  SMap    :: (a -> b) -> Sound a -> Sound b
  SZip    :: (a -> b -> c) -> Sound a -> Sound b -> Sound c

solid :: a -> Sound a
solid = SSolid

t :: Sound Double
t = STime

-- Make basic sound with frequency, length, volume
sound :: Sound Double -> Double -> Int -> Sound Sample
sound f l i = SSound f (floor $ l * (fromIntegral framePerSec)) ((fromIntegral i) :: Int32)

-- Start Sound a `d` later
delay :: Double -> Sound a -> Sound a
delay d s = SStart (floor $ d * (fromIntegral framePerSec)) s

-- Sound a then Sound b
(-->) :: Sound Sample -> Sound Sample -> Sound Sample -- Sound1 then Sound2
(-->) = SConcat

-- Sound a transitions into Sound b
trans :: Double -> Sound Sample -> Sound Sample -> Sound Sample
trans d s1 s2 = STrans (floor $ d * (fromIntegral framePerSec)) s1 s2

-- Slow Sound a
slow :: Double -> Sound a -> Sound a
slow d s = SSpeed (1 / d) s

-- Speed Sound a
speed :: Double -> Sound a -> Sound a
speed = SSpeed

zipSound :: (a -> b -> c) -> Sound a -> Sound b -> Sound c
zipSound = SZip

mapSound :: (a -> b) -> Sound a -> Sound b
mapSound = SMap

-- commonly used volumes
full, half, third, quarter :: Int
full    = maxBound
half    = maxBound `div` 2
third   = maxBound `div` 3
quarter = maxBound `div` 4

-- unfortunately, zipWith throws away the 'extras' in the lists
-- we want to keep as and bs
opList :: (Int32 -> Int32 -> Int32) -> [Int32] -> [Int32] -> [Int32]
opList _ as []           = as
opList _ [] bs           = bs
opList (&) (a:as) (b:bs) = a & b : opList (&) as bs

instance Num Sample where
  (+)           = opList (+)
  (-)           = opList (-)
  (*)           = opList (*)
  abs           = fmap abs
  signum        = fmap signum
  fromInteger i = [fromInteger i]

instance Num a => Num (Sound a) where
  (+)           = zipSound (+)
  (-)           = zipSound (-)
  (*)           = zipSound (*)
  abs           = mapSound abs
  signum        = mapSound signum
  fromInteger i = solid (fromInteger i)

instance Fractional a => Fractional (Sound a) where
  fromRational = solid . fromRational
  (/)          = zipSound (/)

instance Floating a => Floating (Sound a) where
  pi    = solid pi
  exp   = mapSound exp
  log   = mapSound log
  sin   = mapSound sin
  cos   = mapSound cos
  asin  = mapSound asin
  acos  = mapSound acos
  atan  = mapSound atan
  sinh  = mapSound sinh
  cosh  = mapSound cosh
  asinh = mapSound asinh
  acosh = mapSound acosh
  atanh = mapSound atanh

interp :: Sound a -> Int -> WAVEFunc a
interp (SSolid s) _         = (\t -> s, -1)
interp (STime) _            = (\t -> fromIntegral t :: Double, -1)
interp (SStart nst s) st   = (\t -> (fst v) t, nst + (snd v))
                              where
                                v = interp s (nst + st)
interp (SSound f l v) st = (out, l)
                                where
                                  fr    = fromIntegral framePerSec
                                  out t = case (t > st) && ((t - st) < l) of
                                    True -> ((round $ vol * sin (t2 * freq * 2 * pi / fr)) :: Int32) : []
                                      where
                                        vol  = fromIntegral v
                                        t2   = (fromIntegral t) :: Double
                                        freq = fst (interp f st) $ t
                                    _    -> [0 :: Int32]
interp (SSpeed d s) st     = (out, newlen)
                            where
                              v      = interp s st
                              len    = snd v
                              newlen = floor $ (fromIntegral len :: Double) * d
                              out t = fst v $ st + offset
                                where
                                  tDub   = fromIntegral t :: Double
                                  stDub  = fromIntegral st :: Double
                                  mult1  = (tDub - stDub) :: Double
                                  mult   = mult1 * d
                                  offset = floor mult :: Int
interp (SConcat s1 s2) st  = (out, len)
                            where
                              v1       = interp s1 st
                              lenFirst = snd v1
                              v2       = interp s2 (st + lenFirst)
                              len      = lenFirst + (snd v2)
                              out t = case t < st of -- after start
                                     False -> case t < (st + lenFirst) of -- before end of s1
                                        True -> fst v1 $ t
                                        _    -> case t < (st + len) of -- before end of s2
                                          True -> fst v2 $ t
                                          _    -> [0]
                                     _    -> [0]
interp (STrans d s1 s2) st = (out, len)
                              where
                                v1       = interp s1 st
                                lenFirst = snd v1
                                st2      = st + lenFirst - d
                                v2       = interp s2 st2
                                len      = lenFirst + snd v2 - d
                                out t = case t < st of
                                  False -> case t < st2 of
                                    False -> case t < st + lenFirst of
                                      True -> case dist == 0 of
                                        False -> (fst v1 $ t) + (fst v2 $ t)--(fmap (mapHelper dist (*)) (fst v2 $ t)) + (fmap (mapHelper dist (/)) (fst v1 $ t))
                                        _     -> fst v1 $ t
                                      _    -> fst v2 $ t
                                    _     -> fst v1 $ t
                                  _     -> [0]
                                  where
                                    temp, dist :: Double
                                    temp = (fromIntegral t :: Double) - (fromIntegral st2 :: Double)
                                    dist = temp / (fromIntegral d :: Double)

interp (SMap f s) st       = (\t -> f $ (fst v) t, snd v)
                              where
                                v = interp s st
interp (SZip (&) s1 s2) st = (func, len)
                              where
                                v1 = interp s1 st
                                v2 = interp s2 st
                                len = max (snd v1) (snd v2)
                                func = \t -> ((fst v1) t) & ((fst v2) t)

mapHelper :: Double -> (Double -> Double -> Double) -> (Int32 -> Int32)
mapHelper i2 (&) = \i -> floor (((fromIntegral i) :: Double) & i2) :: Int32

header :: WAVEHeader
header = WAVEHeader smpPerFrame framePerSec bitRate Nothing

putWAVE32 :: Samples -> String -> IO ()
putWAVE32 w s = putWAVEFile s (WAVE header w)

putWAVE :: WAVEFunc Sample -> Double -> Maybe String -> IO ()
putWAVE f len Nothing  = putWAVE32 (getSamples 0 len f) "sounds/example.wav"
putWAVE f len (Just s) = putWAVE32 (getSamples 0 len f) s

getSamples :: Int -> Double -> WAVEFunc Sample -> Samples
getSamples n len f
    | n >= floor ((fromIntegral smpPerSec) * len) = [[]]
    | otherwise = (fst f) n : getSamples (n + 1) len f

-- Output .wav file for Sound Sample
makeSound :: Sound Sample -> Double -> IO ()
makeSound s len = putWAVE (interp s 0) len Nothing

-- -- Output .wav file for Sound Sample with title t
makeSoundTitled :: Sound Sample -> Double -> String -> IO ()
makeSoundTitled s len t = putWAVE (interp s 0) len (Just $ "sounds/" ++ t ++ ".wav")

-- My examples:
basic :: Sound Sample
basic = sound (solid 600) 2.5 half

freqT :: Sound Sample
freqT = sound t 3.0 half

addEx :: Sound Sample
addEx = basic + freqT

concatEx :: Sound Sample
concatEx = basic --> freqT

trans1 :: Sound Sample
trans1 = trans 2 basic freqT

trans2 :: Sound Sample
trans2 = trans 1.5 freqT (sound (t / 2) 4.0 third)

noSpeed, speedx2, halfSpeed :: Sound Sample
noSpeed = sound (solid 400.0) 2.5 half
speedx2   = speed 2.0 noSpeed
halfSpeed = slow  2.0 noSpeed

concatSpeedx2 = speed 2.0 concatEx
concatHalfSpeed = slow 2.0 concatEx

sinEx :: Sound Sample
sinEx = sound (sin t) 3.0 half

cosEx :: Sound Sample
cosEx = sound (cos t) 3.0 half

tanEx :: Sound Sample
tanEx = sound (tan t) 3.0 half

-- values and equation from http://www.johndcook.com/blog/2016/03/10/creating-police-siren-sounds-with-frequency-modulation/
f_c = 1500
f_m = 2
beta = 100
tpi  = 3.1415
siren :: Sound Sample
siren = sound (2*tpi*f_c*t - beta*sin(2*f_m*tpi*t)) 5.0 half
--siren = sound (sin(tpi * t)) 5.0 half


-- this function from http://stackoverflow.com/questions/5658391/generating-wav-sound-data-in-haskell --
wave :: Double -- | Frequency
      -> Double -- | Length of sound in seconds
      -> Int32  -- | Volume, (maxBound :: Int32) for highest, 0 for lowest
      -> Sample
wave freq len volume = take t
                         (map (round . (* fromIntegral volume)) $
                         map sin [0.0, (freq * 2 * pi / f)..])
                         where
                           f = fromIntegral framePerSec
                           t = floor (len * f)

-- WAVE library examples
soundEx :: Samples
soundEx = map (:[]) $ wave 600 3 (maxBound `div` 2)

soundEx2 :: Samples -- play two tones at once
soundEx2 = map (:[]) $ zipWith (+) (wave 600 3 (maxBound `div` 2)) (wave 1000 3 (maxBound `div` 2))
