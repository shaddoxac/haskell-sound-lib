The Sound library is a Haskell library for creating sounds or series of sounds.

## Installation
Refer to the Installation.txt file provided.

## Use
The main functions of the Sound library are makeSound and makeSoundTitled.
These functions take a Sound Sample (aka Sound [Int32]) object and a double
indicating the length of the sound to be produced. makeSoundTitled also takes a
string that is the name of the file produced. makeSound uses a default name for
the file produced. The produced sound is saved as a .wav file inside the sounds
directory.


## Operations

There are 9 constructors for Sounds:

SSolid (solid) is a Sound a that contains an a.

STime (t) is a Sound Double that returns the current time.

SSound (sound) is the constructor for a basic sound. It takes a Sound Double
indicating the frequency, a Double indicating the length, and an int indicating
the volume.

SStart (delay) is a Sound a that takes a Double and a Sound a and delays the Sound a
by the Double's amount of time.

SConcat (-->) concatenates two Sound Samples together, so that the first follows the
second.

SSpeed (slow, speed) takes a Double and a Sound a and makes that Sound a faster by a factor
of its Double.

STrans (trans) takes a Double and two Sound as, then transitions from the first Sound a
to the second Sound a the Double amount of time before the first Sound a ends.

SMap (mapSound) maps a function and a Sound a and maps the function to the Sound a.

SZip (zipSound) maps a function between a Sound a and a Sound b.

Sounds also support basic arithmetic operations such as addition and multiplication,
and basic trigonometric functions.

By using these operations in conjunction, one can create long, complex sounds.


## Interpreter

The interpreter takes a Sound a and an Int indicating the starting time for the
Sound a and returns a WAVEFunc a. A WAVEFunc a which is the type (Int -> a, Int),
where the first item in the tuple is a function from the time to an a, and the
second item in the tuple is the length of that Sound.

To evaluate a Sound Sample, as many times are used as the length of the sound in
seconds * number of frames per second samples are evaluated. A WAVEFunc Sample
is an (Int -> Sample, Int), so we can concatenate  all of the Samples from
applying the time to the function inside the tuple together to get a list of
Samples. The WAVE library then takes this list of samples and creates a .wav file.


## Examples

Example sound samples are near the bottom of Sound.hs. The Sounds produced from
these examples are in the examples directory.


## Notes

An obscene amount of conversion between ints, int32s, and doubles was required.
It was tedious to write (maxBound `div` 2) or (maxBound `div` 3) to write the most common volumes for sounds, so full, half, third, and quarter were created to simplify writing sounds.
