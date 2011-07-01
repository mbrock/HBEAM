HBEAM
=====

Hello!  This is a sketchy implementation of a BEAM emulator.  It is supposed
to read & evaluate Erlang bytecode files.

Right now it doesn't understand very much, but if you have the simplest
possible factorial function exported from `mymath.beam` --- there's such a
file in the repository --- you can run it like this:

    $ hbeam mymath factorial 1 '[EVInteger 6]'
     ... after lots of debug spam ...
    Return value: EVInteger 720

Wow!
