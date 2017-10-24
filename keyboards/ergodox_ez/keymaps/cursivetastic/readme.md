# Cursivetastic

This is based on the ErgoDox EZ default configuration, but is a complete rewrite.

I use ClojureScript, via Lumo, to build the main portion of the keymap.

This makes it easier to define new layers in terms of just the keys that change on that
layer.

At the root level, the command `./build-ct.sh` will build the keymap source file,
then build just the cursivetastic keyboard.

This will create the file `ergodox_ez_cursivetastic.hex` in the qmk_firmware root directory, which
can then be flashed onto your keyboard.
