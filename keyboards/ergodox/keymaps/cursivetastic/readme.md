# Cursivetastic

This is based on the ErgoDox EZ default configuration, but is a complete rewrite.

I use Clojure (via Boot) to build the main portion of the keymap:

    boot write-keymap -f target/keymap.h

This makes it easier to define new layers in terms of just the keys that change on that
layer.

Currently, this isn't integrated into the Makefile system, so after invoking `boot`, you must invoke `make`.

This will create the file `ergodox_ez_cursivetastic.hex` in the qmk_firmware root directory.


