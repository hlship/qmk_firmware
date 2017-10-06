# Cursivetastic

This is based on the ErgoDox EZ default configuration, but is a complete rewrite.

I use ClojureScript, via Lumo, to build the main portion of the keymap:

    cd keyboards/ergodox/keymaps/cursivetastic/

    make

This makes it easier to define new layers in terms of just the keys that change on that
layer.

Currently, this isn't integrated into the qmk_firmare build system, so after invoking `make` in this
directory, you must invoke `make ergodox_ez-cursivetastic` in the root directory.

This will create the file `ergodox_ez_cursivetastic.hex` in the qmk_firmware root directory, which
can then be flashed onto your keyboard.


