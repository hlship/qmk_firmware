# Cursivetastic

This is based on the ErgoDox EZ default configuration, but is a complete rewrite.

I use Clojure (via Lumo) to build the main portion of the keymap:

    cd keyboards/ergodox/keymaps/cursivetastic/

    lumo src/cursivetastic/keymapping.cljc

This makes it easier to define new layers in terms of just the keys that change on that
layer.

Currently, this isn't integrated into the Makefile system, so after invoking `lumo`, you must invoke `make`.

This will create the file `ergodox_ez_cursivetastic.hex` in the qmk_firmware root directory.


