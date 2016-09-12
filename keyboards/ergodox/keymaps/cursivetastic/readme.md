# Cursivetastic

This is based on the ErgoDox EZ default configuration, but is a complete rewrite.

I use Clojure (via Boot) to build the main portion of the keymap:

    boot write-keymap -f keymap-gen.h

This makes it easier to define new layers in terms of just the keys that change on that
layer.