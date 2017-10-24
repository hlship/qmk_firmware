#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

pushd keyboards/ergodox_ez/keymaps/cursivetastic/

make

popd

make ergodox_ez-cursivetastic
