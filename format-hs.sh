#!/bin/sh

HIND='stack exec hindent --'
SHAS='stack exec stylish-haskell -- -i'

find app src test -name '*.hs' -exec ${HIND} {} \; -exec ${SHAS} {} \;
