#!/bin/sh -e

HIND='stack exec hindent --'
SHAS='stack exec stylish-haskell -- -i'
NPROC="$(getconf _NPROCESSORS_ONLN)"

find app src test -name '*.hs' -print0 | \
  xargs -0 -P "${NPROC}" -I % sh -c "${HIND} % && ${SHAS} %"