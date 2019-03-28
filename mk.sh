#!/bin/sh

if ! which shyaml > /dev/null; then
    echo 'error: shyaml (https://github.com/0k/shyaml) is not found'
    exit 1
fi

PKG_EXE="pl-lab-exe"
GHC_FLAGS="$(cat package.yaml | shyaml get-values executables.${PKG_EXE}.ghc-options 2> /dev/null)"
if [[ $? != 0 ]]; then
    echo 'error: cannot read ghc-options from package.yaml'
    exit 1
fi

stack build --ghc-options "${GHC_FLAGS}"
