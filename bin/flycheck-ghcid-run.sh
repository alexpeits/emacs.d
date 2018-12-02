#!/bin/bash

set -e

cleanup () {
    rm .ghcid-output
}

trap cleanup 0 1 2 3 6 14 15

ghcid --command="stack ghci --ghci-options=\"$@\"" -o .ghcid-output

