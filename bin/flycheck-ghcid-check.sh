#!/bin/bash

sleep 0.3

f="$1/.ghcid-output"
if grep -q "All good" $f; then
    exit 0
else
    cat $f
    exit 1
fi

