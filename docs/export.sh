#!/usr/bin/env bash

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

emacs --batch -l $HERE/../init.el -f my/org-export-conf --kill
