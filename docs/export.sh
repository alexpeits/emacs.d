#!/usr/bin/env bash

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

emacs --visit $HERE/../configuration.org --batch -l $HERE/../init.el -f org-html-export-to-html --kill
