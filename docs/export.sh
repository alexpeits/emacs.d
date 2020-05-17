#!/usr/bin/env bash

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

#emacs --visit $HERE/../configuration.org --batch -l $HERE/../init.el -f org-html-export-to-html --kill
emacs --visit $HERE/../configuration.org --batch -l $HERE/../init.el --eval '(org-export-to-file (quote html) "docs/index.html")' --kill
