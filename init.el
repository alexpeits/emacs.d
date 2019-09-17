
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(if (< emacs-major-version 27)
    (package-initialize))

(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org-mode/contrib/lisp")

(require 'org)
(org-babel-load-file "~/.emacs.d/configuration.org")
;; (put 'narrow-to-region 'disabled nil)
