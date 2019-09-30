(if (< emacs-major-version 27)
    (package-initialize))

(require 'org)
(org-babel-load-file (concat user-emacs-directory "/configuration.org"))
