(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(require 'org)
(org-babel-load-file (concat user-emacs-directory "configuration.org"))
(put 'narrow-to-region 'disabled nil)
