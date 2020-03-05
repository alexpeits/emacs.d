;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6
      tmp--file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1
                  file-name-handler-alist tmp--file-name-handler-alist)
            (makunbound 'tmp--file-name-handler-alist)
            ))

(require 'org)
(org-babel-load-file (concat user-emacs-directory "configuration.org"))
(put 'narrow-to-region 'disabled nil)
