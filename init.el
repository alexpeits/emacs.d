;; https://blog.d46.us/advanced-emacs-startup/
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t
      tmp--file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1
                  inhibit-compacting-font-caches nil
                  file-name-handler-alist tmp--file-name-handler-alist)
            (makunbound 'tmp--file-name-handler-alist)
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'org)
(org-babel-load-file (concat user-emacs-directory "configuration.org"))
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
