;; elisp: ;; -*- eval: (outshine-mode) -*-
;;; fonts
(setq my/avail-fonts
      '(
        ("Source Code Pro" 10.5)
        ("Source Code Pro" 11)
        ;; ("Source Sans Pro" 13)
        ))
(setq my/current-font 0)

;;; themes
(setq my/modus-vivendi-theme-alt-colors t
      my/modus-vivendi-theme-haskell-distinct-constructor t
      my/modus-operandi-theme-haskell-distinct-constructor t)

(add-to-list 'custom-theme-load-path "~/Dropbox/emacs/themes")

(setq my/avail-themes
      '(
        my/modus-vivendi-theme
        my/zenburn-theme
        my/modus-operandi-theme
        ))
(setq my/current-theme 0)

;;; hooks
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'LaTeX-mode-hook 'hl-line-mode)

;;; variables
(setq magit-repository-directories
      '(
        ("~/projects" . 1)
        ("~/job/projects" . 1)
        ))

;;; projectile overrides
(dolist (override '(
                    ("/home/alex/projects/software-foundations" . "sf")
                    ))
  (let ((proj (car override))
        (name (cdr override)))
    (my/projectile-add-to-project-name-overrides proj name)))
