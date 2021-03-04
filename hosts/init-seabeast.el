;; fonts
(setq my/fonts
      '(("small" . (:fixed ("Source Code Pro" . 10.5) :variable ("Source Serif Pro" . 12)))
        ("medium" . (:fixed ("Source Code Pro" . 11) :variable ("Source Serif Pro" . 13.5)))
        ("large" . (:fixed ("Source Code Pro" . 15) :variable ("Source Serif Pro" . 17)))
        ))

(setq my/font-variant "small")

;; themes
(setq my/modus-vivendi-theme-alt-colors t
      my/modus-vivendi-theme-haskell-distinct-constructor t
      my/modus-operandi-theme-haskell-distinct-constructor t)

(setq my/avail-themes
      '(
        my/modus-vivendi-theme
        ;; my/zenburn-theme
        my/modus-operandi-theme
        ))
(setq my/current-theme 0)

;; hooks
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'LaTeX-mode-hook 'hl-line-mode)

;; TODO
(setq og/publish-base-directory
  (expand-file-name "~/code/gatsby-tutorial/"))

;; variables
(setq magit-repository-directories
      '(
        ("~/code" . 1)
        ))

;; projectile overrides
(dolist (override '(
                    ("~/code/software-foundations" . "sf")
                    ))
  (let ((proj (expand-file-name (car override)))
        (name (cdr override)))
    (my/projectile-add-to-project-name-overrides proj name)))
