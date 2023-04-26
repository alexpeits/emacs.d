;; fonts
(setq my/fonts
      '(("small" . (:fixed ("Source Code Pro" . 10.5) :variable ("Source Serif Pro" . 12)))
        ("medium" . (:fixed ("Source Code Pro" . 13) :variable ("Source Serif Pro" . 16)))
        ("large" . (:fixed ("Source Code Pro" . 16) :variable ("Source Serif Pro" . 20)))
        ))

(setq my/font-variant "small")

;; themes
(setq my/avail-themes
      '(
        modus-vivendi
        modus-operandi
        ))
(setq my/current-theme 0)

;; hooks
(add-hook 'LaTeX-mode-hook 'hl-line-mode)

;; TODO
(setq og/publish-base-directory
  (expand-file-name "~/code/notes-serve/"))

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
