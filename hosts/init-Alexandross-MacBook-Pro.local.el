(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq my/fonts
      '(
        ("vsmall" . (:fixed ("Source Code Pro" . 12) :variable ("Source Serif Pro" . 14)))
        ("small" . (:fixed ("Source Code Pro" . 13) :variable ("Source Serif Pro" . 16)))
        ("medium" . (:fixed ("Source Code Pro" . 15) :variable ("Source Serif Pro" . 18)))
        ("large" . (:fixed ("Source Code Pro" . 18) :variable ("Source Serif Pro" . 22)))
        ))

(setq my/font-variant "small")

(setq my/avail-themes
      '(
        modus-vivendi
        my/nord-theme
        ;; modus-operandi
        ))
(setq my/current-theme 0)

;; auto mode stuff
(add-to-list 'auto-mode-alist '("ya?ml\\.sample\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.env\\..*\\.sample\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env.sample\\'" . conf-mode))

(setq og/publish-base-directory
  (expand-file-name "~/code/notes-serve/"))

(setq magit-repository-directories
      '(
        ("~/code" . 1)
        ))

;; projectile overrides
(dolist (override '(
                    ("~/code/software-foundations" . "sf")
                    ("~/code/intervals-icu-plan" . "icu")
                    ))
  (let ((proj (expand-file-name (car override)))
        (name (cdr override)))
    (my/projectile-add-to-project-name-overrides proj name)))

;; https://github.com/hlissner/doom-emacs/issues/2156#issuecomment-567207604
;; (when (display-graphic-p)
;;   (add-hook 'after-init-hook
;;             '(lambda ()
;;                (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
;;                )))
