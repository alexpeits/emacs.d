(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq my/fonts
      '(
        ("small" . (:fixed ("Source Code Pro" . 13) :variable ("Source Serif Pro" . 16)))
        ("medium" . (:fixed ("Source Code Pro" . 15) :variable ("Source Serif Pro" . 18)))
        ("large" . (:fixed ("Source Code Pro" . 18) :variable ("Source Serif Pro" . 22)))
        ))

(setq my/font-variant "small")

(setq my/modus-vivendi-theme-alt-colors t
      my/modus-vivendi-theme-haskell-distinct-constructor nil
      my/modus-operandi-theme-haskell-distinct-constructor nil)

(setq my/avail-themes
      '(
        my/modus-vivendi-theme
        my/modus-operandi-theme
        ))
(setq my/current-theme 0)

(add-hook 'prog-mode-hook 'hl-line-mode)

;; auto mode stuff
(add-to-list 'auto-mode-alist '("ya?ml\\.sample\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.env\\..*\\.sample\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env.sample\\'" . conf-mode))

;; keys
;; (setq mac-command-modifier 'meta)
(global-set-key [?\A-\C-i] nil)

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

;; https://github.com/hlissner/doom-emacs/issues/2156#issuecomment-567207604
(when (display-graphic-p)
  (add-hook 'after-init-hook
            '(lambda ()
               (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
               )))
