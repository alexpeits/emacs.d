;; fonts
(setq my/fonts
      '(("small" . (:fixed ("Source Code Pro" . 10.5) :variable ("Source Serif 4" . 12)))
        ("medium" . (:fixed ("Source Code Pro" . 13) :variable ("Source Serif 4" . 16)))
        ("large" . (:fixed ("Source Code Pro" . 16) :variable ("Source Serif 4" . 20)))
        ))

(setq my/font-variant "small")

;; themes
(setq my/modus-vivendi-theme-alt-colors t
      my/modus-vivendi-theme-haskell-distinct-constructor nil
      my/modus-operandi-theme-haskell-distinct-constructor nil)

(setq my/avail-themes
      '(
        my/modus-vivendi-theme
        ;; my/zenburn-theme
        my/modus-operandi-theme
        ))
(setq my/current-theme 0)
