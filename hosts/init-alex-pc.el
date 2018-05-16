(setq my/avail-fonts
  '(
    "Iosevka Term-10.5"
    "Iosevka Term-11"
    "Iosevka Term-12"
    "Hack-10.5"
    "Ubuntu Mono-12"
    ;; "Menlo for Powerline-10.5"
    ;; "Menlo for Powerline-9"
    ;; "Ubuntu Mono-10.5"
    ))

(setq my/current-theme 3)
(setq my/current-font 0)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-comment-face nil :slant 'normal)))

(provide 'init-alex-pc)
