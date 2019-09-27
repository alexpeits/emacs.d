(setq my/avail-fonts
  '(
    "Source Code Pro-10"
    ;; "Iosevka Term-10.5"
    ;; "Iosevka Term-12"
    ;; "Hack-10.5"
    ;; "Ubuntu Mono-12"
    ;; "Menlo for Powerline-10.5"
    ;; "Menlo for Powerline-9"
    ;; "Ubuntu Mono-10.5"
    ;; "Terminus-12"
    ))

(setq my/current-font 0)

(setq my/avail-themes
  `(
    lucius-mono
    ))
(setq my/current-theme 0)

(setq my/hl-line-contrast 5)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-comment-face nil :slant 'normal)))

;; (add-to-list 'bibtex-completion-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))
;; (add-to-list 'org-ref-default-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))

(setq org-agenda-files `(,(concat org-directory "notes.org")))

;; (setq markdown-command "pandoc --highlight-style pygments -s")
