(setq my/avail-fonts
  '(
    "Iosevka Term-10.5"
    ;; "Iosevka Term-12"
    ;; "Hack-10.5"
    "Ubuntu Mono-12"
    ;; "Menlo for Powerline-10.5"
    ;; "Menlo for Powerline-9"
    ;; "Ubuntu Mono-10.5"
    ))

(setq my/big-font "Ubuntu Mono-13.2")
(setq my/current-font 1)

(setq my/avail-themes
  '(
    ron-swanson
    my/zenburn
    my/default
    ))
(setq my/current-theme 0)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-comment-face nil :slant 'normal)))

(add-to-list 'bibtex-completion-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))
(add-to-list 'org-ref-default-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))

(setq org-agenda-files `(,(concat org-directory "notes.org")))

(setq base16-highlight-mode-line 'contrast)

;; (setq markdown-command "pandoc --highlight-style pygments -s")
