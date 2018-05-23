;; fonts

(setq my/avail-fonts
      '(
        "Menlo-12"
        "Ubuntu Mono-14"
        ))


;; themes

(setq my/avail-themes
      '(
        my/tomorrow-night-eighties
        my/monokai-light
        my/zenburn
        my/solarized-dark
        ))

(setq my/current-theme 0)

;; paper stuff

(add-to-list 'bibtex-completion-bibliography (expand-file-name "~/Documents/Bibtex/library.bib"))
(add-to-list 'org-ref-default-bibliography (expand-file-name "~/Documents/Bibtex/library.bib"))

;; org-mode
(setq org-agenda-files '("~/job/notes/notes.org"))
