(setq
 my/avail-fonts
 '(
   ("Hack" 10.5)
   ("Source Code Pro" 12)
   ))

(setq my/current-font 0)

;; (setq functor-theme-less-colors t)

(add-to-list 'custom-theme-load-path "~/Dropbox/emacs/themes")

(setq my/avail-themes
      `(
        monad
        ,(assoc 'zenburn my/all-themes)
        monoid
        ))
(setq my/current-theme 0)

(setq my/hl-line-contrast 5)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-comment-face nil :slant 'normal)))

;; (add-to-list 'bibtex-completion-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))
;; (add-to-list 'org-ref-default-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))

(setq org-agenda-files `(,(concat org-directory "notes.org")))

(setq markdown-command "pandoc --highlight-style pygments -s")

(when (boundp 'intero-blacklist)
  (add-to-list 'intero-blacklist "~/projects/harg")
  (add-to-list 'intero-blacklist "~/sources/ghc"))

(add-hook 'prog-mode-hook 'hl-line-mode)
