(setq my/avail-fonts
  '(
    "Iosevka Term-10.5"
    ;; "Iosevka Term-12"
    ;; "Hack-10.5"
    "Ubuntu Mono-12"
    ;; "Menlo for Powerline-10.5"
    ;; "Menlo for Powerline-9"
    ;; "Ubuntu Mono-10.5"
    ;; "Terminus-12"
    ))

(setq my/big-font "Hack-22")
(setq my/current-font 1)

(setq my/avail-themes
  '(
    ;; my/abyss
    ;; my/nimbus
    ;; my/blackboard
    my/gotham
    (zenburn dark t)
    ;; ron-swanson
    (leslie-knope light)
    ))
(setq my/current-theme 0)

(setq my/hl-line-contrast 8)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'font-lock-comment-face nil :slant 'normal)))

(add-to-list 'bibtex-completion-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))
(add-to-list 'org-ref-default-bibliography (expand-file-name "~/Documents/Mendeley/library.bib"))

(setq org-agenda-files `(,(concat org-directory "notes.org")))

(setq
 solarized-use-variable-pitch nil
 solarized-brighter-use-variable-pitch nil
 solarized-brighter-black-use-variable-pitch nil)

(defun my/set-big-font ()
  (interactive)
  (set-frame-font my/big-font))

;; (setq markdown-command "pandoc --highlight-style pygments -s")
