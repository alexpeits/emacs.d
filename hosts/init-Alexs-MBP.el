;; fonts

(setq my/avail-fonts
      '(
        "Menlo-12"
        "Menlo-14"
        "Menlo-22"
        ;; "Inconsolata-14"
        ;; "Ubuntu Mono-14"
        ))


;; themes

(setq
 sunburn-override-colors-alist
 '(("sunburn-green" . "#91b791") ("sunburn-bg" . "#433d44")))

(setq my/avail-themes
  `(
    ,(assoc 'gotham my/all-themes)
    ;; (solarized-light nil t)
    ,(assoc 'kaolin-mono-dark my/all-themes)
    ;; ,(assoc 'zenburn my/all-themes)
    ;; ,(assoc 'blackboard my/all-themes)
    ;; ,(assoc 'leslie-knope my/all-themes)
    ;; ,(assoc 'sunburn my/all-themes)
    ,(assoc 'wombat my/all-themes)
    ))
(setq my/current-theme 0)

;; paper stuff

(add-to-list 'bibtex-completion-bibliography (expand-file-name "~/Documents/Bibtex/library.bib"))
(add-to-list 'org-ref-default-bibliography (expand-file-name "~/Documents/Bibtex/library.bib"))

;; various major modes
(setq mu4e-mu-binary "/usr/local/bin/mu")

;; org-mode
(setq org-agenda-files '("~/job/notes/notes.org"))

(setq markdown-command "pandoc")
(setq markdown-gfm-use-electric-backquote nil)

;; Command key same as alt key
(setq mac-command-modifier 'meta)

(defun my/flycheck-on-save ()
  (interactive)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(defun my/flycheck-always ()
  (interactive)
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change new-line)))


(add-hook 'prog-mode-hook 'hl-line-mode)

