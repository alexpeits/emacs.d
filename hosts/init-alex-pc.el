(setq my/avail-fonts
  '(
    "Iosevka Term-10.5"
    ;; "Iosevka Term-12"
    "Hack-10.5"
    ;; "Ubuntu Mono-12"
    ;; "Menlo for Powerline-10.5"
    ;; "Menlo for Powerline-9"
    ;; "Ubuntu Mono-10.5"
    ;; "Terminus-12"
    ))

(setq my/big-font "Hack-22")
(setq my/current-font 1)

(setq
 sunburn-override-colors-alist
 '(("sunburn-green" . "#91b791") ("sunburn-bg" . "#433d44")))

(setq my/avail-themes
  `(
    ;; ,(assoc 'gotham my/all-themes)
    ;; ,(assoc 'zenburn my/all-themes)
    ,(assoc 'blackboard my/all-themes)
    ;; ,(assoc 'leslie-knope my/all-themes)
    (sunburn
     (progn
       (custom-theme-set-faces
        'sunburn
        '(mode-line-buffer-id ((t (:weight bold))))
        '(minibuffer-prompt ((t (:foreground "#f0dfaf"))))
        '(fringe ((t (:background "#4f4951"))))
        '(region ((t (:background "#6b5f6f"))))
        '(mode-line
          ((t (:box
               (:line-width -1 :color nil :style released-button)
               :foreground "#f1f1f1" :background "#3d344c"))))
        )
       (setq pdf-view-midnight-colors '("#dedded" . "#413b42"))
       (my/hl-line-set-bg) (my/org-set-src-faces)))
    ))
(setq my/current-theme 1)

(setq my/hl-line-contrast 5)

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
