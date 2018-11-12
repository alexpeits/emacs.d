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
    ,(assoc 'gotham my/all-themes)
    ;; ,(assoc 'zenburn my/all-themes)
    ;; ,(assoc 'blackboard my/all-themes)
    ;; ,(assoc 'leslie-knope my/all-themes)
    ,(assoc 'sunburn my/all-themes)
    (kaolin-mono-dark
     (progn
       (custom-theme-set-faces
        'kaolin-mono-dark
        '(mode-line
          ((t (:foreground "#c5d4cd" :background "#1f2f28" :box (:line-width 2 :color "#23352d" :style nil)))))
        '(mode-line-inactive
          ((t (:foreground "#567668" :background "#16211c" :box (:line-width 2 :color "#1b2822" :style nil)))))
        '(mode-line-buffer-id ((t (:weight bold))))
        '(font-lock-type-face ((t (:foreground "#749182"))))
        '(font-lock-comment-face ((t (:foreground "#4c6257"))))
        '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
        '(font-lock-doc-face ((t (:foreground "#6fce9f"))))
        '(persp-selected-face ((t (:foreground "#65e6a7" :weight bold))))
        '(show-paren-match ((t (:background "#327353" :weight bold))))
        )
       (my/diff-hl-set-fringe-faces)
       (my/org-set-src-faces)
       ;; (my/hl-line-set-bg)
       (setq pdf-view-midnight-colors '("#a2d8d5" . "#0a3749"))
       )
     )
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
