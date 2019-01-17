(deftheme jadedragon 
  "jadedragon - Created by Jasonm23 - 8:50pm(+1000) 2012-07-02")

  (custom-theme-set-faces
   'jadedragon

   ;; basic theming.
   '(default                          ((t (:foreground "#b3cecb" :background "#101515" ))))
   '(region                           ((t (:background "#1f3f3d"))))
   '(cursor                           ((t (:background "#00a3a2"))))
   '(fringe                           ((t (:background "#202525"))))
   '(linum                            ((t (:background "#002027" :foreground "#4b7684"))))
   '(minibuffer-prompt                ((t (:foreground "#14948b" :weight bold ))))
   '(minibuffer-message               ((t (:foreground "#ffffff" ))))
   '(mode-line                        ((t (:foreground "#c7e2d8" :background "#073a37" :box (:line-width 1 :color "#0a4946")))))
   '(mode-line-inactive               ((t (:inherit mode-line :foreground "#3e7772" :background "#002320"))))

   '(font-lock-builtin-face           ((t (:foreground "#ffffff" ))))
   '(font-lock-keyword-face           ((t (:foreground "#26a2bf" ))))
   '(font-lock-type-face              ((t (:foreground "#01a37d" ))))
   '(font-lock-constant-face          ((t (:foreground "#00B286" ))))
   ;; '(font-lock-variable-name-face     ((t (:foreground "#63ff2b" ))))
   '(font-lock-variable-name-face     ((t (:foreground "#69e03e" ))))
   '(font-lock-function-name-face     ((t (:foreground "#36ffba" ))))
   '(font-lock-string-face            ((t (:foreground "#039dad" ))))
   '(font-lock-comment-face           ((t (:foreground "#607784" :slant italic))))
   '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   '(font-lock-doc-face               ((t (:foreground "#3a8bb2" ))))
   '(font-lock-doc-string-face        ((t (:inherit font-lock-doc-face))))
   '(font-lock-preprocessor-face      ((t (:foreground "#d16969" ))))
   '(font-lock-warning-face           ((t (:foreground "#FF0000" ))))

   ;; '(haskell-constructor-face ((t (:foregound "#479e89"))))
   ;; '(haskell-constructor-face         ((t (:inherit default))))

   '(org-block ((t (:foreground "#c8d0c8" :background "#141414"))))
   '(org-block-begin-line ((t (:background "#1d1d1d"))))
   '(org-block-end-line ((t (:background "#1d1d1d"))))

   `(diff-hl-insert ((t (:background "#143514" :foreground "#4c934c"))))
   `(diff-hl-change ((t (:background "#122544" :foreground "#466daf"))))
   `(diff-hl-delete ((t (:background "#491111" :foreground "#bc4d4d"))))

   '(show-paren-match ((t (:background "#3f6284" :weight bold))))

   ;; easy defaults...
   '(tooltip ((default nil) (nil nil)))
   '(next-error ((t          (:inherit (region)))))
   '(query-replace ((t       (:inherit (isearch)))))
   '(button ((t              (:inherit (link)))))
   '(fixed-pitch ((t         (:family "Monospace"))))
   '(variable-pitch ((t      (:family "Sans Serif"))))
   '(escape-glyph ((t        (:foreground "#FF6600"))))
   '(mode-line-emphasis ((t  (:weight bold))))
   '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))

   '(persp-selected-face ((t (:inherit font-lock-string-face :weight bold))))

   '(hl-line ((t (:background "#222828"))))

   '(vertical-border ((t (:foreground "#789991"))))

   '(highlight
     ((((class color) (min-colors 88) (background light)) (:background "#003453"))
      (((class color) (min-colors 88) (background dark))  (:background "#003450"))
      (((class color) (min-colors 16) (background light)) (:background "#003450"))
      (((class color) (min-colors 16) (background dark))  (:background "#004560"))
      (((class color) (min-colors 8))                     (:foreground "#000000" :background "#00FF00")) (t (:inverse-video t))))

   '(shadow
     ((((class color grayscale) (min-colors 88) (background light)) (:foreground "#999999"))
      (((class color grayscale) (min-colors 88) (background dark))  (:foreground "#999999"))
      (((class color) (min-colors 8) (background light))            (:foreground "#00ff00"))
      (((class color) (min-colors 8) (background dark))             (:foreground "#ffff00"))))

   '(trailing-whitespace
     ((((class color) (background light)) (:background "#ff0000"))
      (((class color) (background dark))  (:background "#ff0000")) (t (:inverse-video t))))

   '(link
     ((((class color) (min-colors 88) (background light)) (:underline t :foreground "#00b7f0"))
      (((class color) (background light))                 (:underline t :foreground "#0044FF"))
      (((class color) (min-colors 88) (background dark))  (:underline t :foreground "#0099aa"))
      (((class color) (background dark))                  (:underline t :foreground "#0099aa")) (t (:inherit (underline)))))

   '(link-visited
     ((default                            (:inherit (link)))
      (((class color) (background light)) (:inherit (link)))
      (((class color) (background dark))  (:inherit (link)))))

   '(header-line
     ((default                                      (:inherit (mode-line))) (((type tty)) (:underline t :inverse-video nil))
      (((class color grayscale) (background light)) (:box nil :foreground "#222222" :background "#bbbbbb"))
      (((class color grayscale) (background dark))  (:box nil :foreground "#bbbbbb" :background "#222222"))
      (((class mono) (background light))            (:underline t :box nil :inverse-video nil :foreground "#000000" :background "#ffffff"))
      (((class mono) (background dark))             (:underline t :box nil :inverse-video nil :foreground "#ffffff" :background "#000000"))))

   '(isearch-fail
     ((((class color) (min-colors 88) (background light)) (:background "#ffaaaa"))
      (((class color) (min-colors 88) (background dark))  (:background "#880000"))
      (((class color) (min-colors 16))                    (:background "#FF0000"))
      (((class color) (min-colors 8))                     (:background "#FF0000"))
      (((class color grayscale))                          (:foreground "#888888")) (t (:inverse-video t))))

   '(isearch                             ((t (:inverse-video nil :foreground "black" :background "#db7e4c"))))
   '(lazy-highlight                      ((t (:foreground "#a0a8b0" :background "#3d464f"))))

   '(match
     ((((class color) (min-colors 88) (background light)) (:background "#3388cc"))
      (((class color) (min-colors 88) (background dark)) (:background "#3388cc"))
      (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00"))
      (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF"))
      (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))

   )

(provide-theme 'jadedragon)
