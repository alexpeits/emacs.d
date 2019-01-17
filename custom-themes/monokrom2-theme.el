(deftheme monokrom2)
(let ((class '((class color) (min-colors 89)))
       (fg1 "#d9d9d9")
       (fg2 "#c3c3c3")
       (fg3 "#b2b2b2")
       (fg4 "#a1a1a1")
       (bg1 "#121212")
       (bg2 "#141414")
       (bg3 "#292929")
       (bg4 "#3d3d3d")

       (blue1 "#3b84cc")
       (blue2 "#5693b5")

       (basic1 "#d4eee3")
       (basic2 "#c6d1cb")
       (basic3 "#97bfad")
       (basic4 "#90aea1")
       (basic5 "#656d69")

       (black1 "#111915")

       (green1 "#65e6a7")
       (green2 "#6fce9f")
       (green3 "#47ba99")

       (red "#cd5c60")
       (orange "#dbac66")

       (builtin "#d4d4d4")
       (keyword "#d4d4d4")
       (const   "#d4d4d4")
       (comment "#787878")
       (func    "#cacaca")
       (str     "#00af87")
       (doc     "#549385")
       (type    "#a8a8a8")
       (var     "#d4d4d4")
       (warning "#fa8072")
       (warning2 "#ff8800"))
   (custom-theme-set-faces
   'monokrom2
        `(default ((,class (:background ,black1 :foreground ,basic2))))
        `(font-lock-builtin-face ((,class (:foreground ,builtin))))
        `(font-lock-comment-face ((,class (:foreground ,basic5 :slant italic))))
	`(font-lock-negation-char-face ((,class (:foreground ,const))))
	`(font-lock-reference-face ((,class (:foreground ,const))))
	`(font-lock-constant-face ((,class (:foreground ,const))))
        `(font-lock-doc-face ((,class (:foreground ,green2))))
        `(font-lock-function-name-face ((,class (:foreground ,basic2 :weight bold))))
        `(font-lock-keyword-face ((,class (:bold ,class :foreground ,basic3))))
        `(font-lock-string-face ((,class (:foreground ,green1))))
        `(font-lock-type-face ((,class (:foreground ,basic4))))
        `(font-lock-variable-name-face ((,class (:foreground ,var))))
        `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
        `(font-lock-preprocessor-face ((,class (:foreground ,green1))))
        `(region ((,class (:background ,fg1 :foreground ,bg1))))
        `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
	`(hl-line ((,class (:background ,bg3))))
        `(fringe ((,class (:background "#1e1e1e"))))
        `(linum ((,class (:foreground "#5a5a5a"))))
	`(fringe ((,class (:background ,bg1 :foreground ,fg4))))
	`(cursor ((,class (:background ,fg3))))
        `(show-paren-match-face ((,class (:background ,warning))))
        `(persp-selected-face ((,class (:weight bold :foreground ,blue2))))
        `(mode-line
          ((t (:foreground "#c5d4cd" :background "#24342c" :box (:line-width 2 :color "#273931" :style nil)))))
        `(mode-line-inactive
          ((t (:foreground "#567668" :background "#1a2520" :box (:line-width 2 :color "#1f2c26" :style nil)))))
        ;; `(mode-line ((,class (:foreground ,fg2 :background ,bg4))))
        ;; `(mode-line-inactive ((,class (:foreground "#656565" :background "#202020"))))
        `(mode-line-buffer-id ((,class (:weight bold))))
	`(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
        `(mode-line-emphasis ((,class (:foreground ,fg1))))
	`(vertical-border ((,class (:foreground "#6a6a6a"))))
        `(minibuffer-prompt ((,class (:weight bold :foreground ,keyword))))
        `(default-italic ((,class (:italic t))))
	`(link ((,class (:foreground ,const :underline t))))
        `(secondary-selection ((,class (:background "#1a3856"))))
        `(markdown-code-face ((,class (:foreground "#79c6b3"))))
        `(markdown-pre-face ((,class (:foreground  "#79c6b3"))))
	`(org-code ((,class (:foreground ,fg2))))
	`(org-hide ((,class (:foreground ,fg4))))
        `(org-level-1 ((,class (:weight bold :foreground "#79c6b3"))))
        `(org-level-2 ((,class (:weight bold :foreground "#6b9fbc"))))
        `(org-level-3 ((,class (:weight bold :foreground "#ababab"))))
        `(org-level-4 ((,class (:weight bold :foreground "#79c6b3"))))
        `(outline-1 ((,class (:weight bold :foreground "#79c6b3"))))
        `(outline-2 ((,class (:weight bold :foreground "#6b9fbc"))))
        `(outline-3 ((,class (:weight bold :foreground "#ababab"))))
        `(outline-4 ((,class (:weight bold :foreground "#79c6b3"))))
        `(org-headline-done ((,class (:bold nil :foreground ,fg2))))
        `(org-date ((,class (:underline t :foreground ,var) )))
        `(org-footnote  ((,class (:underline t :foreground ,fg4))))
        `(org-link ((,class (:underline t :foreground ,type ))))
        `(org-special-keyword ((,class (:foreground ,func))))
        `(org-block ((,class (:foreground ,fg2 :background "#131c17"))))
        `(org-block-begin-line ((,class (:foreground "#909090" :background "#1a2620"))))
        `(org-block-end-line ((,class (:inherit org-block-begin-line))))
        `(org-quote ((,class (:inherit org-block :slant italic))))
        `(org-verse ((,class (:inherit org-block :slant italic))))
        `(org-todo ((,class (:foreground ,warning :weight bold))))
        `(org-table ((,class (:foreground ,fg1 :bold nil))))
        `(org-done ((,class (:weight bold :foreground ,fg4))))
        `(org-warning ((,class (:underline nil :foreground ,warning))))
        `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
        `(org-agenda-date ((,class (:foreground ,var))))
        `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
        `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
        `(org-agenda-done ((,class (:foreground ,fg4))))
	`(org-scheduled ((,class (:foreground ,type))))
        `(org-scheduled-today ((,class (:foreground ,func :weight bold))))
	`(org-ellipsis ((,class (:foreground ,builtin))))
	`(org-verbatim ((,class (:foreground ,fg4))))
        `(org-document-info-keyword ((,class (:foreground ,func))))
	`(font-latex-bold-face ((,class (:foreground ,type))))
	`(font-latex-italic-face ((,class (:foreground ,var :italic t))))
	`(font-latex-string-face ((,class (:foreground ,str))))
	`(font-latex-match-reference-keywords ((,class (:foreground ,const))))
	`(font-latex-match-variable-keywords ((,class (:foreground ,var))))
	`(ido-only-match ((,class (:foreground ,warning))))
	`(org-sexp-date ((,class (:foreground ,fg4))))
	`(ido-first-match ((,class (:foreground ,keyword :weight bold))))
	`(gnus-header-content ((,class (:foreground ,keyword))))
	`(gnus-header-from ((,class (:foreground ,var))))
	`(gnus-header-name ((,class (:foreground ,type))))
	`(gnus-header-subject ((,class (:foreground ,func :weight bold))))
	`(mu4e-view-url-number-face ((,class (:foreground ,type))))
	`(mu4e-cited-1-face ((,class (:foreground ,fg2))))
	`(mu4e-cited-7-face ((,class (:foreground ,fg3))))
	`(mu4e-header-marks-face ((,class (:foreground ,type))))
	`(ffap ((,class (:foreground ,fg4))))
	`(js2-private-function-call ((,class (:foreground ,const))))
	`(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
	`(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
	`(js2-external-variable ((,class (:foreground ,type  ))))
        `(js2-function-param ((,class (:foreground ,const))))
        `(js2-jsdoc-value ((,class (:foreground ,str))))
        `(js2-private-member ((,class (:foreground ,fg3))))
        `(js3-warning-face ((,class (:underline ,keyword))))
        `(js3-error-face ((,class (:underline ,warning))))
        `(js3-external-variable-face ((,class (:foreground ,var))))
        `(js3-function-param-face ((,class (:foreground ,fg2))))
        `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
        `(js3-instance-member-face ((,class (:foreground ,const))))
	`(warning ((,class (:foreground ,warning))))
	`(ac-completion-face ((,class (:underline t :foreground ,keyword))))
	`(info-quoted-name ((,class (:foreground ,builtin))))
	`(info-string ((,class (:foreground ,str))))
	`(icompletep-determined ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
        `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
        `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
	`(slime-repl-inputed-output-face ((,class (:foreground ,type))))
        `(trailing-whitespace ((,class :foreground nil :background ,warning)))
        `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
        `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
        `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
        `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
        `(diff-hl-insert ((,class (:background "#143514" :foreground "#4c934c"))))
        `(diff-hl-change ((,class (:background "#122544" :foreground "#466daf"))))
        `(diff-hl-delete ((,class (:background "#491111" :foreground "#bc4d4d"))))
        `(magit-item-highlight ((,class :background ,bg3)))
        `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
        `(magit-hunk-heading           ((,class (:background ,bg3))))
        `(magit-section-highlight      ((,class (:background ,bg3))))
        `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
        `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
        `(magit-diffstat-added   ((,class (:foreground ,type))))
        `(magit-diffstat-removed ((,class (:foreground ,var))))
        `(magit-process-ok ((,class (:foreground ,func :weight bold))))
        `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
        `(magit-branch ((,class (:foreground ,const :weight bold))))
        `(magit-log-author ((,class (:foreground ,fg3))))
        `(magit-hash ((,class (:foreground ,fg2))))
        `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
        `(isearch ((,class (:background ,warning :foreground ,bg3))))
        `(lazy-highlight ((,class (:foreground "#a0a8b0" :background "#525a6d"))))
        `(term ((,class (:foreground ,fg1 :background ,bg1))))
        `(term-color-monokrom2 ((,class (:foreground ,bg3 :background ,bg3))))
        `(term-color-blue ((,class (:foreground ,func :background ,func))))
        `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
        `(term-color-green ((,class (:foreground ,type :background ,bg3))))
        `(term-color-yellow ((,class (:foreground ,var :background ,var))))
        `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
        `(term-color-cyan ((,class (:foreground ,str :background ,str))))
        `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
        `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
        `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
        `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
        `(helm-selection ((,class (:background ,bg2 :underline nil))))
        `(helm-selection-line ((,class (:background ,bg2))))
        `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
        `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
        `(helm-separator ((,class (:foreground ,type :background ,bg1))))
        `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
        `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
        `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
        `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
        `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
        `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
        `(helm-ff-executable ((,class (:foreground ,var :background ,bg1 :weight normal))))
        `(helm-ff-invalid-symlink ((,class (:foreground ,warning2 :background ,bg1 :weight bold))))
        `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
        `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
        `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
        `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
        `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
        `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
        `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
        `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
        `(helm-bookmark-w3m ((,class (:foreground ,type))))
        `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
        `(company-preview ((,class (:background ,bg1 :foreground ,var))))
        `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
        `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
        `(company-scrollbar-bg ((,class (:background ,bg3))))
        `(company-scrollbar-fg ((,class (:foreground ,keyword))))
        `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :weight bold))))
        `(company-tooltop-annotation ((,class (:foreground ,const))))
        `(company-tooltip-common ((,class ( :foreground ,fg3))))
        `(company-tooltip-common-selection ((,class (:foreground ,str))))
        `(company-tooltip-mouse ((,class (:inherit highlight))))
        `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
        `(company-template-field ((,class (:inherit region))))
        `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
        `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
        `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
        `(web-mode-keyword-face ((,class (:foreground ,keyword))))
        `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
        `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
        `(web-mode-string-face ((,class (:foreground ,str))))
        `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
        `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
        `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
        `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
        `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
        `(ediff-odd-diff-A ((,class (:background ,bg3))))
        `(ediff-even-diff-A ((,class (:background ,bg3))))
        `(ediff-odd-diff-B ((,class (:background ,bg3))))
        `(ediff-even-diff-B ((,class (:background ,bg3))))
        `(ediff-odd-diff-C ((,class (:background ,bg3))))
        `(ediff-even-diff-C ((,class (:background ,bg3))))
	`(custom-face-tag ((t (:inherit nil))))))
