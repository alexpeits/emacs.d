;; Base16 PaperColor (https://github.com/chriskempson/base16)
;; Scheme: Nguyen Nguyen (http://github.com/NLKNguyen)

;;; base16-papercolor-dark-theme.el

;;; Code:

(deftheme base16-papercolor-dark)

(let ((base00 "#262626")
      (base01 "#262626")
      (base01a "#323232")
      (base01b "#353535")
      (base02 "#4d4d4c")
      (base02b "#5d5d5d")
      (base03 "#808080")
      (base04 "#949494")
      (base05 "#d0d0d0")
      (base06 "#f3f3f3")
      (base07 "#f3f3f3")
      (base08 "#af87af")
      (base09 "#ff5faf")
      (base09b "#ff6c6b")
      (base0A "#00afaf")
      (base0B "#dfaf5f")
      (base0C "#5fafdf")
      (base0D "#df875f")
      (base0E "#afdf00")
      (base0F "#00af5f"))

  (unless (display-graphic-p)
    (setq base00 "black"
          base01 "color-18"
          base02 "color-19"
          base03 "brightblack"
          base04 "color-20"
          base05 "white"
          base06 "color-21"
          base07 "brightwhite"
          base08 "red"
          base09 "color-16"
          base0A "yellow"
          base0B "green"
          base0C "cyan"
          base0D "blue"
          base0E "magenta"
          base0F "color-17"))

  (custom-theme-set-faces
   'base16-papercolor-dark

   ;; Built-in stuff (Emacs 23)
   `(border ((t (:background ,base03))))
   `(border-glyph ((t (nil))))
   `(cursor ((t (:background ,base08))))
   `(default ((t (:background ,base00 :foreground ,base05))))
   `(fringe ((t (:background ,base01b))))
   `(gui-element ((t (:background ,base03 :foreground ,base06))))
   `(highlight ((t (:background ,base01))))
   `(link ((t (:foreground ,base0D))))
   `(link-visited ((t (:foreground ,base0E))))
   `(minibuffer-prompt ((t (:foreground ,base0D))))
   `(mode-line ((t (:background ,base02b :foreground ,base05 :box (:line-width 2 :color ,base02b)))))
   `(mode-line-buffer-id ((t (:foreground ,base0E :background nil :weight bold))))
   `(mode-line-emphasis ((t (:foreground ,base06 :slant italic))))
   `(mode-line-highlight ((t (:foreground ,base0E :box nil :weight bold))))
   `(mode-line-inactive ((t (:background ,base01b :foreground ,base03 :box (:line-width 2 :color ,base01b)))))
   `(region ((t (:background ,base02))))
   `(secondary-selection ((t (:background ,base02))))
   `(error ((t (:foreground ,base08 :weight bold))))
   `(warning ((t (:foreground ,base09 :weight bold))))
   `(success ((t (:foreground ,base0B :weight bold))))

   `(hl-line ((t (:background ,base01a))))

   `(header-line ((t (:inherit mode-line :foreground ,base0E :background nil))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,base0C))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,base02))))
   `(font-lock-comment-face ((t (:foreground ,base03))))
   `(font-lock-constant-face ((t (:foreground ,base09))))
   `(font-lock-doc-face ((t (:foreground ,base04))))
   `(font-lock-doc-string-face ((t (:foreground ,base03))))
   `(font-lock-function-name-face ((t (:foreground ,base0D))))
   `(font-lock-keyword-face ((t (:foreground ,base0E))))
   `(font-lock-negation-char-face ((t (:foreground ,base0B))))
   `(font-lock-preprocessor-face ((t (:foreground ,base0D))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,base0A))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,base0E))))
   `(font-lock-string-face ((t (:foreground ,base0B))))
   `(font-lock-type-face ((t (:foreground ,base0A))))
   `(font-lock-variable-name-face ((t (:foreground ,base0C))))
   `(font-lock-warning-face ((t (:foreground ,base08))))

   ;; persp
   `(persp-selected-face ((t (:foreground ,base0C :weight bold))))

   ;; linum-mode
   `(linum ((t (:background ,base01 :foreground ,base03))))

   ;; Search
   `(match ((t (:foreground ,base0D :background ,base01 :inverse-video t))))
   `(isearch ((t (:foreground ,base0A :background ,base01 :inverse-video t))))
   `(isearch-lazy-highlight-face ((t (:foreground ,base0C :background ,base01 :inverse-video t))))
   `(isearch-fail ((t (:background ,base01 :inherit font-lock-warning-face :inverse-video t))))
   `(evil-search-highlight-persist-highlight-face ((t (:background ,base01 :inherit font-lock-warning-face :inverse-video t))))

   ;; Popups
   `(popup-face ((t (:foreground ,base05 :background ,base02))))
   `(popup-isearch-match ((t (:foreground ,base00 :background ,base0B))))
   `(popup-scroll-bar-background-face ((t (:background ,base03))))
   `(popup-scroll-bar-foreground-face ((t (:background ,base05))))
   `(popup-summary-face ((t (:foreground ,base04))))
   `(popup-tip-face ((t (:foreground ,base00 :background ,base0A))))
   `(popup-menu-mouse-face ((t (:foreground ,base00 :background ,base0D))))
   `(popup-menu-selection-face ((t (:foreground ,base00 :background ,base0C))))

   ;; Flymake
   `(flymake-warnline ((t (:underline ,base09 :background ,base01))))
   `(flymake-errline ((t (:underline ,base08 :background ,base01))))

   ;; Flycheck
   `(flycheck-warning ((t (:underline (:color ,base0B :style wave)))))
   `(flycheck-fringe-warning ((t (:foreground ,base0B))))
   `(flycheck-error ((t (:underline (:color ,base09b :style wave)))))
   `(flycheck-fringe-error ((t (:foreground ,base09b))))
   `(flycheck-info ((t (:underline (:color ,base0C :style wave)))))
   `(flycheck-fringe-info ((t (:foreground ,base0C))))

   ;; Clojure errors
   `(clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((t (:background nil :foreground nil :underline ,base0B))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((t (:foreground ,base0A))))
   `(clojure-parens ((t (:foreground ,base06))))
   `(clojure-braces ((t (:foreground ,base0B))))
   `(clojure-brackets ((t (:foreground ,base0A))))
   `(clojure-double-quote ((t (:foreground ,base0C :background nil))))
   `(clojure-special ((t (:foreground ,base0D))))
   `(clojure-java-call ((t (:foreground ,base0E))))

   ;; MMM-mode
   `(mmm-code-submode-face ((t (:background ,base03))))
   `(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((t (:background ,base03))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,base0E))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,base0D))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,base0C))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,base0B))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,base0A))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,base09))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,base08))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,base03))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,base05))))

   ;; IDO
   `(ido-subdir ((t (:foreground ,base04))))
   `(ido-first-match ((t (:foreground ,base09 :weight bold))))
   `(ido-only-match ((t (:foreground ,base08 :weight bold))))
   `(ido-indicator ((t (:foreground ,base08 :background ,base01))))
   `(ido-virtual ((t (:foreground ,base04))))

   ;; which-function
   `(which-func ((t (:foreground ,base0D :background nil :weight bold))))

   `(trailing-whitespace ((t (:background ,base0C :foreground ,base0A))))
   `(whitespace-empty ((t (:foreground ,base08 :background ,base0A))))
   `(whitespace-hspace ((t (:background ,base04 :foreground ,base04))))
   `(whitespace-indentation ((t (:background ,base0A :foreground ,base08))))
   `(whitespace-line ((t (:background ,base01 :foreground ,base0F))))
   `(whitespace-newline ((t (:foreground ,base04))))
   `(whitespace-space ((t (:background ,base01 :foreground ,base04))))
   `(whitespace-space-after-tab ((t (:background ,base0A :foreground ,base08))))
   `(whitespace-space-before-tab ((t (:background ,base09 :foreground ,base08))))
   `(whitespace-tab ((t (:background ,base04 :foreground ,base04))))
   `(whitespace-trailing ((t (:background ,base08 :foreground ,base0A))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match ((t (:background ,base0D :foreground ,base03))))
   `(show-paren-mismatch ((t (:background ,base09 :foreground ,base03))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((t (:foreground ,base04 :background nil))))

   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((t (:weight bold))))
   `(slime-repl-input-face ((t (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((t (:underline nil :weight bold :foreground ,base0E))))
   `(slime-repl-result-face ((t (:foreground ,base0B))))
   `(slime-repl-output-face ((t (:foreground ,base0D :background ,base01))))

   `(csv-separator-face ((t (:foreground ,base09))))

   `(diff-added ((t (:foreground ,base0B))))
   `(diff-changed ((t (:foreground ,base0A))))
   `(diff-removed ((t (:foreground ,base08))))
   `(diff-header ((t (:background ,base01))))
   `(diff-file-header ((t (:background ,base02))))
   `(diff-hunk-header ((t (:background ,base01 :foreground ,base0E))))

   `(diff-hl-change ((t (:foreground ,base0A))))
   `(diff-hl-delete ((t (:foreground ,base08))))
   `(diff-hl-insert ((t (:foreground ,base0B))))

   `(ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((t (:foreground ,base04 :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((t (:foreground ,base04 :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((t (:foreground ,base0B :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((t (:foreground ,base06))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,base0B :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,base08))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,base0A))))

   ;; auctex
   `(font-latex-bold-face ((t (:foreground ,base0B))))
   `(font-latex-doctex-documentation-face ((t (:background ,base03))))
   `(font-latex-italic-face ((t (:foreground ,base0B))))
   `(font-latex-math-face ((t (:foreground ,base09))))
   `(font-latex-sectioning-0-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-1-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-2-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-3-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-4-face ((t (:foreground ,base0A))))
   `(font-latex-sectioning-5-face ((t (:foreground ,base0A))))
   `(font-latex-sedate-face ((t (:foreground ,base0C))))
   `(font-latex-string-face ((t (:foreground ,base0A))))
   `(font-latex-verbatim-face ((t (:foreground ,base09))))
   `(font-latex-warning-face ((t (:foreground ,base08))))

   ;; dired+
   `(diredp-compressed-file-suffix ((t (:foreground ,base0D))))
   `(diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
   `(diredp-dir-priv ((t (:foreground ,base0C :background nil))))
   `(diredp-exec-priv ((t (:foreground ,base0D :background nil))))
   `(diredp-executable-tag ((t (:foreground ,base08 :background nil))))
   `(diredp-file-name ((t (:foreground ,base0A))))
   `(diredp-file-suffix ((t (:foreground ,base0B))))
   `(diredp-flag-mark-line ((t (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((t (:foreground ,base04))))
   `(diredp-link-priv ((t (:background nil :foreground ,base0E))))
   `(diredp-mode-line-flagged ((t (:foreground ,base08))))
   `(diredp-mode-line-marked ((t (:foreground ,base0B))))
   `(diredp-no-priv ((t (:background nil))))
   `(diredp-number ((t (:foreground ,base0A))))
   `(diredp-other-priv ((t (:background nil :foreground ,base0E))))
   `(diredp-rare-priv ((t (:foreground ,base08 :background nil))))
   `(diredp-read-priv ((t (:foreground ,base0B :background nil))))
   `(diredp-symlink ((t (:foreground ,base0E))))
   `(diredp-write-priv ((t (:foreground ,base0A :background nil))))

   ;; term and ansi-term
   `(term-color-black ((t (:foreground ,base02 :background ,base00))))
   `(term-color-white ((t (:foreground ,base05 :background ,base07))))
   `(term-color-red ((t (:foreground ,base08 :background ,base08))))
   `(term-color-yellow ((t (:foreground ,base0A :background ,base0A))))
   `(term-color-green ((t (:foreground ,base0B :background ,base0B))))
   `(term-color-cyan ((t (:foreground ,base0C :background ,base0C))))
   `(term-color-blue ((t (:foreground ,base0D :background ,base0D))))
   `(term-color-magenta ((t (:foreground ,base0E :background ,base0E))))

   `(link ((t (:foreground nil :underline t))))
   `(widget-button ((t (:underline t))))
   `(widget-field ((t (:background ,base03 :box (:line-width 1 :color ,base06)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((t (:foreground ,base0A))))
   `(compilation-line-number ((t (:foreground ,base0A))))
   `(compilation-message-face ((t (:foreground ,base0D))))
   `(compilation-mode-line-exit ((t (:foreground ,base0B))))
   `(compilation-mode-line-fail ((t (:foreground ,base08))))
   `(compilation-mode-line-run ((t (:foreground ,base0D))))

   ;; Grep
   `(grep-context-face ((t (:foreground ,base04))))
   `(grep-error-face ((t (:foreground ,base08 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,base0D))))
   `(grep-match-face ((t (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

   ;; Cscope
   `(cscope-file-face ((t (:foreground ,base0B))))
   `(cscope-function-face ((t (:foreground ,base0D))))
   `(cscope-line-number-face ((t (:foreground ,base0A))))
   `(cscope-mouse-face ((t (:background ,base01 :foreground ,base04))))
   `(cscope-separator-face ((t (:foreground ,base08 :overline t :underline t :weight bold))))

   ;; mark-multiple
   `(mm/master-face ((t (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((t (:inherit region :foreground nil :background nil))))

   ;; org-mode
   `(org-agenda-structure ((t (:foreground ,base0E))))
   `(org-agenda-date ((t (:foreground ,base0D :underline nil))))
   `(org-agenda-done ((t (:foreground ,base0B))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,base04))))
   `(org-block ((t (:foreground ,base05 :background "#2e2e2e"))))
   `(org-block-begin-line ((t (:background "#3a3a3a"))))
   `(org-block-end-line ((t (:background "#3a3a3a"))))
   `(org-code ((t (:foreground ,base0A))))
   `(org-column ((t (:background ,base01))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground ,base0E :underline t))))
   `(org-document-info ((t (:foreground ,base0C))))
   `(org-document-info-keyword ((t (:foreground ,base0B))))
   `(org-document-title ((t (:weight bold :foreground ,base09 :height 1.44))))
   `(org-done ((t (:foreground ,base0B))))
   `(org-ellipsis ((t (:foreground ,base04))))
   `(org-footnote ((t (:foreground ,base0C))))
   `(org-formula ((t (:foreground ,base08))))
   `(org-hide ((t (:foreground ,base03))))
   `(org-link ((t (:foreground ,base0D))))
   `(org-scheduled ((t (:foreground ,base0B))))
   `(org-scheduled-previously ((t (:foreground ,base09))))
   `(org-scheduled-today ((t (:foreground ,base0B))))
   `(org-special-keyword ((t (:foreground ,base09))))
   `(org-table ((t (:foreground ,base0E))))
   `(org-todo ((t (:foreground ,base08))))
   `(org-upcoming-deadline ((t (:foreground ,base09))))
   `(org-warning ((t (:weight bold :foreground ,base08))))

   `(markdown-url-face ((t (:inherit link))))
   `(markdown-link-face ((t (:foreground ,base0D :underline t))))

   `(hl-sexp-face ((t (:background ,base03))))
   `(highlight-80+ ((t (:background ,base03))))

   ;; Python-specific overrides
   `(py-builtins-face ((t (:foreground ,base09 :weight normal))))

   ;; js2-mode
   `(js2-warning-face ((t (:underline ,base09))))
   `(js2-error-face ((t (:foreground nil :underline ,base08))))
   `(js2-external-variable-face ((t (:foreground ,base0E))))
   `(js2-function-param-face ((t (:foreground ,base0D))))
   `(js2-instance-member-face ((t (:foreground ,base0D))))
   `(js2-private-function-call-face ((t (:foreground ,base08))))

   ;; js3-mode
   `(js3-warning-face ((t (:underline ,base09))))
   `(js3-error-face ((t (:foreground nil :underline ,base08))))
   `(js3-external-variable-face ((t (:foreground ,base0E))))
   `(js3-function-param-face ((t (:foreground ,base0D))))
   `(js3-jsdoc-tag-face ((t (:foreground ,base09))))
   `(js3-jsdoc-type-face ((t (:foreground ,base0C))))
   `(js3-jsdoc-value-face ((t (:foreground ,base0A))))
   `(js3-jsdoc-html-tag-name-face ((t (:foreground ,base0D))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,base0B))))
   `(js3-instance-member-face ((t (:foreground ,base0D))))
   `(js3-private-function-call-face ((t (:foreground ,base08))))

   ;; nxml
   `(nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((t (:underline ,base08))))

   ;; RHTML
   `(erb-delim-face ((t (:background ,base03))))
   `(erb-exec-face ((t (:background ,base03 :weight bold))))
   `(erb-exec-delim-face ((t (:background ,base03))))
   `(erb-out-face ((t (:background ,base03 :weight bold))))
   `(erb-out-delim-face ((t (:background ,base03))))
   `(erb-comment-face ((t (:background ,base03 :weight bold :slant italic))))
   `(erb-comment-delim-face ((t (:background ,base03))))

   ;; Message-mode
   `(message-header-other ((t (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((t (:inherit message-header-other :weight bold :foreground ,base0A))))
   `(message-header-to ((t (:inherit message-header-other :weight bold :foreground ,base09))))
   `(message-header-cc ((t (:inherit message-header-to :foreground nil))))
   `(message-header-name ((t (:foreground ,base0D :background nil))))
   `(message-header-newsgroups ((t (:foreground ,base0C :background nil :slant normal))))
   `(message-separator ((t (:foreground ,base0E))))

   ;; Jabber
   `(jabber-chat-prompt-local ((t (:foreground ,base0A))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,base09))))
   `(jabber-chat-prompt-system ((t (:foreground ,base0A :weight bold))))
   `(jabber-chat-text-local ((t (:foreground ,base0A))))
   `(jabber-chat-text-foreign ((t (:foreground ,base09))))
   `(jabber-chat-text-error ((t (:foreground ,base08))))

   `(jabber-roster-user-online ((t (:foreground ,base0B))))
   `(jabber-roster-user-xa ((t :foreground ,base04)))
   `(jabber-roster-user-dnd ((t :foreground ,base0A)))
   `(jabber-roster-user-away ((t (:foreground ,base09))))
   `(jabber-roster-user-chatty ((t (:foreground ,base0E))))
   `(jabber-roster-user-error ((t (:foreground ,base08))))
   `(jabber-roster-user-offline ((t (:foreground ,base04))))

   `(jabber-rare-time-face ((t (:foreground ,base04))))
   `(jabber-activity-face ((t (:foreground ,base0E))))
   `(jabber-activity-personal-face ((t (:foreground ,base0C))))

   ;; Gnus
   `(gnus-cite-1 ((t (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((t (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((t (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((t (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((t (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((t (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((t (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((t (:inherit outline-8 :foreground nil))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-header-from ((t (:inherit message-header-other-face :weight bold :foreground ,base09))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-button ((t (:inherit link :foreground nil))))
   `(gnus-signature ((t (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((t (:foreground ,base0D :weight normal))))
   `(gnus-summary-normal-read ((t (:foreground ,base06 :weight normal))))
   `(gnus-summary-normal-ancient ((t (:foreground ,base0C :weight normal))))
   `(gnus-summary-normal-ticked ((t (:foreground ,base09 :weight normal))))
   `(gnus-summary-low-unread ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-low-read ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-low-ancient ((t (:foreground ,base04 :weight normal))))
   `(gnus-summary-high-unread ((t (:foreground ,base0A :weight normal))))
   `(gnus-summary-high-read ((t (:foreground ,base0B :weight normal))))
   `(gnus-summary-high-ancient ((t (:foreground ,base0B :weight normal))))
   `(gnus-summary-high-ticked ((t (:foreground ,base09 :weight normal))))
   `(gnus-summary-cancelled ((t (:foreground ,base08 :background nil :weight normal))))

   `(gnus-group-mail-low ((t (:foreground ,base04))))
   `(gnus-group-mail-low-empty ((t (:foreground ,base04))))
   `(gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :foreground ,base04))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :foreground ,base04))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :foreground ,base04))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-mail-4 :foreground ,base04))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-mail-5 :foreground ,base04))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-mail-6 :foreground ,base04))))
   `(gnus-group-news-1 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((t (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((t (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :foreground ,base04))))
   `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :foreground ,base04))))
   `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :foreground ,base04))))
   `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-4 :foreground ,base04))))
   `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-5 :foreground ,base04))))
   `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-6 :foreground ,base04))))

   `(erc-direct-msg-face ((t (:foreground ,base09))))
   `(erc-error-face ((t (:foreground ,base08))))
   `(erc-header-face ((t (:foreground ,base06 :background ,base04))))
   `(erc-input-face ((t (:foreground ,base0B))))
   `(erc-keyword-face ((t (:foreground ,base0A))))
   `(erc-current-nick-face ((t (:foreground ,base0B))))
   `(erc-my-nick-face ((t (:foreground ,base0B))))
   `(erc-nick-default-face ((t (:weight normal :foreground ,base0E))))
   `(erc-nick-msg-face ((t (:weight normal :foreground ,base0A))))
   `(erc-notice-face ((t (:foreground ,base04))))
   `(erc-pal-face ((t (:foreground ,base09))))
   `(erc-prompt-face ((t (:foreground ,base0D))))
   `(erc-timestamp-face ((t (:foreground ,base0C))))

   ;; helm
   `(helm-M-x-key ((t (:foreground ,base0C))))
   `(helm-action ((t (:foreground ,base05))))
   `(helm-buffer-directory ((t (:foreground ,base04 :background nil :weight bold))))
   `(helm-buffer-file ((t (:foreground ,base0C))))
   `(helm-buffer-not-saved ((t (:foreground ,base08))))
   `(helm-buffer-process ((t (:foreground ,base03))))
   `(helm-buffer-saved-out ((t (:foreground ,base0F))))
   `(helm-buffer-size ((t (:foreground ,base09))))
   `(helm-candidate-number ((t (:foreground ,base00 :background ,base09))))
   `(helm-ff-directory ((t (:foreground ,base04 :background nil :weight bold))))
   `(helm-ff-executable ((t (:foreground ,base0B))))
   `(helm-ff-file ((t (:foreground ,base0C))))
   `(helm-ff-invalid-symlink ((t (:foreground ,base00 :background ,base08))))
   `(helm-ff-prefix ((t (:foreground nil :background nil))))
   `(helm-ff-symlink ((t (:foreground ,base00 :background ,base0C))))
   `(helm-grep-cmd-line ((t (:foreground ,base0B))))
   `(helm-grep-file ((t (:foreground ,base0C))))
   `(helm-grep-finish ((t (:foreground ,base00 :background ,base09))))
   `(helm-grep-lineno ((t (:foreground ,base03))))
   `(helm-grep-match ((t (:foreground ,base0A))))
   `(helm-grep-running ((t (:foreground ,base09))))
   `(helm-header ((t (:foreground ,base0A :background ,base00 :underline nil))))
   `(helm-match ((t (:foreground ,base0A))))
   `(helm-moccur-buffer ((t (:foreground ,base0C))))
   `(helm-selection ((t (:foreground nil :background ,base02 :underline nil))))
   `(helm-selection-line ((t (:foreground nil :background ,base02))))
   `(helm-separator ((t (:foreground ,base02))))
   `(helm-source-header ((t (:foreground ,base05 :background ,base01 :weight bold))))
   `(helm-visible-mark ((t (:foreground ,base00 :background ,base0B))))

   `(custom-variable-tag ((t (:foreground ,base0D))))
   `(custom-group-tag ((t (:foreground ,base0D))))
   `(custom-state ((t (:foreground ,base0B)))))


  (custom-theme-set-variables
   'base16-papercolor-dark

   `(ansi-color-names-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05])
   `(ansi-term-color-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [unspecified ,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05])))

(provide-theme 'base16-papercolor-dark)

;;; base16-papercolor-dark-theme.el ends here

