(deftheme cyrus "Simple flat dark theme.")

(require 'cus-edit) ; for buttons

(let (;; theme base colors
      (theme-background "#1a1a1a")
      (theme-foreground "#c5c8c6")
      (theme-accent     "#ff6000")
      (theme-bright     "#ffffff")
      (theme-faint      "#373b41")
      (theme-dark       "#111213")
      (theme-very-dark  "#090909")

      ;; emacs common colors
      (theme-red        "#cc6666")
      (theme-green      "#b5bd68")
      (theme-yellow     "#f0c674")
      (theme-blue       "#81a2be")
      (theme-pink       "#b294bb")

      ;; font lock palette
      (theme-palette-1  "#5f819d")
      (theme-palette-2  "#f0c674")
      (theme-palette-3  "#a54242")
      (theme-palette-4  "#666d65")
      (theme-palette-5  "#de935f")
      (theme-palette-6  "#85678f")
      (theme-palette-7  "#81a2be")
      (theme-palette-8  "#b5bd68"))

  (custom-theme-set-faces
   'cyrus

   ;; basic faces
   `(default        ((t (:foreground ,theme-foreground :background ,theme-background))))
   `(shadow         ((t (:foreground ,theme-faint))))
   `(link           ((t (:foreground ,theme-accent :underline (:color foreground-color :style line)))))
   `(link-visited   ((t (:inherit (link) :weight normal))))
   `(highlight      ((t (:background ,theme-dark))))
   `(match          ((t (:foreground ,theme-accent :weight bold))))
   `(isearch        ((t (:foreground ,theme-background :background ,theme-accent))))
   `(lazy-highlight ((t (:foreground ,theme-background :background ,theme-bright))))
   `(error          ((t (:foreground ,theme-red))))
   `(warning        ((t (:foreground ,theme-yellow))))
   `(success        ((t (:foreground ,theme-green))))

   ;; header/mode line
   `(mode-line           ((t (:foreground ,theme-foreground :background ,theme-faint))))
   `(mode-line-inactive  ((t (:inherit (mode-line) :foreground ,theme-palette-4 :background "#252525"))))
   `(mode-line-highlight ((t (:inverse-video t :box nil))))
   `(header-line         ((t (:inherit (mode-line) :foreground ,theme-foreground))))

   ;; font lock
   `(font-lock-function-name-face ((t (:foreground ,theme-palette-1))))
   `(font-lock-variable-name-face ((t (:foreground ,theme-palette-2))))
   `(font-lock-keyword-face       ((t (:foreground ,theme-palette-3))))
   `(font-lock-comment-face       ((t (:foreground ,theme-palette-4))))
   `(font-lock-type-face          ((t (:foreground ,theme-palette-5))))
   `(font-lock-constant-face      ((t (:foreground ,theme-palette-6))))
   `(font-lock-builtin-face       ((t (:foreground ,theme-palette-7))))
   `(font-lock-string-face        ((t (:foreground ,theme-palette-8))))
   `(font-lock-negation-char-face ((t (:inherit (default)))))

   ;; highlightings
   `(hi-black-b  ((t (:inherit (bold)))))
   `(hi-black-hb ((t (:inherit (bold)))))
   `(hi-blue     ((t (:foreground ,theme-background :background ,theme-blue))))
   `(hi-blue-b   ((t (:inherit (hi-blue bold) :inverse-video t))))
   `(hi-green    ((t (:foreground ,theme-background :background ,theme-green))))
   `(hi-green-b  ((t (:inherit (hi-green bold) :inverse-video t))))
   `(hi-pink     ((t (:foreground ,theme-background :background ,theme-pink))))
   `(hi-red-b    ((t (:inherit (bold) :foreground ,theme-red))))
   `(hi-yellow   ((t (:foreground ,theme-background :background ,theme-yellow))))

   ;; widgets
   `(custom-button         ((t (:box (:line-width 2 :color nil :style released-button) :foreground ,theme-foreground :background ,theme-faint))))
   `(custom-button-pressed ((t (:inherit (custom-button-mouse) :box (:line-width 2 :color nil :style released-button) :foreground ,theme-accent))))
   `(custom-button-mouse   ((t (:inherit (highlight))))) ; for coherence with widget-button
   `(widget-field          ((t (:foreground ,theme-foreground :background ,theme-faint))))
   `(widget-button         ((t (:inherit (custom-button)))))
   `(widget-button-pressed ((t (:inherit (custom-button-pressed)))))

   ;; others
   `(vertical-border              ((t (:foreground ,theme-dark))))
   `(cursor                       ((t (:background ,theme-bright))))
   `(fringe                       ((t (:foreground ,theme-dark))))
   `(minibuffer-prompt            ((t (:foreground ,theme-accent :weight bold))))
   `(region                       ((t (:background ,theme-faint))))
   `(secondary-selection          ((t (:foreground ,theme-accent :background ,theme-dark))))
   `(isearch-fail                 ((t (:inherit (error)))))
   `(completions-common-part      ((t (:inherit (shadow)))))
   `(completions-first-difference ((t (:foreground ,theme-accent))))
   `(pulse-highlight-start-face   ((t (:background ,theme-accent))))

   ;; fix: compilation
   `(compilation-mode-line-exit ((t (:inherit (success)))))
   `(compilation-mode-line-run  ((t (:inherit (warning)))))
   `(compilation-mode-line-fail ((t (:inherit (error)))))

   ;; fix: show-paren
   `(show-paren-match    ((t (:inherit (bold) :foreground ,theme-accent))))
   `(show-paren-mismatch ((t (:inherit (error) :inverse-video t))))

   ;; fix: eshell
   `(eshell-prompt ((t (:inherit (minibuffer-prompt)))))

   ;; fix: term
   `(term-color-black ((t (:foreground ,theme-faint :background ,theme-faint))))

   ;; fix: woman
   `(woman-bold   ((t (:inherit (bold) :foreground ,theme-bright))))
   `(woman-italic ((t (:inherit (italic) :foreground ,theme-green))))

   ;; fix: js2-mode
   `(js2-object-property ((t (:inherit (font-lock-builtin-face)))))

   ;; fix: erc
   `(erc-prompt-face    ((t (:inherit (minibuffer-prompt)))))
   `(erc-timestamp-face ((t (:inherit (shadow)))))
   `(erc-notice-face    ((t (:inherit (shadow bold)))))

   ;; fix: markdown-mode
   `(markdown-code-face ((t (:background ,theme-very-dark))))
   `(markdown-header-face-1 ((t (:inherit (outline-1 bold) :height 1.4 :background ,theme-very-dark))))
   `(markdown-header-face-2 ((t (:inherit (outline-2 bold) :height 1.4 :background ,theme-very-dark))))
   `(markdown-header-face-3 ((t (:inherit (outline-3 bold) :height 1.4 :background ,theme-very-dark))))
   `(markdown-header-face-4 ((t (:inherit (outline-4 bold) :height 1.4 :background ,theme-very-dark))))
   `(markdown-header-face-5 ((t (:inherit (outline-5 bold) :height 1.4 :background ,theme-very-dark))))
   `(markdown-header-face-6 ((t (:inherit (outline-6 bold) :height 1.4 :background ,theme-very-dark))))

   ;; fix: org-mode
   `(org-block                 ((t (:background ,theme-very-dark))))
   `(org-code                  ((t (:inherit (font-lock-string-face) :background ,theme-very-dark))))
   `(org-verbatim              ((t (:inherit (font-lock-string-face) :background ,theme-very-dark))))
   `(org-document-info-keyword ((t (:inherit (org-meta-line)))))
   `(org-block-begin-line      ((t (:inherit (org-meta-line) :height 0.7))))
   `(org-block-end-line        ((t (:inherit (org-meta-line) :height 0.7))))
   `(org-ellipsis              ((t (:inherit (link)))))
   `(org-tag                   ((t (:inherit (shadow)))))
   `(org-level-1               ((t (:inherit (outline-1 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-2               ((t (:inherit (outline-2 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-3               ((t (:inherit (outline-3 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-4               ((t (:inherit (outline-4 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-5               ((t (:inherit (outline-5 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-6               ((t (:inherit (outline-6 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-7               ((t (:inherit (outline-7 bold) :height 1.4 :background ,theme-very-dark))))
   `(org-level-8               ((t (:inherit (outline-8 bold) :height 1.4 :background ,theme-very-dark))))

   ;; fix: magit
   `(git-commit-overlong-summary ((t (:inherit (font-lock-warning-face) :inverse-video t))))))

(provide-theme 'cyrus)
