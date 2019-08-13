;;; yazz-theme.el --- A warm color theme for Emacs 24+.

;; Copyright (C) 2012-2014 Roman Parykin, Bozhidar Batsov

;; Author: Roman Parykin <donderom@ymail.com>
;; URL: https://github.com/donderom/yazz-theme
;; Package-Version: 20170411.1411
;; Version: 1.0

;; Based on zenburn-theme.el
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 1.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;   Drop the theme in a folder that is on `custom-theme-load-path'
;; and enjoy
;;
;; or
;;
;; (load-file "path/to/yazz-theme.el")
;; (load-theme 'yazz t)
;;
;; Don't forget that the theme requires Emacs 24+.
;;
;; The more information on the project page at https://github.com/donderom/yazz-theme
;;
;;; Credits
;;
;; Jani Nurminen (zenburn theme for vim)
;; Bozhidar Batsov (zenburn theme port for Emacs),
;; Christian Brassat <crshd@mail.com> (yazz theme initial colors inspiration)
;;
;;; Code:
(deftheme yazz "The Yazz color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Yazz palette
      ;; colors with +x are lighter, colors with -x are darker
      (yazz-fg       (color-lighten-name "#c6a57b" 10))
      (yazz-fg-1     (color-lighten-name "#96754b" 10))
      (yazz-fg+1     (color-lighten-name "#505050" 10))

      (yazz-bg-1     (color-lighten-name "#101010" 3))
      (yazz-bg-05    (color-lighten-name "#151515" 3))
      (yazz-bg       (color-lighten-name "#151515" 1))
      (yazz-bg+1     (color-lighten-name "#202020" 3))
      (yazz-bg+1b    (color-lighten-name "#363636" 3))
      (yazz-bg+2     (color-lighten-name "#505050" 10))
      (yazz-bg+3     (color-lighten-name "#606060" 10))

      (yazz-red+1    (color-lighten-name "#8d4a4a" 10))
      (yazz-red      (color-lighten-name "#953331" 10))
      (yazz-red-1    (color-lighten-name "#953331" 10))
      (yazz-red-2    (color-lighten-name "#953331" 10))
      (yazz-red-3    (color-lighten-name "#953331" 10))
      (yazz-red-4    (color-lighten-name "#953331" 10))

      (yazz-orange   (color-lighten-name "#ba5b34" 10))

      (yazz-yellow+1 (color-lighten-name "#96a62d" 8))
      (yazz-yellow   (color-lighten-name "#909737" 8))
      (yazz-yellow-1 (color-lighten-name "#909737" 8))
      (yazz-yellow-2 (color-lighten-name "#909737" 8))

      (yazz-green-1  (color-lighten-name "#546a29" 10))
      (yazz-green    (color-lighten-name "#546a29" 10))
      (yazz-green+1  (color-lighten-name "#7e9960" 10))
      (yazz-green+2  (color-lighten-name "#7e9960" 10))
      (yazz-green+3  (color-lighten-name "#7e9960" 10))
      (yazz-green+4  (color-lighten-name "#7e9960" 10))

      (yazz-cyan     (color-lighten-name "#34676f" 10))

      (yazz-blue+1   (color-lighten-name "#5c737c" 10))
      (yazz-blue     (color-lighten-name "#385e6b" 10))
      (yazz-blue-1   (color-lighten-name "#385e6b" 10))
      (yazz-blue-2   (color-lighten-name "#385e6b" 10))
      (yazz-blue-3   (color-lighten-name "#385e6b" 10))
      (yazz-blue-4   (color-lighten-name "#385e6b" 10))
      (yazz-blue-5   (color-lighten-name "#385e6b" 10))

      (yazz-magenta  (color-lighten-name "#7f355e" 10)))
  (custom-theme-set-faces
   'yazz
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,yazz-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,yazz-yellow-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,yazz-fg :background ,yazz-bg))))
   `(cursor ((,class (:foreground ,yazz-fg :background ,yazz-fg))))
   `(escape-glyph-face ((,class (:foreground ,yazz-red))))
   `(fringe ((,class (:foreground ,yazz-fg+1 :background ,yazz-bg+1))))
   `(header-line ((,class (:foreground ,yazz-yellow
                                       :background ,yazz-bg-1
                                       :box (:line-width -1 :color ,yazz-bg :style released-button)))))
   `(highlight ((,class (:background ,yazz-bg+1))))
   `(persp-selected-face ((,class (:foreground ,yazz-blue+1 :weight bold))))

   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,yazz-yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,yazz-green))))
   `(compilation-error-face ((,class (:foreground ,yazz-red :weight bold :underline t))))
   `(compilation-error ((,class (:foreground ,yazz-red :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,yazz-fg))))
   `(compilation-info-face ((,class (:foreground ,yazz-blue))))
   `(compilation-info ((,class (:foreground ,yazz-blue))))
   `(compilation-leave-directory-face ((,class (:foreground ,yazz-green))))
   `(compilation-line-face ((,class (:foreground ,yazz-yellow))))
   `(compilation-line-number ((,class (:foreground ,yazz-yellow))))
   `(compilation-message-face ((,class (:foreground ,yazz-blue))))
   `(compilation-warning-face ((,class (:foreground ,yazz-orange :weight bold :underline t))))
   `(compilation-warning ((,class (:foreground ,yazz-orange :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,yazz-fg))))
   `(grep-error-face ((,class (:foreground ,yazz-red-1 :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,yazz-blue))))
   `(grep-match-face ((,class (:foreground ,yazz-orange :weight bold))))
   `(match ((,class (:background ,yazz-bg-1 :foreground ,yazz-orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,yazz-yellow :background ,yazz-bg+2))))
   `(isearch-fail ((,class (:foreground ,yazz-fg :background ,yazz-red-4))))
   `(lazy-highlight ((,class (:foreground ,yazz-yellow :background ,yazz-bg+1))))

   `(menu ((,class (:foreground ,yazz-fg :background ,yazz-bg))))
   `(minibuffer-prompt ((,class (:foreground ,yazz-blue))))
   `(mode-line
     ((,class (:foreground ,yazz-fg
                           :background ,yazz-bg+1b
                           :box (:line-width 2 :color ,yazz-bg+1b)))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:inverse-video t))))
   `(mode-line-inactive
     ((,class (:inherit mode-line :foreground ,yazz-fg+1
                        :background ,yazz-bg+1
                        :box (:line-width 2 :color ,yazz-bg+1)))))
   `(mode-line-folder-face ((,class (:foreground ,yazz-bg+2))))
   `(mode-line-modified-face ((,class (:foreground ,yazz-red))))
   `(mode-line-ro-modified-face ((,class (:foreground ,yazz-blue))))
   `(mode-line-buffer-name ((,class (:foreground ,yazz-yellow))))
   `(mode-line-mode-name ((,class (:foreground ,yazz-blue))))
   `(mode-line-mode-string ((,class (:foreground ,yazz-bg+3))))
   `(mode-line-vc-mode ((,class (:foreground ,yazz-magenta))))
   `(mode-line-minor-mode-face ((,class (:foreground ,yazz-bg+2 :height 96))))

   `(region ((,class (:background ,yazz-fg :foreground ,yazz-blue))))
   `(secondary-selection ((,class (:background ,yazz-bg+2))))
   `(trailing-whitespace ((,class (:background ,yazz-red))))
   `(vertical-border ((,class (:foreground ,yazz-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,yazz-blue))))
   `(font-lock-comment-face ((,class (:foreground ,yazz-bg+3))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,class (:foreground ,yazz-magenta))))
   `(font-lock-doc-face ((,class (:foreground ,yazz-green+1))))
   `(font-lock-doc-string-face ((,class (:foreground ,yazz-blue+1))))
   `(font-lock-function-name-face ((,class (:foreground ,yazz-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,yazz-yellow))))
   `(font-lock-negation-char-face ((,class (:foreground ,yazz-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,yazz-blue))))
   `(font-lock-string-face ((,class (:foreground ,yazz-red))))
   `(font-lock-type-face ((,class (:foreground ,yazz-yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,yazz-orange))))
   `(font-lock-warning-face ((,class (:foreground ,yazz-yellow-1 :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(haskell-constructor-face ((,class (:inherit default))))
   `(purescript-constructor-face ((,class (:inherit haskell-constructor-face))))

   ;;; external

   ;; ace-jump
   `(ace-jump-face-background ((,class (:foreground ,yazz-bg+2))))
   `(ace-jump-face-foreground ((,class (:foreground ,yazz-yellow+1 :underline nil))))

   ;; anzu
   `(anzu-mode-line ((,class (:foreground ,yazz-orange :weigth bold))))
   `(anzu-replace-to ((,class :foreground ,yazz-orange)))

   ;; full-ack
   `(ack-separator ((,class (:foreground ,yazz-fg))))
   `(ack-file ((,class (:foreground ,yazz-blue))))
   `(ack-line ((,class (:foreground ,yazz-yellow))))
   `(ack-match ((,class (:foreground ,yazz-orange :background ,yazz-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,yazz-yellow :weight bold ))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))

   ;; popup
   `(popup-summary-face ((,class (:background ,yazz-bg+3 :foreground ,yazz-fg+1))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,yazz-bg+3))))
   `(popup-scroll-bar-background-face ((,class (:background ,yazz-fg+1))))
   `(popup-menu-mouse-face ((,class (:background ,yazz-yellow+1 :foreground ,yazz-bg))))
   `(popup-tip-face ((,class (:background ,yazz-bg+3 :foreground ,yazz-bg))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,yazz-bg+3 :foreground "black"))))
   `(ac-selection-face ((,class (:background ,yazz-blue-4 :foreground ,yazz-fg))))
   `(popup-tip-face ((,class (:background ,yazz-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,yazz-blue-5))))
   `(popup-scroll-bar-background-face ((,class (:background ,yazz-bg-1))))
   `(popup-isearch-match ((,class (:background ,yazz-bg :foreground ,yazz-fg))))

   ;; cheatsheet
   `(cheatsheet-group-face ((,class (:foreground ,yazz-blue))))
   `(cheatsheet-key-face ((,class (:foreground ,yazz-yellow))))

   ;; company
   `(company-tooltip ((,class (:background ,yazz-bg+3 :foreground ,yazz-bg))))
   `(company-tooltip-common ((,class (:foreground ,yazz-bg :underline t))))
   `(company-tooltip-common-selection ((,class (:background ,yazz-blue-4 :foreground ,yazz-fg :underline t))))
   `(company-tooltip-selection ((,class (:background ,yazz-blue-4 :foreground ,yazz-fg))))
   `(company-preview ((,class (:background ,yazz-blue :foreground ,yazz-fg))))
   `(company-preview-common ((,class (:inherit company-preview))))
   `(company-preview-search ((,class (:inherit company-preview :background ,yazz-blue+1))))
   `(company-scrollbar-fg ((,class (:background ,yazz-bg+1))))
   `(company-scrollbar-bg ((,class (:background ,yazz-bg+2))))
   `(company-tooltip-annotation ((,class (:background nil :foreground ,yazz-yellow))))

   ;; diff
   `(diff-added ((,class (:foreground ,yazz-green+4))))
   `(diff-changed ((,class (:foreground ,yazz-yellow))))
   `(diff-removed ((,class (:foreground ,yazz-red))))
   `(diff-header ((,class (:background ,yazz-bg+1))))
   `(diff-file-header
     ((,class (:background ,yazz-bg+2 :foreground ,yazz-fg :bold t))))

   ;; ein
   `(ein:cell-input-area ((,class (:background ,yazz-bg+1))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,yazz-green+4 :background ,yazz-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,yazz-red :background ,yazz-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,yazz-yellow))))
   `(eshell-ls-archive ((,class (:foreground ,yazz-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,yazz-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,yazz-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,yazz-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,yazz-yellow))))
   `(eshell-ls-symlink ((,class (:foreground ,yazz-cyan :weight bold))))

   ;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yazz-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yazz-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-blue)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yazz-blue :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-yellow-1) :inherit unspecified))
      (t (:foreground ,yazz-yellow-1 :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-red-1) :inherit unspecified))
      (t (:foreground ,yazz-red-1 :weight bold :underline t))))

   ;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yazz-red :weight bold :underline t))))
   `(flycheck-fringe-error ((,class (:foreground ,yazz-red :background ,yazz-bg))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yazz-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yazz-orange :weight bold :underline t))))
   `(flycheck-fringe-warning ((,class (:foreground ,yazz-orange :background ,yazz-bg))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,yazz-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,yazz-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,yazz-yellow))))
   `(erc-keyword-face ((,class (:foreground ,yazz-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,yazz-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,yazz-red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,yazz-green))))
   `(erc-pal-face ((,class (:foreground ,yazz-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,yazz-orange :background ,yazz-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,yazz-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,yazz-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,yazz-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,yazz-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,yazz-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,yazz-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,yazz-blue))))
   `(gnus-summary-low-read ((t (:foreground ,yazz-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,yazz-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,yazz-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,yazz-blue))))
   `(gnus-summary-normal-read ((,class (:foreground ,yazz-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,yazz-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,yazz-fg))))
   `(gnus-summary-selected ((,class (:foreground ,yazz-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,yazz-blue))))
   `(gnus-cite-10 ((,class (:foreground ,yazz-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,yazz-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,yazz-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,yazz-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,yazz-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,yazz-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,yazz-green))))
   `(gnus-cite-7 ((,class (:foreground ,yazz-red))))
   `(gnus-cite-8 ((,class (:foreground ,yazz-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,yazz-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,yazz-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,yazz-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,yazz-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,yazz-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,yazz-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,yazz-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,yazz-bg+2))))
   `(gnus-signature ((,class (:foreground ,yazz-yellow))))
   `(gnus-x ((,class (:background ,yazz-fg :foreground ,yazz-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,yazz-green
                           :background ,yazz-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,yazz-yellow
                           :background ,yazz-bg-1
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,yazz-bg+1))))
   `(helm-selection-line ((,class (:background ,yazz-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,yazz-bg :background ,yazz-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,yazz-green+4 :background ,yazz-bg-1))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,yazz-bg-1))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,yazz-yellow :weight normal))))
   `(ido-only-match ((,class (:foreground ,yazz-orange :weight normal))))
   `(ido-subdir ((,class (:foreground ,yazz-red))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,yazz-orange))))
   `(js2-error-face ((,class (:foreground ,yazz-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,yazz-green-1))))
   `(js2-jsdoc-type-face ((,class (:foreground ,yazz-green+2))))
   `(js2-jsdoc-value-face ((,class (:foreground ,yazz-green+3))))
   `(js2-function-param-face ((,class (:foreground, yazz-green+3))))
   `(js2-external-variable-face ((,class (:foreground ,yazz-orange))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,yazz-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,yazz-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,yazz-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,yazz-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,yazz-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,yazz-red+1))))
   `(jabber-activity-face((,class (:foreground ,yazz-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,yazz-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,yazz-bg+2 :background ,yazz-bg-1))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,yazz-yellow :weight bold :box nil :background ,yazz-bg))))
   `(magit-branch ((,class (:foreground ,yazz-orange :weight bold :box nil :background ,yazz-bg))))
   `(magit-item-highlight ((t (:background ,yazz-bg+1))))

   ;; markdown
   `(markdown-header-face ((,class (:inherit variable-pitch))))
   `(markdown-header-face-1 ((,class (:height 1.8 :inherit markdown-header-face))))
   `(markdown-header-face-2 ((,class (:height 1.4 :inherit markdown-header-face))))
   `(markdown-header-face-3 ((,class (:height 1.2 :inherit markdown-header-face))))
   `(markdown-header-face-4 ((,class (:height 1.0 :inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,yazz-green+1))))
   `(message-header-other ((,class (:foreground ,yazz-green))))
   `(message-header-to ((,class (:foreground ,yazz-yellow :weight bold))))
   `(message-header-from ((,class (:foreground ,yazz-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,yazz-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,yazz-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,yazz-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,yazz-green))))
   `(message-mml ((,class (:foreground ,yazz-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,yazz-orange))))
   `(mew-face-header-from ((,class (:foreground ,yazz-yellow))))
   `(mew-face-header-date ((,class (:foreground ,yazz-green))))
   `(mew-face-header-to ((,class (:foreground ,yazz-red))))
   `(mew-face-header-key ((,class (:foreground ,yazz-green))))
   `(mew-face-header-private ((,class (:foreground ,yazz-green))))
   `(mew-face-header-important ((,class (:foreground ,yazz-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,yazz-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,yazz-red))))
   `(mew-face-header-xmew ((,class (:foreground ,yazz-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,yazz-red))))
   `(mew-face-body-url ((,class (:foreground ,yazz-orange))))
   `(mew-face-body-comment ((,class (:foreground ,yazz-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,yazz-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,yazz-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,yazz-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,yazz-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,yazz-red))))
   `(mew-face-mark-review ((,class (:foreground ,yazz-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,yazz-green))))
   `(mew-face-mark-delete ((,class (:foreground ,yazz-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,yazz-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,yazz-green))))
   `(mew-face-mark-unread ((,class (:foreground ,yazz-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,yazz-green))))
   `(mew-face-eof-part ((,class (:foreground ,yazz-yellow))))

   ;; minimap
   `(minimap-font-face ((default (:height 30 :family "Anka/Coder")) (nil nil)))
   `(minimap-semantic-function-face ((((background dark)) (:inherit (font-lock-function-name-face minimap-font-face) :background "gray10"))))
   `(minimap-semantic-type-face ((,class (:inherit (font-lock-type-face minimap-font-face) :background "gray10"))))
   `(minimap-semantic-variable-face ((,class (:inherit (font-lock-variable-name-face minimap-font-face) :background "gray10"))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,yazz-cyan :background ,yazz-bg :weight bold :underline t))))
   `(paren-face-mismatch ((,class (:foreground ,yazz-bg :background ,yazz-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,yazz-bg :background ,yazz-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,yazz-yellow))))
   `(nav-face-button-num ((,class (:foreground ,yazz-cyan))))
   `(nav-face-dir ((,class (:foreground ,yazz-green))))
   `(nav-face-hdir ((,class (:foreground ,yazz-red))))
   `(nav-face-file ((,class (:foreground ,yazz-fg))))
   `(nav-face-hfile ((,class (:foreground ,yazz-red-4))))

   ;; org-mode
   `(org-agenda-date-today
     ((,class (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:foreground ,yazz-fg :weight bold))))
   `(org-checkbox ((,class (:background ,yazz-bg+2 :foreground "white"
                                        :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,yazz-blue :underline t))))
   `(org-deadline-announce ((,class (:foreground ,yazz-red-1))))
   `(org-done ((,class (:bold t :weight bold :foreground ,yazz-green+3))))
   `(org-formula ((,class (:foreground ,yazz-yellow-2))))
   `(org-headline-done ((,class (:foreground ,yazz-green+3))))
   `(org-hide ((,class (:foreground ,yazz-bg-1))))
   `(org-level-1 ((,class (:foreground ,yazz-orange))))
   `(org-level-2 ((,class (:foreground ,yazz-green+1))))
   `(org-level-3 ((,class (:foreground ,yazz-blue-1))))
   `(org-level-4 ((,class (:foreground ,yazz-yellow-2))))
   `(org-level-5 ((,class (:foreground ,yazz-cyan))))
   `(org-level-6 ((,class (:foreground ,yazz-green-1))))
   `(org-level-7 ((,class (:foreground ,yazz-red-4))))
   `(org-level-8 ((,class (:foreground ,yazz-blue-4))))
   `(org-link ((,class (:foreground ,yazz-yellow-2 :underline t))))
   `(org-scheduled ((,class (:foreground ,yazz-green+4))))
   `(org-scheduled-previously ((,class (:foreground ,yazz-red-4))))
   `(org-scheduled-today ((,class (:foreground ,yazz-blue+1))))
   `(org-special-keyword ((,class (:foreground ,yazz-yellow-1))))
   `(org-table ((,class (:foreground ,yazz-green+2))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-time-grid ((,class (:foreground ,yazz-orange))))
   `(org-todo ((,class (:bold t :foreground ,yazz-red :weight bold))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   `(org-warning ((,class (:bold t :foreground ,yazz-red :weight bold))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,yazz-cyan))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yazz-yellow))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yazz-blue+1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,yazz-red+1))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,yazz-orange))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yazz-blue-1))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,yazz-green+4))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yazz-red-3))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,yazz-yellow-2))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yazz-green+2))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,yazz-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,yazz-red-4))))

   ;; rcirc
   `(rcirc-my-nick ((,class (:foreground ,yazz-blue))))
   `(rcirc-other-nick ((,class (:foreground ,yazz-orange))))
   `(rcirc-bright-nick ((,class (:foreground ,yazz-blue+1))))
   `(rcirc-dim-nick ((,class (:foreground ,yazz-blue-2))))
   `(rcirc-server ((,class (:foreground ,yazz-green))))
   `(rcirc-server-prefix ((,class (:foreground ,yazz-green+1))))
   `(rcirc-timestamp ((,class (:foreground ,yazz-green+2))))
   `(rcirc-nick-in-message ((,class (:foreground ,yazz-yellow))))
   `(rcirc-nick-in-message-full-line ((,class (:bold t))))
   `(rcirc-prompt ((,class (:foreground ,yazz-yellow :bold t))))
   `(rcirc-track-nick ((,class (:inverse-video t))))
   `(rcirc-track-keyword ((,class (:bold t))))
   `(rcirc-url ((,class (:bold t))))
   `(rcirc-keyword ((,class (:foreground ,yazz-yellow :bold t))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,yazz-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,yazz-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,yazz-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,yazz-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,yazz-red))))
   `(rpm-spec-package-face ((,class (:foreground ,yazz-red))))
   `(rpm-spec-section-face ((,class (:foreground ,yazz-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,yazz-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,yazz-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,yazz-orange))))
   `(rst-level-2-face ((,class (:foreground ,yazz-green+1))))
   `(rst-level-3-face ((,class (:foreground ,yazz-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,yazz-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,yazz-cyan))))
   `(rst-level-6-face ((,class (:foreground ,yazz-green-1))))

   ;; sbt-mode
   `(sbt:error ((,class (:foreground ,yazz-red))))
   `(sbt:info ((,class (:foreground ,yazz-green))))
   `(sbt:warning ((,class (:foreground ,yazz-orange))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,yazz-red-3 :background ,yazz-bg :weight bold))))
   `(show-paren-match ((,class (:foreground ,yazz-blue+1 :background ,yazz-bg+1b :weight bold))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,yazz-red))))

   ;; term
   `(term ((,class (:foreground ,yazz-fg))))
   `(term-color-black ((,class (:foreground ,yazz-bg))))
   `(term-color-blue ((,class (:foreground ,yazz-blue))))
   `(term-color-cyan ((,class (:foreground ,yazz-cyan))))
   `(term-color-green ((,class (:foreground ,yazz-green))))
   `(term-color-magenta ((,class (:foreground ,yazz-magenta))))
   `(term-color-red ((,class (:foreground ,yazz-red))))
   `(term-color-white ((,class (:foreground ,yazz-fg))))
   `(term-color-yellow ((,class (:foreground ,yazz-yellow))))

   ;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,yazz-blue))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,yazz-red :weight bold))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,yazz-fg))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yazz-yellow))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground, yazz-fg))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,yazz-bg :foreground ,yazz-bg+1))))
   `(whitespace-hspace ((,class (:background ,yazz-bg :foreground ,yazz-bg+1))))
   `(whitespace-tab ((,class (:background ,yazz-bg :foreground ,yazz-red))))
   `(whitespace-newline ((,class (:foreground ,yazz-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,yazz-red :background ,yazz-bg))))
   `(whitespace-line ((,class (:background ,yazz-bg-05 :foreground ,yazz-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,yazz-orange :foreground ,yazz-orange))))
   `(whitespace-indentation ((,class (:background ,yazz-yellow :foreground ,yazz-red))))
   `(whitespace-empty ((,class (:background ,yazz-yellow :foreground ,yazz-red))))
   `(whitespace-space-after-tab ((,class (:background ,yazz-yellow :foreground ,yazz-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,yazz-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,yazz-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,yazz-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,yazz-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,yazz-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,yazz-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,yazz-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,yazz-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,yazz-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,yazz-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,yazz-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,yazz-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,yazz-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,yazz-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,yazz-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,yazz-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,yazz-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,yazz-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,yazz-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,yazz-fg
                                                              :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,yazz-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,yazz-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yazz-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,yazz-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,yazz-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; ensime
   `(ensime-errline-highlight ((,class (:background ,yazz-red :foreground ,yazz-fg))))
   `(ensime-warnline ((,class (:background ,yazz-bg+1))))
   
   ;; coffee-mode
   `(coffee-mode-function-param ((,class (:foreground ,yazz-blue+1))))
   `(coffee-mode-class-name ((,class (:foreground ,yazz-blue))))
   
   ;; which-func-mode
   `(which-func ((,class (:foreground ,yazz-green+4)))))

  ;;; custom theme variables
  (custom-theme-set-variables
   'yazz
   `(ansi-color-names-vector [,yazz-bg ,yazz-red ,yazz-green ,yazz-yellow
                                       ,yazz-blue ,yazz-magenta ,yazz-cyan ,yazz-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,yazz-bg-05)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yazz)

;;; yazz-theme.el ends here.
