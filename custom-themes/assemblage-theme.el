;;; assemblage-theme.el --- a dark theme for Emacs 24
;;;
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-assemblage-theme 
;;; Version: 20130715.621
;;;
;;; Changelog :
;;;
;;; 20130715.621: Inital version
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, version 3 of the License.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs.
;;;
;;; This file is not a part of Emacs
;;;

(deftheme assemblage
  "assemblage theme - a dark theme for Emacs 24")

(custom-theme-set-faces
 'assemblage

 '(cursor                              ((t (                      :background "#99d1ce"                                                          ))))
 '(default                             ((t (:foreground "#b8cfd3" :background "#101617"                        :inherit (fixed-pitch)            ))))
 '(linum                               ((t (:foreground "#53676b" :background "#09150F"                                                          ))))
 '(minibuffer-prompt                   ((t (:foreground "#1278A8" :background nil          :weight bold                                          ))))
 '(escape-glyph                        ((t (:foreground "orange"  :background nil                                                                ))))
 '(highlight                           ((t (                      :background "#2b3132"                                                          ))))
 '(region                              ((t (                      :background "#203b56"                                                          ))))
 '(fringe                              ((t (                      :background "#171f20"                                                          ))))
 '(shadow                              ((t (:foreground "#777777" :background nil                                                                ))))
 '(secondary-selection                 ((t (                      :background "#132125"                                                          ))))
 '(trailing-whitespace                 ((t (                      :background "#C74000"                                                          ))))

 '(show-paren-match ((t (:background "#477099" :weight bold))))

 `(font-lock-comment-face              ((t (:foreground "#59676b" :background nil :slant italic                                                  ))))
 `(font-lock-comment-delimiter-face    ((t (                                                                   :inherit font-lock-comment-face   ))))
 `(font-lock-constant-face             ((t (:foreground "#00E1DF" :background nil                                                                ))))
 `(font-lock-builtin-face              ((t (:foreground "#4FDaDF" :background nil                                                                ))))
 `(font-lock-function-name-face        ((t (:foreground "#1eeecc" :background nil                                                                ))))
 `(font-lock-variable-name-face        ((t (:foreground "#64adbc" :background nil                                                                ))))
 `(font-lock-keyword-face              ((t (:foreground "#50cba2" :background nil                                                                ))))
 `(font-lock-string-face               ((t (:foreground "#1994C1" :background nil                                                                ))))
 `(font-lock-doc-string-face           ((t (:foreground "#1994C1" :background nil                                                                ))))
 `(font-lock-doc-face                  ((t (:foreground "#1994C1" :background nil                                                                ))))
 `(font-lock-type-face                 ((t (:foreground "#86acBa" :background nil                                                                ))))
 `(haskell-type-face                   ((t (:foreground "#62c8d6" :background nil                                                                ))))

 '(font-lock-negation-char-face        ((t (:foreground "#C75311" :background nil                                                                ))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#3499aa" :background nil                              :inherit (font-lock-comment-face) ))))
 '(font-lock-preprocessor-face         ((t (:foreground "#d26937" :background nil                              :inherit (font-lock-builtin-face) ))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500" :background nil                              :inherit (bold)                   ))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"     :background nil                              :inherit (bold)                   ))))
 '(font-lock-doc-face                  ((t (:foreground "#90A0A0" :background nil                              :inherit (font-lock-string-face)  ))))
 '(font-lock-warning-face              ((t (:foreground "salmon"  :background nil                              :inherit (error)                  ))))

 '(link                                ((t (:foreground "#00b7f0" :background nil       :underline t                                             ))))
 '(link-visited                        ((t (:foreground "magenta4"                      :underline t           :inherit (link)                   ))))
 '(button                              ((t (:foreground "#FFFFFF" :background "#333333" :underline t           :inherit (link)                   ))))
 '(vertical-border                     ((t (:foreground "#75758a"                                                                                ))))
 '(next-error                          ((t (                                                                   :inherit (region)                 ))))
 '(query-replace                       ((t (                                                                   :inherit (isearch)                ))))
 '(header-line                         ((t (:foreground "#222222" :background "#bbbbbb" :box nil               :inherit (mode-line)              ))))
 '(mode-line-highlight                 ((t (                                            :box nil                                                 ))))
 '(mode-line-emphasis                  ((t (                                                     :weight bold                                    ))))
 '(mode-line-buffer-id                 ((t (                                                     :weight bold                                    ))))
 ;; '(mode-line                           ((t (:background "#505050" :foreground "#d6d6d6" :box (:line-width 1 :color "#505050")))))
 ;; '(mode-line-inactive                  ((t (:inherit mode-line :background "#282828" :foreground "#858585"))))
 '(mode-line ((t (:box nil :foreground "#89c9d7" :background "#233a49"))))
 '(mode-line-inactive ((t (:box nil :foreground "#245361" :background "#19232d"))))
 '(persp-selected-face                 ((t (:inherit font-lock-string-face :weight bold))))
 '(isearch-fail                        ((t (:foreground "#3f4758" :background "salmon"                                                          ))))
 '(isearch                             ((t (:inverse-video nil :foreground "black" :background "#d26937"))))
 '(lazy-highlight                      ((t (:foreground "black" :background "#2a6e83"))))
 '(match                               ((t (                      :background "#3388cc"                                                          ))))
 '(tooltip                             ((t (:foreground "black"   :background "LightYellow"                    :inherit (variable-pitch)         ))))
 '(js3-function-param-face             ((t (:foreground "#AFD3C9"                                                                                ))))
 '(js3-external-variable-face          ((t (:foreground "#A0B0B0"                                                                                ))))
 '(cua-rectangle                       ((t (:foreground "white"   :background "#DD6600"                                                          ))))

 '(outline-1 ((t (:foreground "#49a4c1" :weight bold))))
 '(outline-2 ((t (:foreground "#41bc93" :weight bold))))
 '(outline-3 ((t (:foreground "#B48EAD" :weight bold))))
 '(outline-4 ((t (:foreground "#49a4c1"))))
 '(outline-5 ((t (:foreground "#41bc93"))))
 '(outline-6 ((t (:foreground "#B48EAD"))))
 )

;; Rainbow delimiters
(defun assemblage-rainbow-delim-set-face ()
  (set-face-attribute 'rainbow-delimiters-depth-1-face   nil :foreground "#446622")
  (set-face-attribute 'rainbow-delimiters-depth-2-face   nil :foreground "#668844")
  (set-face-attribute 'rainbow-delimiters-depth-3-face   nil :foreground "#88aa66")
  (set-face-attribute 'rainbow-delimiters-depth-4-face   nil :foreground "#AACC88")
  (set-face-attribute 'rainbow-delimiters-depth-5-face   nil :foreground "#CCDDAA")
  (set-face-attribute 'rainbow-delimiters-depth-6-face   nil :foreground "#DEEEAA")
  (set-face-attribute 'rainbow-delimiters-depth-7-face   nil :foreground "#EFFFBB")
  (set-face-attribute 'rainbow-delimiters-depth-8-face   nil :foreground "#EFFFCC")
  (set-face-attribute 'rainbow-delimiters-depth-9-face   nil :foreground "#EFFFEE")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#AA0000"))

(eval-after-load "rainbow-delimiters" '(assemblage-rainbow-delim-set-face))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'assemblage)

;;; assemblage-theme.el ends here
