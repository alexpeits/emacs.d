;;; monokai-light-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/monokai-light-emacs
;; Package-Version: 20170731.504
;; Version: 3.3.0

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
;; A port of the popular Textmate theme Monokai-Light for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.monokai-light.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal monokai-light theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/monokai-light-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The monokai-light theme requires Emacs 24 or later!"))

(deftheme monokai-light "The Monokai-Light colour theme")

(defgroup monokai-light nil
  "Monokai-Light theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom monokai-light-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'monokai-light)

(defcustom monokai-light-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'monokai-light)

(defcustom monokai-light-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'monokai-light)

(defcustom monokai-light-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'monokai-light)

(defcustom monokai-light-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'monokai-light)

(defcustom monokai-light-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'monokai-light)

(defcustom monokai-light-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'monokai-light)

;; Primary colors
(defcustom monokai-light-yellow "#D73700"
  "Primary colors - yellow"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-red "#D62867"
  "Primary colors - red"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-magenta "#DB67D1"
  "Primary colors - magenta"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-blue "#3A8F9F"
  "Primary colors - blue"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-green "#7AA400"
  "Primary colors - green"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-cyan "#6A9D96"
  "Primary colors - cyan"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-violet "#6749B0"
  "Primary colors - violet"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-gray "#C4C4B2"
  "Primary colors - gray"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-foreground "#28406b"
  "Adaptive colors - foreground"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-background "#FDF8E1"
  "Adaptive colors - background"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-comments "#898989"
  "Adaptive colors - comments"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-emphasis "#28406b"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-highlight "#DAD5BF"
  "Adaptive colors - highlight"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-highlight-alt "#F0EAD5"
  "Adaptive colors - highlight"
  :type 'string
  :group 'monokai-light)

(defcustom monokai-light-highlight-line "#EEE9D2"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'monokai-light)

(let* (;; Variable pitch
       (monokai-light-pitch (if monokai-light-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (monokai-light-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (monokai-light-yellow-d       "#BEB244")
       (monokai-light-yellow-l       "#E4DC90")
       (monokai-light-orange-d       "#D47402")
       (monokai-light-orange-l       "#FFAC4A")
       (monokai-light-red-d          "#F70057")
       (monokai-light-red-l          "#FA518D")
       (monokai-light-magenta-d      "#FB35EA")
       (monokai-light-magenta-l      "#FE8CF4")
       (monokai-light-violet-d       "#945AFF")
       (monokai-light-violet-l       "#C9ACFF")
       (monokai-light-blue-d         "#47A3B4")
       (monokai-light-blue-l         "#6CBFCE")
       (monokai-light-cyan-d         "#69A89F")
       (monokai-light-cyan-l         "#89C6BF")
       (monokai-light-green-d        "#86C30D")
       (monokai-light-green-l        "#BBEF53")
       (monokai-light-gray-d         "#B8B8A6")
       (monokai-light-gray-l         "#d1d1bf")
       ;; Adaptive higher/lower contrast accented colors
       (monokai-light-foreground-hc  "#141414")
       (monokai-light-foreground-lc  "#171A0B")
       ;; High contrast colors
       (monokai-light-yellow-hc      "#FFFACE")
       (monokai-light-yellow-lc      "#9A8F21")
       (monokai-light-orange-hc      "#FFBE74")
       (monokai-light-orange-lc      "#A75B00")
       (monokai-light-red-hc         "#FEB0CC")
       (monokai-light-red-lc         "#F20055")
       (monokai-light-magenta-hc     "#FEC6F9")
       (monokai-light-magenta-lc     "#F309DF")
       (monokai-light-violet-hc      "#F0E7FF")
       (monokai-light-violet-lc      "#7830FC")
       (monokai-light-blue-hc        "#CAF5FD")
       (monokai-light-blue-lc        "#1DB4D0")
       (monokai-light-cyan-hc        "#D3FBF6")
       (monokai-light-cyan-lc        "#4BBEAE")
       (monokai-light-green-hc       "#CCF47C")
       (monokai-light-green-lc       "#679A01")

       ;; Distinct fringe
       (monokai-light-fringe-bg (if monokai-light-distinct-fringe-background
                              monokai-light-gray
                            monokai-light-background))

       ;; Definitions for terminals that do not support 256 colors
       (monokai-light-256-class '((class color) (min-colors 89)))
       ;; Primary colors
       (monokai-light-256-yellow         "#CDC673")
       (monokai-light-256-orange         "#FF8C00")
       (monokai-light-256-red            "#FF1493")
       (monokai-light-256-magenta        "#D700D7")
       (monokai-light-256-violet         "#AF87FF")
       (monokai-light-256-blue           "#5FD7FF")
       (monokai-light-256-cyan           "#5FFFFF")
       (monokai-light-256-green          "#87D700")
       (monokai-light-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (monokai-light-256-yellow-d       "#878700")
       (monokai-light-256-yellow-l       "#FFFF87")
       (monokai-light-256-orange-d       "#AF5F00")
       (monokai-light-256-orange-l       "#FFAF5F")
       (monokai-light-256-red-d          "#870000")
       (monokai-light-256-red-l          "#FF5F87")
       (monokai-light-256-magenta-d      "#AF0087")
       (monokai-light-256-magenta-l      "#FF87DF")
       (monokai-light-256-violet-d       "#5F00AF")
       (monokai-light-256-violet-l       "#AF87D7")
       (monokai-light-256-blue-d         "#008787")
       (monokai-light-256-blue-l         "#87D7FF")
       (monokai-light-256-cyan-d         "#5FAFAF")
       (monokai-light-256-cyan-l         "#AFFFFF")
       (monokai-light-256-green-d        "#5F8700")
       (monokai-light-256-green-l        "#AFD700")
       (monokai-light-256-gray-d         "#333333")
       (monokai-light-256-gray-l         "#707070")
       ;; Adaptive colors
       (monokai-light-256-foreground     "#F5F5F5")
       (monokai-light-256-background     "#1B1E1C")
       (monokai-light-256-comments       "#8B8878")
       (monokai-light-256-emphasis       "#FFFAFA")
       (monokai-light-256-line-number    "#8F908A")
       (monokai-light-256-highlight      "#474747")
       (monokai-light-256-highlight-alt  "#3E3E3E")
       (monokai-light-256-highlight-line "#000000")
       ;; Adaptive higher/lower contrast accented colors
       (monokai-light-256-foreground-hc  "#171A0B")
       (monokai-light-256-foreground-lc  "#141414")
       ;; High contrast colors
       (monokai-light-256-yellow-hc      monokai-light-256-yellow-d)
       (monokai-light-256-yellow-lc      monokai-light-256-yellow-l)
       (monokai-light-256-orange-hc      monokai-light-256-orange-d)
       (monokai-light-256-orange-lc      monokai-light-256-orange-l)
       (monokai-light-256-red-hc         monokai-light-256-red-d)
       (monokai-light-256-red-lc         monokai-light-256-red-l)
       (monokai-light-256-magenta-hc     monokai-light-256-magenta-d)
       (monokai-light-256-magenta-lc     monokai-light-256-magenta-l)
       (monokai-light-256-violet-hc      monokai-light-256-violet-d)
       (monokai-light-256-violet-lc      monokai-light-256-violet-l)
       (monokai-light-256-blue-hc        monokai-light-256-blue-d)
       (monokai-light-256-blue-lc        monokai-light-256-blue-l)
       (monokai-light-256-cyan-hc        monokai-light-256-cyan-d)
       (monokai-light-256-cyan-lc        monokai-light-256-cyan-l)
       (monokai-light-256-green-hc       monokai-light-256-green-d)
       (monokai-light-256-green-lc       monokai-light-256-green-l)

       ;; Distinct fringe
       (monokai-light-256-fringe-bg (if monokai-light-distinct-fringe-background
                                  monokai-light-256-gray
                                monokai-light-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'monokai-light

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(font-lock-comment-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(font-lock-constant-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(font-lock-doc-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(font-lock-function-name-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(font-lock-keyword-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(font-lock-type-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :italic nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(font-lock-warning-face
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,monokai-light-class (:inherit font-lock-constant-face))
      (,monokai-light-256-class  (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,monokai-light-class (:foreground ,monokai-light-foreground
                                    :background ,monokai-light-background))
       (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                         :background ,monokai-light-256-background))))

   `(highlight
     ((,monokai-light-class (:background ,monokai-light-highlight))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight))))

   `(lazy-highlight
     ((,monokai-light-class (:inherit highlight
                                :background ,monokai-light-highlight-alt))
      (,monokai-light-256-class  (:inherit highlight
                                     :background ,monokai-light-256-highlight-alt))))

   `(region
     ((,monokai-light-class (:inherit highlight
                                :background ,monokai-light-highlight))
      (,monokai-light-256-class  (:inherit highlight
                                     :background ,monokai-light-256-highlight))))

   `(secondary-selection
     ((,monokai-light-class (:inherit region
                                :background ,monokai-light-highlight-alt))
      (,monokai-light-256-class  (:inherit region
                                     :background ,monokai-light-256-highlight-alt))))

   `(shadow
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(match
     ((,monokai-light-class (:background ,monokai-light-green
                                   :foreground ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-green
                                        :foreground ,monokai-light-256-background
                                        :weight bold))))

   `(cursor
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-foreground
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-foreground
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(escape-glyph-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(fringe
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :background ,monokai-light-fringe-bg))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :background ,monokai-light-256-fringe-bg))))

   `(link
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :underline t
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :underline t
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,monokai-light-class (:foreground ,monokai-light-green ))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green ))))

   `(warning
     ((,monokai-light-class (:foreground ,monokai-light-yellow ))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow ))))

   `(error
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(eval-sexp-fu-flash
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-green))))

   `(eval-sexp-fu-flash-error
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-red))))

   `(trailing-whitespace
     ((,monokai-light-class (:background ,monokai-light-red))
      (,monokai-light-256-class  (:background ,monokai-light-256-red))))

   `(vertical-border
     ((,monokai-light-class (:foreground ,monokai-light-gray))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-gray))))

   `(menu
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :background ,monokai-light-256-background))))

   `(minibuffer-prompt
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   `(mode-line
     ((,monokai-light-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,monokai-light-emphasis
                                      :background ,monokai-light-highlight
                                      :box (:line-width 1
                                                        :color ,monokai-light-gray
                                                        :style unspecified)))
      (,monokai-light-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,monokai-light-256-foreground
                                           :background ,monokai-light-256-background
                                           :box (:line-width 1
                                                             :color ,monokai-light-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,monokai-light-class (:background ,monokai-light-gray-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray-d))))

   `(powerline-active2
     ((,monokai-light-class (:background ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-background))))


   `(mode-line-inactive
     ((,monokai-light-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,monokai-light-comments
                                      :background ,monokai-light-background
                                      :box (:line-width 1
                                                        :color ,monokai-light-gray
                                                        :style unspecified)))
      (,monokai-light-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,monokai-light-256-comments
                                           :background ,monokai-light-256-background
                                           :box (:line-width 1
                                                             :color ,monokai-light-256-gray
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,monokai-light-class (:background ,monokai-light-gray-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray-d))))

   `(powerline-inactive2
     ((,monokai-light-class (:background ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-background))))

   ;; header-line
   `(header-line
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-highlight
                                   :box (:color ,monokai-light-gray
                                                :line-width 1
                                                :style unspecified)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-highlight
                                        :box (:color ,monokai-light-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground ,monokai-light-256-background))))

   `(cua-rectangle
     ((,monokai-light-class (:inherit region))
      (,monokai-light-256-class  (:inherit region))))

   `(cua-rectangle-noselect
     ((,monokai-light-class (:inherit secondary-selection))
      (,monokai-light-256-class  (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   ;; dired
   `(dired-directory
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(dired-flagged
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(dired-header
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,monokai-light-class (:inherit shadow))
      (,monokai-light-256-class  (:inherit shadow))))

   `(dired-mark
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   `(dired-marked
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-blue))))

   `(dropdown-list-selection-face
     ((,monokai-light-class (:background ,monokai-light-green
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-green
                                        :foreground ,monokai-light-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,monokai-light-class (:inherit ecb-history-bucket-node-face
                                :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:inherit ecb-history-bucket-node-face
                                     :foreground ,monokai-light-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,monokai-light-class (:inherit ecb-directories-general-face
                                :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,monokai-light-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,monokai-light-class (:inherit ecb-history-general-face
                                :foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:inherit ecb-history-general-face
                                     :foreground ,monokai-light-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,monokai-light-class (:inherit ecb-directories-general-face
                                :foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,monokai-light-256-comments))))

   `(ecb-bucket-node-face
     ((,monokai-light-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,monokai-light-256-blue))))

   `(ecb-tag-header-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,monokai-light-class (:inherit ecb-analyse-general-face
                                :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:inherit ecb-analyse-general-face
                                     :foreground ,monokai-light-256-green))))

   `(ecb-directories-general-face
     ((,monokai-light-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,monokai-light-256-class  (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,monokai-light-class (:inherit ecb-methods-general-face
                                :foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:inherit ecb-methods-general-face
                                     :foreground ,monokai-light-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(ecb-tree-guide-line-face
     ((,monokai-light-class (:inherit ecb-default-general-face
                                :foreground ,monokai-light-gray
                                :height 1.0))
      (,monokai-light-256-class  (:inherit ecb-default-general-face
                                     :foreground ,monokai-light-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,monokai-light-class (:foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis))))

   `(ee-category
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(ee-link
     ((,monokai-light-class (:inherit link))
      (,monokai-light-256-class  (:inherit link))))

   `(ee-link-visited
     ((,monokai-light-class (:inherit link-visited))
      (,monokai-light-256-class  (:inherit link-visited))))

   `(ee-marked
     ((,monokai-light-class (:foreground ,monokai-light-magenta
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(ee-shadow
     ((,monokai-light-class (:inherit shadow))
      (,monokai-light-256-class  (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(grep-error-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(grep-match-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,monokai-light-class (:inherit region
                                :background ,monokai-light-green))
      (,monokai-light-256-class  (:inherit region
                                     :background ,monokai-light-256-green))))

   `(isearch-fail
     ((,monokai-light-class (:inherit isearch
                                :foreground ,monokai-light-red
                                :background ,monokai-light-background
                                :bold t))
      (,monokai-light-256-class  (:inherit isearch
                                     :foreground ,monokai-light-256-red
                                     :background ,monokai-light-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background ,monokai-light-background
                                   :inverse-video nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :background ,monokai-light-background
                                   :inverse-video nil
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :background ,monokai-light-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,monokai-light-class (:inherit bold
                                :foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:inherit bold
                                     :foreground ,monokai-light-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,monokai-light-class (:background unspecified))
      (,monokai-light-256-class  (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,monokai-light-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,monokai-light-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,monokai-light-class (:inherit italic :foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:inherit italic :foreground ,monokai-light-256-emphasis))))

   `(font-latex-math-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(font-latex-sectioning-0-face
     ((,monokai-light-class (:inherit font-latex-sectioning-1-face
                                :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:inherit font-latex-sectioning-1-face
                                     :height ,monokai-light-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,monokai-light-class (:inherit font-latex-sectioning-2-face
                                :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:inherit font-latex-sectioning-2-face
                                     :height ,monokai-light-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,monokai-light-class (:inherit font-latex-sectioning-3-face
                                :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:inherit font-latex-sectioning-3-face
                                     :height ,monokai-light-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,monokai-light-class (:inherit font-latex-sectioning-4-face
                                :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:inherit font-latex-sectioning-4-face
                                     :height ,monokai-light-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,monokai-light-class (:inherit font-latex-sectioning-5-face
                                :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:inherit font-latex-sectioning-5-face
                                     :height ,monokai-light-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-yellow
                                :weight bold))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch :
                                     foreground ,monokai-light-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis))))

   `(font-latex-slide-title-face
     ((,monokai-light-class (:inherit (,monokai-light-pitch font-lock-type-face)
                                :weight bold
                                :height ,monokai-light-height-plus-3))
      (,monokai-light-256-class  (:inherit (,monokai-light-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,monokai-light-height-plus-3))))

   `(font-latex-string-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(font-latex-subscript-face
     ((,monokai-light-class (:height ,monokai-light-height-minus-1))
      (,monokai-light-256-class  (:height ,monokai-light-height-minus-1))))

   `(font-latex-superscript-face
     ((,monokai-light-class (:height ,monokai-light-height-minus-1))
      (,monokai-light-256-class  (:height ,monokai-light-height-minus-1))))

   `(font-latex-verbatim-face
     ((,monokai-light-class (:inherit fixed-pitch
                                :foreground ,monokai-light-foreground
                                :slant italic))
      (,monokai-light-256-class  (:inherit fixed-pitch
                                     :foreground ,monokai-light-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,monokai-light-class (:inherit bold
                                :foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:inherit bold
                                     :foreground ,monokai-light-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-blue))))

   `(ac-selection-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(ac-candidate-mouse-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(ac-completion-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-blue))))

   `(ac-gtags-selection-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(ac-yasnippet-candidate-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground ,monokai-light-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-blue))))

   `(ahs-edit-mode-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-highlight))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-highlight))))

   `(ahs-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-violet ))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-green))))

   `(ahs-warning-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(android-mode-error-face
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(android-mode-verbose-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(android-mode-warning-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,monokai-light-class (:background ,monokai-light-yellow-lc
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc
                                        :foreground ,monokai-light-256-background))))

   `(bm-fringe-face
     ((,monokai-light-class (:background ,monokai-light-yellow-lc
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc
                                        :foreground ,monokai-light-256-background))))

   `(bm-fringe-persistent-face
     ((,monokai-light-class (:background ,monokai-light-green-lc
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc
                                        :foreground ,monokai-light-256-background))))

   `(bm-persistent-face
     ((,monokai-light-class (:background ,monokai-light-green-lc
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc
                                        :foreground ,monokai-light-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(cfw:face-annotation
     ((,monokai-light-class (:inherit cfw:face-day-title
                                :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:inherit cfw:face-day-title
                                     :foreground ,monokai-light-256-yellow))))

   `(cfw:face-default-content
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(cfw:face-default-day
     ((,monokai-light-class (:inherit cfw:face-day-title
                                :weight bold))
      (,monokai-light-256-class  (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,monokai-light-class (:inherit cfw:face-day-title
                                :foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:inherit cfw:face-day-title
                                     :foreground ,monokai-light-256-comments))))

   `(cfw:face-grid
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(cfw:face-header
     ((,monokai-light-class (:foreground ,monokai-light-blue-hc
                                   :background ,monokai-light-blue-lc
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue-hc
                                        :background ,monokai-light-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,monokai-light-class (:background nil
                                   :foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:background nil
                                        :foreground ,monokai-light-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,monokai-light-class (:foreground ,monokai-light-magenta))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta))))

   `(cfw:face-select
     ((,monokai-light-class (:background ,monokai-light-magenta-lc
                                   :foreground ,monokai-light-magenta-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-magenta-lc
                                        :foreground ,monokai-light-256-magenta-hc))))

   `(cfw:face-saturday
     ((,monokai-light-class (:foreground ,monokai-light-cyan-hc
                                   :background ,monokai-light-cyan-lc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan-hc
                                        :background ,monokai-light-256-cyan-lc))))

   `(cfw:face-sunday
     ((,monokai-light-class (:foreground ,monokai-light-red-hc
                                   :background ,monokai-light-red-lc
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red-hc
                                        :background ,monokai-light-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-yellow
                                :weight bold
                                :height ,monokai-light-height-plus-4))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-yellow
                                     :weight bold
                                     :height ,monokai-light-height-plus-4))))

   `(cfw:face-today
     ((,monokai-light-class (:weight bold
                               :background ,monokai-light-highlight-line
                               :foreground nil))
      (,monokai-light-256-class  (:weight bold
                                    :background ,monokai-light-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,monokai-light-class (:background ,monokai-light-yellow-lc
                                   :foreground ,monokai-light-yellow-hc
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc
                                        :foreground ,monokai-light-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,monokai-light-class (:background ,monokai-light-yellow-lc
                                   :foreground ,monokai-light-yellow-hc
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc
                                        :foreground ,monokai-light-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,monokai-light-class (:background ,monokai-light-yellow-hc
                                   :foreground ,monokai-light-yellow-lc
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-hc
                                        :foreground ,monokai-light-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :background nil
                                   :box (:color ,monokai-light-yellow :line-width -1 :style nil)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :background nil
                                        :box (:color ,monokai-light-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(cider-instrumented-face
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :background nil
                                   :box (:color ,monokai-light-violet :line-width -1 :style nil)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :background nil
                                        :box (:color ,monokai-light-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background nil
                                   :box (:color ,monokai-light-blue :line-width -1 :style nil)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background nil
                                        :box (:color ,monokai-light-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-orange))))

   `(cider-test-failure-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-red))))

   `(cider-test-success-face
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-green))))

   `(cider-traced-face
     ((,monokai-light-class :box (:color ,monokai-light-blue :line-width -1 :style nil))
      (,monokai-light-256-class  :box (:color ,monokai-light-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis))))

   `(company-tooltip-selection
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(company-tooltip-mouse
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(company-tooltip-common
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-blue
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-blue
                                        :underline t))))

   `(company-preview
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis))))

   `(company-preview-common
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,monokai-light-class (:background ,monokai-light-gray))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray))))

   `(company-scrollbar-fg
     ((,monokai-light-class (:background ,monokai-light-comments))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments))))

   `(company-tooltip-annotation
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-green))))

   `(company-template-field
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,monokai-light-class (:inherit font-lock-doc-face
                                :foreground ,monokai-light-cyan
                                :underline nil))
      (,monokai-light-256-class  (:inherit font-lock-doc-face
                                     :foreground ,monokai-light-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :underline nil))))

   `(compilation-error
     ((,monokai-light-class (:inherit error
                                :underline nil))
      (,monokai-light-256-class  (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :underline nil))))

   `(compilation-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :underline nil
                                   :bold nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,monokai-light-class (:inherit warning
                                :underline nil))
      (,monokai-light-256-class  (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight normal
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,monokai-light-class (:inherit compilation-info
                                :foreground ,monokai-light-green
                                :weight bold))
      (,monokai-light-256-class  (:inherit compilation-info
                                     :foreground ,monokai-light-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,monokai-light-class (:inherit compilation-error
                                :foreground ,monokai-light-red
                                :weight bold))
      (,monokai-light-256-class  (:inherit compilation-error
                                     :foreground ,monokai-light-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(cscope-line-number-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(cscope-line-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(cscope-mouse-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis
                                   :underline ,monokai-light-emphasis
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis
                                        :underline ,monokai-light-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,monokai-light-class (:background ,monokai-light-gray
                                   :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray
                                        :foreground ,monokai-light-256-yellow))))

   `(ctbl:face-row-select
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground
                                   :underline t))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :height ,monokai-light-height-plus-3
                                :foreground ,monokai-light-violet
                                :weight bold))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :height ,monokai-light-height-plus-3
                                     :foreground ,monokai-light-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-cyan
                                :height ,monokai-light-height-plus-3))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-cyan
                                     :height ,monokai-light-height-plus-3))))

   `(custom-comment-tag
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(custom-group-tag
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-blue
                                :height ,monokai-light-height-plus-3))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-blue
                                     :height ,monokai-light-height-plus-3))))

   `(custom-group-tag-1
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-red
                                :height ,monokai-light-height-plus-3))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-red
                                     :height ,monokai-light-height-plus-3))))

   `(custom-state
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   ;; diff
   `(diff-added
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-background))))

   `(diff-changed
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-background))))

   `(diff-removed
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background))))

   `(diff-header
     ((,monokai-light-class (:background ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-background))))

   `(diff-file-header
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-foreground
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-green))))

   `(diff-refine-change
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-blue))))

   `(diff-refine-removed
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,monokai-light-class (:background ,monokai-light-blue-lc
                                   :foreground ,monokai-light-blue-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue-lc
                                        :foreground ,monokai-light-256-blue-hc))))

   `(diff-hl-delete
     ((,monokai-light-class (:background ,monokai-light-red-lc
                                   :foreground ,monokai-light-red-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-red-lc
                                        :foreground ,monokai-light-256-red-hc))))

   `(diff-hl-insert
     ((,monokai-light-class (:background ,monokai-light-green-lc
                                   :foreground ,monokai-light-green-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc
                                        :foreground ,monokai-light-256-green-hc))))

   `(diff-hl-unknown
     ((,monokai-light-class (:background ,monokai-light-violet-lc
                                   :foreground ,monokai-light-violet-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-violet-lc
                                        :foreground ,monokai-light-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,monokai-light-class (:background ,monokai-light-orange-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-orange-lc))))

   `(ediff-fine-diff-B
     ((,monokai-light-class (:background ,monokai-light-green-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc))))

   `(ediff-fine-diff-C
     ((,monokai-light-class (:background ,monokai-light-yellow-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc))))

   `(ediff-current-diff-C
     ((,monokai-light-class (:background ,monokai-light-blue-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue-lc))))

   `(ediff-even-diff-A
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-foreground-lc ))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-foreground-hc ))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-foreground-hc ))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-foreground-lc ))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-foreground ))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-foreground ))))

   `(ediff-odd-diff-C
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-background ))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) monokai-light-class)
       (:underline (:style line :color ,monokai-light-red)
                   :inherit unspecified))
      (,monokai-light-class (:foreground ,monokai-light-red-hc
                                   :background ,monokai-light-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-light-256-class )
       (:underline (:style line :color ,monokai-light-256-red)
                   :inherit unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red-hc
                                        :background ,monokai-light-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) monokai-light-class)
       (:underline (:style line :color ,monokai-light-yellow)
                   :inherit unspecified))
      (,monokai-light-class (:foreground ,monokai-light-yellow-hc
                                   :background ,monokai-light-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-light-256-class )
       (:underline (:style line :color ,monokai-light-256-yellow)
                   :inherit unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow-hc
                                        :background ,monokai-light-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :background unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground unspecified))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground unspecified))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(elfeed-search-feed-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(elfeed-search-tag-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(elfeed-search-title-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))
   `(ein:cell-output-prompt
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))
   `(ein:notification-tab-normal
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))
   `(ein:notification-tab-selected
     ((,monokai-light-class (:foreground ,monokai-light-orange :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,monokai-light-class (:inherit font-lock-string-face))
      (,monokai-light-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,monokai-light-class (:inherit font-lock-string-face))
      (,monokai-light-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,monokai-light-class (:inherit font-lock-string-face))
      (,monokai-light-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,monokai-light-class (:inherit font-lock-keyword-face))
      (,monokai-light-256-class  (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-red)
                   :inherit unspecified))
      (,monokai-light-class (:foreground ,monokai-light-red-hc
                                   :background ,monokai-light-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-red)
                   :inherit unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red-hc
                                        :background ,monokai-light-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-orange)
                   :inherit unspecified))
      (,monokai-light-class (:foreground ,monokai-light-orange-hc
                                   :background ,monokai-light-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-orange)
                   :inherit unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange-hc
                                        :background ,monokai-light-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-background
                                   :weight normal
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,monokai-light-class (:inherit erc-default-face))
      (,monokai-light-256-class  (:inherit erc-default-face))))

   `(erc-bold-face
     ((,monokai-light-class (:weight bold))
      (,monokai-light-256-class  (:weight bold))))

   `(erc-current-nick-face
     ((,monokai-light-class (:foreground ,monokai-light-blue :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,monokai-light-class (:inherit font-lock-warning-face))
      (,monokai-light-256-class  (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(erc-highlight-face
     ((,monokai-light-class (:inherit erc-default-face
                                :background ,monokai-light-highlight))
      (,monokai-light-256-class  (:inherit erc-default-face
                                     :background ,monokai-light-256-highlight))))

   `(erc-direct-msg-face
     ((,monokai-light-class (:inherit erc-default-face))
      (,monokai-light-256-class  (:inherit erc-default-face))))

   `(erc-error-face
     ((,monokai-light-class (:inherit font-lock-warning-face))
      (,monokai-light-256-class  (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,monokai-light-class (:inherit erc-default-face))
      (,monokai-light-256-class  (:inherit erc-default-face))))

   `(erc-input-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(erc-keyword-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,monokai-light-class (:inherit erc-default-face))
      (,monokai-light-256-class  (:inherit erc-default-face))))

   `(erc-notice-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(erc-pal-face
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :background ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :background ,monokai-light-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,monokai-light-class (:inherit font-lock-comment-face))
      (,monokai-light-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,monokai-light-class (:inherit font-lock-comment-face))
      (,monokai-light-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(eshell-ls-missing
     ((,monokai-light-class (:inherit font-lock-warning-face))
      (,monokai-light-256-class  (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,monokai-light-class (:inherit font-lock-doc-face))
      (,monokai-light-256-class  (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-red-l
                                   :inherit italic))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-green-l
                                   :inherit italic))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line :foreground ,monokai-light-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,monokai-light-class (:inherit region))
      (,monokai-light-256-class  (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-orange
                                   :underline t
                                   :slant italic))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-orange
                                   :weight normal
                                   :slant italic))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-orange
                                   :weight normal
                                   :slant italic))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight normal
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-light-class (:foreground ,monokai-light-red-hc
                                   :background ,monokai-light-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red-hc
                                        :background ,monokai-light-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-light-class (:foreground ,monokai-light-green-hc
                                   :background ,monokai-light-green-lc))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green-hc
                                        :background ,monokai-light-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-light-class (:foreground ,monokai-light-yellow-hc
                                   :background ,monokai-light-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow-hc
                                        :background ,monokai-light-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) monokai-light-class)
       (:underline (:style line :color ,monokai-light-red)))
      (,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-light-256-class )
       (:underline (:style line :color ,monokai-light-256-red)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) monokai-light-class)
       (:underline (:style line :color ,monokai-light-orange)))
      (,monokai-light-class (:foreground ,monokai-light-orange
                                   :background ,monokai-light-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-light-256-class )
       (:underline (:style line :color ,monokai-light-256-orange)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :background ,monokai-light-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) monokai-light-class)
       (:underline (:style line :color ,monokai-light-blue)))
      (,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-light-256-class )
       (:underline (:style line :color ,monokai-light-256-blue)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,monokai-light-class (:foreground ,monokai-light-red-l
                                   :background unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,monokai-light-class (:foreground ,monokai-light-orange-l
                                   :background unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,monokai-light-class (:foreground ,monokai-light-blue-l
                                   :background unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-yellow)
                   :inherit unspecified))
      (,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-yellow)
                   :inherit unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) monokai-light-class)
       (:underline (:style wave :color ,monokai-light-red)
                   :inherit unspecified))
      (,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-light-256-class )
       (:underline (:style wave :color ,monokai-light-256-red)
                   :inherit unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,monokai-light-class (:background ,monokai-light-green
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-green
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,monokai-light-class (:background ,monokai-light-green
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-green
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-background
                                   :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-highlight-line
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-blue
                                        :background ,monokai-light-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(guide-key/key-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(guide-key/prefix-command-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,monokai-light-class (:inherit gnus-group-news-1-empty))
      (,monokai-light-256-class  (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,monokai-light-class (:inherit gnus-group-news-2-empty))
      (,monokai-light-256-class  (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,monokai-light-class (:inherit gnus-group-news-3-empty))
      (,monokai-light-256-class  (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,monokai-light-class (:inherit gnus-group-news-low-empty))
      (,monokai-light-256-class  (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,monokai-light-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,monokai-light-256-class  (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,monokai-light-class (:inherit message-header-other))
      (,monokai-light-256-class  (:inherit message-header-other))))

   `(gnus-header-from
     ((,monokai-light-class (:inherit message-header-other))
      (,monokai-light-256-class  (:inherit message-header-other))))

   `(gnus-header-name
     ((,monokai-light-class (:inherit message-header-name))
      (,monokai-light-256-class  (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,monokai-light-class (:inherit message-header-other))
      (,monokai-light-256-class  (:inherit message-header-other))))

   `(gnus-header-subject
     ((,monokai-light-class (:inherit message-header-subject))
      (,monokai-light-256-class  (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(gnus-summary-high-ancient
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-summary-low-read
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-summary-low-ticked
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(gnus-summary-low-unread
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-summary-normal-read
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-summary-normal-ticked
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(gnus-summary-normal-unread
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(gnus-summary-selected
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-cite-2
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-cite-3
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-cite-4
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-cite-5
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-cite-6
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-cite-7
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(gnus-cite-8
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(gnus-cite-9
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(gnus-cite-10
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(gnus-cite-11
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(gnus-group-news-1-empty
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(gnus-group-news-2-empty
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-group-news-3-empty
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(gnus-group-news-4-empty
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-group-news-5-empty
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(gnus-group-news-6-empty
     ((,monokai-light-class (:foreground ,monokai-light-blue-lc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(gnus-signature
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(gnus-x-face
     ((,monokai-light-class (:background ,monokai-light-foreground
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-foreground
                                        :foreground ,monokai-light-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(helm-apt-installed
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(helm-bookmark-directory
     ((,monokai-light-class (:inherit helm-ff-directory))
      (,monokai-light-256-class  (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(helm-bookmark-gnus
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(helm-bookmark-info
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(helm-bookmark-man
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(helm-bookmark-w3m
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(helm-bookmarks-su
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(helm-buffer-file
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(helm-buffer-directory
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(helm-buffer-process
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(helm-buffer-saved-out
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(helm-candidate-number
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis
                                   :bold t))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(helm-ff-executable
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(helm-ff-file
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-orange
                                   :slant italic))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,monokai-light-class (:background ,monokai-light-green
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-green
                                        :foreground ,monokai-light-256-background))))

   `(helm-ff-symlink
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(helm-grep-file
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(helm-grep-lineno
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(helm-grep-match
     ((,monokai-light-class (:inherit helm-match)))
     ((,monokai-light-256-class  (:inherit helm-match))))

   `(helm-grep-running
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(helm-header
     ((,monokai-light-class (:inherit header-line))
      (,monokai-light-256-class  (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(helm-lisp-show-completion
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :background ,monokai-light-highlight-line
                                   :bold t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :background ,monokai-light-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :underline t))))

   `(helm-match
     ((,monokai-light-class (:foreground ,monokai-light-green :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green :inherit bold))))

   `(helm-match-item
     ((,monokai-light-class (:inherit helm-match))
      (,monokai-light-256-class  (:inherit helm-match))))

   `(helm-selection
     ((,monokai-light-class (:background ,monokai-light-highlight
                                   :inherit bold
                                   :underline nil))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis
                                   :underline nil))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,monokai-light-class (:foreground ,monokai-light-gray))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-gray))))

   `(helm-source-header
     ((,monokai-light-class (:background ,monokai-light-violet-l
                                   :foreground ,monokai-light-background
                                   :underline nil))
      (,monokai-light-256-class  (:background ,monokai-light-256-violet-l
                                        :foreground ,monokai-light-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(helm-time-zone-current
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(helm-time-zone-home
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(helm-visible-mark
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-magenta :bold t))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,monokai-light-class :foreground ,monokai-light-blue)
      (,monokai-light-256-class  :foreground ,monokai-light-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,monokai-light-class :foreground ,monokai-light-blue-l)
      (,monokai-light-256-class  :foreground ,monokai-light-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,monokai-light-class :foreground ,monokai-light-blue-l)
      (,monokai-light-256-class  :foreground ,monokai-light-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,monokai-light-class :foreground ,monokai-light-orange)
      (,monokai-light-256-class  :foreground ,monokai-light-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,monokai-light-class :foreground ,monokai-light-green)
      (,monokai-light-256-class  :foreground ,monokai-light-256-green)))

   `(helm-ls-git-added-modified-face
     ((,monokai-light-class :foreground ,monokai-light-green-l)
      (,monokai-light-256-class  :foreground ,monokai-light-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,monokai-light-class :foreground ,monokai-light-red)
      (,monokai-light-256-class  :foreground ,monokai-light-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,monokai-light-class :foreground ,monokai-light-red-l)
      (,monokai-light-256-class  :foreground ,monokai-light-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,monokai-light-class :foreground ,monokai-light-yellow)
      (,monokai-light-256-class  :foreground ,monokai-light-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,monokai-light-class (:foreground ,monokai-light-yellow-lc
                                   :background ,monokai-light-yellow-hc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow-lc
                                        :background ,monokai-light-256-yellow-hc))))

   `(hi-pink
     ((,monokai-light-class (:foreground ,monokai-light-magenta-lc
                                   :background ,monokai-light-magenta-hc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta-lc
                                        :background ,monokai-light-256-magenta-hc))))

   `(hi-green
     ((,monokai-light-class (:foreground ,monokai-light-green-lc
                                   :background ,monokai-light-green-hc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green-lc
                                        :background ,monokai-light-256-green-hc))))

   `(hi-blue
     ((,monokai-light-class (:foreground ,monokai-light-blue-lc
                                   :background ,monokai-light-blue-hc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue-lc
                                        :background ,monokai-light-256-blue-hc))))

   `(hi-black-b
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,monokai-light-class (:foreground ,monokai-light-blue-lc
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,monokai-light-class (:foreground ,monokai-light-green-lc
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold))))

   `(hi-black-hb
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(highlight-changes-delete
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,monokai-light-class (:background ,monokai-light-gray))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray))))

   `(highlight-indentation-current-column-face
     ((,monokai-light-class (:background ,monokai-light-gray))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,monokai-light-class (:background ,monokai-light-highlight))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(hl-line-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-yellow
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(ido-incomplete-regexp
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold ))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground ,monokai-light-background
                                   :width condensed))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground ,monokai-light-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   ;; info
   `(info-header-xref
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :inherit bold
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(info-node
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :inherit bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(info-reference-item
     ((,monokai-light-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,monokai-light-256-class  (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(info-title-1
     ((,monokai-light-class (:height ,monokai-light-height-plus-4))
      (,monokai-light-256-class  (:height ,monokai-light-height-plus-4))))

   `(info-title-2
     ((,monokai-light-class (:height ,monokai-light-height-plus-3))
      (,monokai-light-256-class  (:height ,monokai-light-height-plus-3))))

   `(info-title-3
     ((,monokai-light-class (:height ,monokai-light-height-plus-2))
      (,monokai-light-256-class  (:height ,monokai-light-height-plus-2))))

   `(info-title-4
     ((,monokai-light-class (:height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:height ,monokai-light-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,monokai-light-class (:background ,monokai-light-gray :inherit bold))
      (,monokai-light-256-class  (:background ,monokai-light-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,monokai-light-class (:inherit bold))
      (,monokai-light-256-class  (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(swiper-line-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))))

   `(swiper-match-face-1
     ((,monokai-light-class (:background ,monokai-light-gray-d))))

   `(swiper-match-face-2
     ((,monokai-light-class (:background ,monokai-light-green))))

   `(swiper-match-face-3
     ((,monokai-light-class (:background ,monokai-light-orange))))

   `(swiper-match-face-4
     ((,monokai-light-class (:background ,monokai-light-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-red))))

   `(jabber-activity-personal-face
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-blue))))

   `(jabber-chat-error
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-red))))

   `(jabber-chat-prompt-foreign
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-red))))

   `(jabber-chat-prompt-local
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-blue))))

   `(jabber-chat-prompt-system
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-green))))

   `(jabber-chat-text-foreign
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(jabber-chat-text-local
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,monokai-light-class (:underline t
                                  :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:underline t
                                       :foreground ,monokai-light-256-green))))

   `(jabber-roster-user-away
     ((,monokai-light-class (:slant italic
                              :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:slant italic
                                   :foreground ,monokai-light-256-green))))

   `(jabber-roster-user-chatty
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-orange))))

   `(jabber-roster-user-dnd
     ((,monokai-light-class (:slant italic
                              :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:slant italic
                                   :foreground ,monokai-light-256-red))))

   `(jabber-roster-user-error
     ((,monokai-light-class (:weight light
                               :slant italic
                               :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:weight light
                                    :slant italic
                                    :foreground ,monokai-light-256-red))))

   `(jabber-roster-user-offline
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(jabber-roster-user-online
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-blue))))

   `(jabber-roster-user-xa
     ((,monokai-light-class (:slant italic
                              :foreground ,monokai-light-magenta))
      (,monokai-light-256-class  (:slant italic
                                   :foreground ,monokai-light-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(js2-external-variable
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(js2-function-call
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(js2-function-param
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(js2-instance-member
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(js2-jsdoc-tag
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(js2-jsdoc-type
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(js2-jsdoc-value
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(js2-magic-paren
     ((,monokai-light-class (:underline t))
      (,monokai-light-256-class  (:underline t))))

   `(js2-object-property
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(js2-private-function-call
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(js2-private-member
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(js2-warning
     ((,monokai-light-class (:underline ,monokai-light-orange))
      (,monokai-light-256-class  (:underline ,monokai-light-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,monokai-light-class (:inherit bold))
      (,monokai-light-256-class  (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,monokai-light-class (:foreground ,monokai-light-line-number
                                   :background ,monokai-light-fringe-bg
                                   :inherit default
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-line-number
                                        :background ,monokai-light-256-fringe-bg
                                        :inherit default
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,monokai-light-class (:foreground ,monokai-light-line-number
                                   :background ,monokai-light-highlight-line
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-line-number
                                        :background ,monokai-light-256-highlight-line
                                        :underline nil))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,monokai-light-class (:inherit dimonokai-light-red-directory))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-directory))))

   `(lusty-file-face
     ((,monokai-light-class nil)
      (,monokai-light-256-class  nil)))

   `(lusty-match-face
     ((,monokai-light-class (:inherit ido-first-match))
      (,monokai-light-256-class  (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-background))))

   `(magit-diff-added-highlight
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-highlight-line))))

   `(magit-diff-removed
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background))))

   `(magit-diff-removed-highlight
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-highlight-line))))

   `(magit-section-title
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :weight unspecified))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(magit-log-graph
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,monokai-light-class (:background ,monokai-light-red-hc
                                   :foreground ,monokai-light-red-lc
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-red-hc
                                        :foreground ,monokai-light-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,monokai-light-class (:background ,monokai-light-green-hc
                                   :foreground ,monokai-light-green-lc
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-hc
                                        :foreground ,monokai-light-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,monokai-light-class (:background ,monokai-light-blue-lc
                                   :foreground ,monokai-light-blue-hc
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue-lc
                                        :foreground ,monokai-light-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,monokai-light-class (:background ,monokai-light-red-lc
                                   :foreground ,monokai-light-red-hc
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-red-lc
                                        :foreground ,monokai-light-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,monokai-light-class (:background ,monokai-light-green-lc
                                   :foreground ,monokai-light-green-hc
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc
                                        :foreground ,monokai-light-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,monokai-light-class (:background ,monokai-light-yellow-lc
                                   :foreground ,monokai-light-yellow-hc
                                   :box 1))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc
                                        :foreground ,monokai-light-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(Man-underline
     ((,monokai-light-class (:foreground ,monokai-light-green :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(monky-diff-del
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(markdown-header-face-1
     ((,monokai-light-class (:inherit markdown-header-face
                                :height ,monokai-light-height-plus-4))
      (,monokai-light-256-class  (:inherit markdown-header-face
                                     :height ,monokai-light-height-plus-4))))

   `(markdown-header-face-2
     ((,monokai-light-class (:inherit markdown-header-face
                                :height ,monokai-light-height-plus-3))
      (,monokai-light-256-class  (:inherit markdown-header-face
                                     :height ,monokai-light-height-plus-3))))

   `(markdown-header-face-3
     ((,monokai-light-class (:inherit markdown-header-face
                                :height ,monokai-light-height-plus-2))
      (,monokai-light-256-class  (:inherit markdown-header-face
                                     :height ,monokai-light-height-plus-2))))

   `(markdown-header-face-4
     ((,monokai-light-class (:inherit markdown-header-face
                                :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:inherit markdown-header-face
                                     :height ,monokai-light-height-plus-1))))

   `(markdown-header-face-5
     ((,monokai-light-class (:inherit markdown-header-face))
      (,monokai-light-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,monokai-light-class (:inherit markdown-header-face))
      (,monokai-light-256-class  (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(message-header-name
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(message-header-other
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(message-mml
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(mew-face-header-from
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(mew-face-header-date
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-header-to
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(mew-face-header-key
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-header-private
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-header-important
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(mew-face-header-marginal
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(mew-face-header-xmew
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-header-xmew-bad
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(mew-face-body-url
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(mew-face-body-comment
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-body-cite2
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(mew-face-body-cite3
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(mew-face-body-cite4
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(mew-face-body-cite5
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(mew-face-mark-review
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(mew-face-mark-escape
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-mark-delete
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(mew-face-mark-unlink
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(mew-face-mark-refile
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-mark-unread
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(mew-face-eof-message
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(mew-face-eof-part
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(mingus-pausing-face
     ((,monokai-light-class (:foreground ,monokai-light-magenta))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta))))

   `(mingus-playing-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(mingus-playlist-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan ))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan ))))

   `(mingus-song-file-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(mingus-stopped-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,monokai-light-class (:background ,monokai-light-violet-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,monokai-light-class (:background ,monokai-light-orange-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,monokai-light-class (:background ,monokai-light-cyan-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,monokai-light-class (:background ,monokai-light-blue-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue-d))))

   `(mmm-output-submode-face
     ((,monokai-light-class (:background ,monokai-light-red-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-red-d))))

   `(mmm-special-submode-face
     ((,monokai-light-class (:background ,monokai-light-green-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-d))))

   `(mmm-code-submode-face
     ((,monokai-light-class (:background ,monokai-light-gray))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray))))

   `(mmm-default-submode-face
     ((,monokai-light-class (:background ,monokai-light-gray-d))
      (,monokai-light-256-class  (:background ,monokai-light-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,monokai-light-class (:underline t))
      (,monokai-light-256-class  (:underline t))))

   `(moccur-edit-done-face
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background ,monokai-light-background
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground ,monokai-light-256-background))))

   `(moccur-edit-file-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(moccur-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :slant italic
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,monokai-light-class (:foreground ,monokai-light-magenta
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :slant normal
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,monokai-light-class (:inherit unspecified
                                :foreground unspecified
                                :background ,monokai-light-highlight-line
                                :underline ,monokai-light-emphasis
                                :weight normal))
      (,monokai-light-256-class  (:inherit unspecified
                                     :foreground unspecified
                                     :background ,monokai-light-256-highlight-line
                                     :underline ,monokai-light-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,monokai-light-class (:inherit font-lock-string-face))
      (,monokai-light-256-class  (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,monokai-light-class (:inherit font-lock-comment-face))
      (,monokai-light-256-class  (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,monokai-light-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,monokai-light-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,monokai-light-class (:inherit default))
      (,monokai-light-256-class  (:inherit default))))

   `(mu4e-header-marks-face
     ((,monokai-light-class (:inherit font-lock-preprocessor-face))
      (,monokai-light-256-class  (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,monokai-light-class (:inherit font-lock-type-face))
      (,monokai-light-256-class  (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,monokai-light-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,monokai-light-256-class  (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,monokai-light-class (:inherit font-lock-comment-face
                                :slant italic))
      (,monokai-light-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,monokai-light-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,monokai-light-256-class  (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,monokai-light-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,monokai-light-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,monokai-light-class (:inherit font-lock-comment-face
                                :slant italic))
      (,monokai-light-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,monokai-light-class (:inherit font-lock-type-face
                                :weight bold))
      (,monokai-light-256-class  (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,monokai-light-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,monokai-light-256-class  (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,monokai-light-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,monokai-light-256-class  (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,monokai-light-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,monokai-light-256-class  (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,monokai-light-class (:inherit message-header-name
                                :weight normal))
      (,monokai-light-256-class  (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :weight normal
                                   :slant normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,monokai-light-class (:inherit link))
      (,monokai-light-256-class  (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight normal
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(nav-face-button-num
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(nav-face-dir
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(nav-face-hdir
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(nav-face-file
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(nav-face-hfile
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background))))

   `(neo-root-dir-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-background))))

   `(neo-dir-link-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-background))))

   `(neo-file-link-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(neo-button-face
     ((,monokai-light-class (:underline nil))
      (,monokai-light-256-class  (:underline nil))))

   `(neo-expand-btn-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(neo-vc-default-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(neo-vc-user-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(neo-vc-edited-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(neo-vc-needs-update-face
     ((,monokai-light-class (:underline t))
      (,monokai-light-256-class  (:underline t))))

   `(neo-vc-needs-merge-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-comments))))

   `(neo-vc-added-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(neo-vc-removed-face
     ((,monokai-light-class (:strike-through t))
      (,monokai-light-256-class  (:strike-through t))))

   `(neo-vc-conflict-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(neo-vc-missing-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(neo-vc-ignored-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,monokai-light-class (:foreground ,monokai-light-gray-l))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-gray-l))))

   `(markup-table-face
     ((,monokai-light-class (:foreground ,monokai-light-blue-hc
                                   :background ,monokai-light-blue-lc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue-hc
                                        :background ,monokai-light-256-blue-lc))))

   `(markup-verbatim-face
     ((,monokai-light-class (:background ,monokai-light-orange-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-orange-lc))))

   `(markup-list-face
     ((,monokai-light-class (:foreground ,monokai-light-violet-hc
                                   :background ,monokai-light-violet-lc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet-hc
                                        :background ,monokai-light-256-violet-lc))))

   `(markup-replacement-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(markup-complex-replacement-face
     ((,monokai-light-class (:foreground ,monokai-light-violet-hc
                                   :background ,monokai-light-violet-lc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet-hc
                                        :background ,monokai-light-256-violet-lc))))

   `(markup-gen-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(markup-secondary-text-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,monokai-light-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,monokai-light-background)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,monokai-light-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,monokai-light-256-background)))))

   `(org-agenda-calendar-event
     ((,monokai-light-class (:foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,monokai-light-background)))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,monokai-light-256-background)))) t)

   `(org-agenda-date-weekend
     ((,monokai-light-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,monokai-light-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,monokai-light-256-class  (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,monokai-light-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,monokai-light-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,monokai-light-blue
                                :background ,monokai-light-background))
      (,monokai-light-256-class  (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,monokai-light-256-blue
                                     :background ,monokai-light-256-background))) t)

   `(org-agenda-done
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :weight normal))))

   `(org-block
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background "#f5efda"))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-highlight-alt))))

   `(org-block-background
     ((,monokai-light-class (:background ,monokai-light-highlight-alt))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-alt))))

   `(org-block-begin-line
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background "#e9e4d2"
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background "#e9e4d2"
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-foreground
                                   :box (:line-width 1 :style released-button)))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(org-date
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :underline t))))

   `(org-done
     ((,monokai-light-class (:weight bold
                               :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:weight bold
                                    :foreground ,monokai-light-256-green))))

   `(org-ellipsis
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(org-formula
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(org-headline-done
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(org-hide
     ((,monokai-light-class (:foreground ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background))))

   `(org-level-1
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :height ,monokai-light-height-plus-4
                                :foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :height ,monokai-light-height-plus-4
                                     :foreground ,monokai-light-256-orange))))

   `(org-level-2
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :height ,monokai-light-height-plus-3
                                :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :height ,monokai-light-height-plus-3
                                     :foreground ,monokai-light-256-green))))

   `(org-level-3
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :height ,monokai-light-height-plus-2
                                :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :height ,monokai-light-height-plus-2
                                     :foreground ,monokai-light-256-blue))))

   `(org-level-4
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :height ,monokai-light-height-plus-1
                                :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :height ,monokai-light-height-plus-1
                                     :foreground ,monokai-light-256-yellow))))

   `(org-level-5
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-cyan))))

   `(org-level-6
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-green))))

   `(org-level-7
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-red))))

   `(org-level-8
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-blue))))

   `(org-link
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(org-scheduled
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(org-scheduled-previously
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(org-scheduled-today
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :weight bold))))

   `(org-table
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(org-tag
     ((,monokai-light-class (:weight bold))
      (,monokai-light-256-class  (:weight bold))))

   `(org-time-grid
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(org-todo
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold)))
     ((,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight normal
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,monokai-light-class (:foreground ,monokai-light-orange
                                   :weight normal
                                   :underline nil))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,monokai-light-class (:background ,monokai-light-blue-lc
                                   :foreground ,monokai-light-blue-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue-lc
                                        :foreground ,monokai-light-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,monokai-light-class (:background ,monokai-light-blue-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue-lc))))

   `(org-habit-ready-face
     ((,monokai-light-class (:background ,monokai-light-green-lc
                                   :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc
                                        :foreground ,monokai-light-256-green))))

   `(org-habit-ready-future-face
     ((,monokai-light-class (:background ,monokai-light-green-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc))))

   `(org-habit-alert-face
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground ,monokai-light-yellow-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground ,monokai-light-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,monokai-light-class (:background ,monokai-light-yellow-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground ,monokai-light-red-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground ,monokai-light-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,monokai-light-class (:background ,monokai-light-red-lc))
      (,monokai-light-256-class  (:background ,monokai-light-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(org-agenda-restriction-lock
     ((,monokai-light-class (:background ,monokai-light-yellow))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow))))

   `(org-clock-overlay
     ((,monokai-light-class (:background ,monokai-light-yellow))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow))))

   `(org-column
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :underline t
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(org-document-title
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :weight bold
                                   :height ,monokai-light-height-plus-4))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :weight bold
                                        :height ,monokai-light-height-plus-4))))

   `(org-drawer
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(org-footnote
     ((,monokai-light-class (:foreground ,monokai-light-magenta
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(org-mode-line-clock-overrun
     ((,monokai-light-class (:inherit mode-line))
      (,monokai-light-256-class  (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,monokai-light-class (:inherit org-level-1))
      (,monokai-light-256-class  (:inherit org-level-1))))

   `(outline-2
     ((,monokai-light-class (:inherit org-level-2))
      (,monokai-light-256-class  (:inherit org-level-2))))

   `(outline-3
     ((,monokai-light-class (:inherit org-level-3))
      (,monokai-light-256-class  (:inherit org-level-3))))

   `(outline-4
     ((,monokai-light-class (:inherit org-level-4))
      (,monokai-light-256-class  (:inherit org-level-4))))

   `(outline-5
     ((,monokai-light-class (:inherit org-level-5))
      (,monokai-light-256-class  (:inherit org-level-5))))

   `(outline-6
     ((,monokai-light-class (:inherit org-level-6))
      (,monokai-light-256-class  (:inherit org-level-6))))

   `(outline-7
     ((,monokai-light-class (:inherit org-level-7))
      (,monokai-light-256-class  (:inherit org-level-7))))

   `(outline-8
     ((,monokai-light-class (:inherit org-level-8))
      (,monokai-light-256-class  (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,monokai-light-256-class  (:foreground ,monokai-light-comments))))

   ;; perspective
   `(persp-selected-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight normal))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground))))

   `(popup-isearch-match
     ((,monokai-light-class (:background ,monokai-light-green))
      (,monokai-light-256-class  (:background ,monokai-light-256-green))))

   `(popup-menu-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground))))

   `(popup-menu-mouse-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-foreground))))

   `(popup-menu-selection-face
     ((,monokai-light-class (:background ,monokai-light-magenta
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-magenta
                                        :foreground ,monokai-light-256-background))))

   `(popup-scroll-bar-background-face
     ((,monokai-light-class (:background ,monokai-light-comments))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,monokai-light-class (:background ,monokai-light-emphasis))
      (,monokai-light-256-class  (:background ,monokai-light-256-emphasis))))

   `(popup-tip-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :background ,monokai-light-background
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :background ,monokai-light-256-background
                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,monokai-light-class (:foreground ,monokai-light-green-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green-d))))

   `(realgud-overlay-arrow2
     ((,monokai-light-class (:foreground ,monokai-light-yellow-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow-d))))

   `(realgud-overlay-arrow3
     ((,monokai-light-class (:foreground ,monokai-light-orange-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange-d))))

   `(realgud-bp-enabled-face
     ((,monokai-light-class (:inherit error)))
     ((,monokai-light-256-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,monokai-light-class (:inherit secondary-selection)))
     ((,monokai-light-256-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,monokai-light-class (:foreground ,monokai-light-red-d)))
     ((,monokai-light-256-class (:foreground ,monokai-light-256-red-d))))

   `(realgud-bp-line-disabled-face
     ((,monokai-light-class (:inherit secondary-selection)))
     ((,monokai-light-256-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,monokai-light-class (:inerhit monokai-light-line-number)))
     ((,monokai-light-256-class (:inerhit monokai-light-line-number))))

   `(realgud-backtrace-number
     ((,monokai-light-class (:foreground ,monokai-light-yellow-d
                                   :weight bold)))
     ((,monokai-light-256-class (:foreground ,monokai-light-256-yellow
                                       :weight bold))))

   ;; rhtm-mode
   `(erb-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background))))

   `(erb-delim-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :background ,monokai-light-256-background))))

   `(erb-exec-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background))))

   `(erb-exec-delim-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :background ,monokai-light-256-background))))

   `(erb-out-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background))))

   `(erb-out-delim-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :background ,monokai-light-256-background))))

   `(erb-comment-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background))))

   `(erb-comment-delim-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :background ,monokai-light-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground ,monokai-light-256-background))))

   `(rst-level-2-face
     ((,monokai-light-class (:background ,monokai-light-cyan
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-cyan
                                        :foreground ,monokai-light-256-background))))

   `(rst-level-3-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background))))

   `(rst-level-4-face
     ((,monokai-light-class (:background ,monokai-light-violet
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-violet
                                        :foreground ,monokai-light-256-background))))

   `(rst-level-5-face
     ((,monokai-light-class (:background ,monokai-light-magenta
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-magenta
                                        :foreground ,monokai-light-256-background))))

   `(rst-level-6-face
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground ,monokai-light-background))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground ,monokai-light-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(rpm-spec-doc-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(rpm-spec-ghost-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(rpm-spec-macro-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(rpm-spec-package-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(rpm-spec-section-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(rpm-spec-tag-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(rpm-spec-var-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,monokai-light-class (:foreground ,monokai-light-violet
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,monokai-light-class (:inherit highlight))
      (,monokai-light-256-class  (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-background
                                   :weight normal
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-comments))))

   `(speedbar-directory-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-blue))))

   `(speedbar-file-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-foreground))))

   `(speedbar-highlight-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :background ,monokai-light-256-highlight-line))))

   `(speedbar-selected-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-yellow
                                :underline t))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :background ,monokai-light-blue
                                :foreground ,monokai-light-background
                                :overline ,monokai-light-cyan-lc))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :background ,monokai-light-256-blue
                                     :foreground ,monokai-light-256-background
                                     :overline ,monokai-light-256-cyan-lc))))

   `(speedbar-tag-face
     ((,monokai-light-class (:inherit ,monokai-light-pitch
                                :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:inherit ,monokai-light-pitch
                                     :foreground ,monokai-light-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,monokai-light-class (:background ,monokai-light-blue
                                   :foreground ,monokai-light-background
                                   :height ,monokai-light-height-plus-1
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-blue
                                        :foreground ,monokai-light-256-background
                                        :height ,monokai-light-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,monokai-light-class (:background ,monokai-light-yellow
                                   :foreground ,monokai-light-background
                                   :weight bold
                                   :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:background ,monokai-light-256-yellow
                                        :foreground ,monokai-light-256-background
                                        :weight bold
                                        :height ,monokai-light-height-plus-1))))

   `(sr-highlight-path-face
     ((,monokai-light-class (:background ,monokai-light-green
                                   :foreground ,monokai-light-background
                                   :weight bold
                                   :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:background ,monokai-light-256-green
                                        :foreground ,monokai-light-256-background
                                        :weight bold
                                        :height ,monokai-light-height-plus-1))))

   `(sr-passive-path-face
     ((,monokai-light-class (:background ,monokai-light-comments
                                   :foreground ,monokai-light-background
                                   :weight bold
                                   :height ,monokai-light-height-plus-1))
      (,monokai-light-256-class  (:background ,monokai-light-256-comments
                                        :foreground ,monokai-light-256-background
                                        :weight bold
                                        :height ,monokai-light-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,monokai-light-class (:inherit dimonokai-light-red-marked))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-marked))))

   `(sr-marked-file-face
     ((,monokai-light-class (:inherit dimonokai-light-red-marked))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-marked))))

   `(sr-alt-marked-dir-face
     ((,monokai-light-class (:background ,monokai-light-magenta
                                   :foreground ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-magenta
                                        :foreground ,monokai-light-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,monokai-light-class (:background ,monokai-light-magenta
                                   :foreground ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-magenta
                                        :foreground ,monokai-light-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,monokai-light-class (:inherit dimonokai-light-red-directory
                                :weight normal))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,monokai-light-class (:inherit dimonokai-light-red-directory
                                :slant italic
                                :weight normal))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,monokai-light-class (:inherit dimonokai-light-red-symlink
                                :slant italic
                                :weight normal))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,monokai-light-class (:inherit dimonokai-light-red-warning
                                :slant italic
                                :weight normal))
      (,monokai-light-256-class  (:inherit dimonokai-light-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(sr-encrypted-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(sr-log-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(sr-packaged-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(sr-html-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(sr-xml-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,monokai-light-class (:background ,monokai-light-red
                                   :foreground ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:background ,monokai-light-256-red
                                        :foreground ,monokai-light-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-yellow))))

   `(syslog-hour-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-green))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-green))))

   `(syslog-error-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-orange
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-blue
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-cyan
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-magenta))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-magenta))))

   ;; table
   `(table-cell
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :background ,monokai-light-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,monokai-light-class (:foreground ,monokai-light-background
                                   :background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-background
                                        :background ,monokai-light-256-highlight-line))))

   `(term-color-red
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :background ,monokai-light-red-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :background ,monokai-light-256-red-d))))

   `(term-color-green
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :background ,monokai-light-green-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :background ,monokai-light-256-green-d))))

   `(term-color-yellow
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :background ,monokai-light-yellow-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :background ,monokai-light-256-yellow-d))))

   `(term-color-blue
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-blue-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-blue-d))))

   `(term-color-magenta
     ((,monokai-light-class (:foreground ,monokai-light-magenta
                                   :background ,monokai-light-magenta-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta
                                        :background ,monokai-light-256-magenta-d))))

   `(term-color-cyan
     ((,monokai-light-class (:foreground ,monokai-light-cyan
                                   :background ,monokai-light-cyan-d))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan
                                        :background ,monokai-light-256-cyan-d))))

   `(term-color-white
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-foreground))))

   `(term-default-fg-color
     ((,monokai-light-class (:inherit term-color-white))
      (,monokai-light-256-class  (:inherit term-color-white))))

   `(term-default-bg-color
     ((,monokai-light-class (:inherit term-color-black))
      (,monokai-light-256-class  (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,monokai-light-class (:background ,monokai-light-yellow-hc
                                   :foreground ,monokai-light-background
                                   :inherit ,monokai-light-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,monokai-light-class (:foreground ,monokai-light-magenta
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :background ,monokai-light-highlight-line
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :background ,monokai-light-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :background ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :background ,monokai-light-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,monokai-light-class (:foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background ,monokai-light-background))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(undo-tree-visualizer-current-face
     ((,monokai-light-class (:foreground ,monokai-light-blue
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :background ,monokai-light-background
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :background ,monokai-light-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,monokai-light-class (:background ,monokai-light-green-lc
                                   :foreground ,monokai-light-green-hc))
      (,monokai-light-256-class  (:background ,monokai-light-256-green-lc
                                        :foreground ,monokai-light-256-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,monokai-light-class (:inherit link))
      (,monokai-light-256-class  (:inherit link))))

   `(w3m-arrived-anchor
     ((,monokai-light-class (:inherit link-visited))
      (,monokai-light-256-class  (:inherit link-visited))))

   `(w3m-form
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-foreground))))

   `(w3m-header-line-location-title
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-yellow))))

   `(w3m-header-line-location-content

     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground))))

   `(w3m-bold
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-cyan
                                   :inherit link))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-cyan))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,monokai-light-class (:foreground ,monokai-light-emphasis))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis))))

   `(w3m-lnum-match
     ((,monokai-light-class (:background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line))))

   `(w3m-lnum
     ((,monokai-light-class (:underline nil
                                  :bold nil
                                  :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:underline nil
                                       :bold nil
                                       :foreground ,monokai-light-256-red))))

   `(w3m-session-select
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(w3m-session-selected
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :bold t
                                   :underline t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-foreground))))

   `(w3m-tab-selected-background
     ((,monokai-light-class (:background ,monokai-light-background
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-background
                                        :foreground ,monokai-light-256-foreground))))

   `(w3m-tab-mouse
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-yellow))))

   `(w3m-tab-selected
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-emphasis
                                   :bold t))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-red))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :foreground ,monokai-light-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(web-mode-comment-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(web-mode-constant-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,monokai-light-class (:underline unspecified
                                  :weight unspecified
                                  :background ,monokai-light-highlight-line))
      (,monokai-light-256-class  (:underline unspecified
                                       :weight unspecified
                                       :background ,monokai-light-256-highlight-line))))

   `(web-mode-doctype-face
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :slant italic
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,monokai-light-class (:underline t))
      (,monokai-light-256-class  (:underline t))))

   `(web-mode-function-name-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(web-mode-html-attr-name-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,monokai-light-class (:inherit web-mode-html-attr-name-face))
      (,monokai-light-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,monokai-light-class (:inherit web-mode-block-delimiter-face))
      (,monokai-light-256-class  (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,monokai-light-class (:inherit web-mode-html-attr-name-face))
      (,monokai-light-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(web-mode-html-tag-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(web-mode-keyword-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(web-mode-preprocessor-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow
                                   :slant normal
                                   :weight unspecified))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(web-mode-type-face
     ((,monokai-light-class (:inherit font-lock-type-face))
      (,monokai-light-256-class  (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(web-mode-warning-face
     ((,monokai-light-class (:inherit font-lock-warning-face))
      (,monokai-light-256-class  (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,monokai-light-class (:background unspecified))
      (,monokai-light-256-class  (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,monokai-light-class (:inherit font-lock-preprocessor-face))
      (,monokai-light-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,monokai-light-class (:inherit web-mode-comment-face))
      (,monokai-light-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,monokai-light-class (:inherit font-lock-preprocessor-face))
      (,monokai-light-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,monokai-light-class (:inherit web-mode-string-face))
      (,monokai-light-256-class  (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,monokai-light-class (:box 1 :weight bold))
      (,monokai-light-256-class  (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,monokai-light-class (:inherit font-lock-constant-face))
      (,monokai-light-256-class  (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,monokai-light-class (:inherit font-lock-builtin-face))
      (,monokai-light-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,monokai-light-class (:inherit font-lock-builtin-face))
      (,monokai-light-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,monokai-light-class (:inherit font-lock-function-name-face))
      (,monokai-light-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,monokai-light-class (:inherit font-lock-builtin-face))
      (,monokai-light-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,monokai-light-class (:inherit font-lock-function-name-face))
      (,monokai-light-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,monokai-light-class (:inherit font-lock-builtin-face))
      (,monokai-light-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,monokai-light-class (:inherit font-lock-variable-name-face))
      (,monokai-light-256-class  (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,monokai-light-class (:inherit font-lock-keyword-face))
      (,monokai-light-256-class  (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,monokai-light-class (:inherit web-mode-string-face))
      (,monokai-light-256-class  (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,monokai-light-class (:inherit web-mode-string-face))
      (,monokai-light-256-class  (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,monokai-light-class (:inherit web-mode-comment-face))
      (,monokai-light-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(web-mode-json-key-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(web-mode-json-string-face
     ((,monokai-light-class (:inherit web-mode-string-face))
      (,monokai-light-256-class  (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(web-mode-part-comment-face
     ((,monokai-light-class (:inherit web-mode-comment-face))
      (,monokai-light-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,monokai-light-class (:inherit web-mode-block-face))
      (,monokai-light-256-class  (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,monokai-light-class (:inherit web-mode-string-face))
      (,monokai-light-256-class  (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,monokai-light-class (:foreground ,monokai-light-violet))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-violet))))

   `(web-mode-whitespace-face
     ((,monokai-light-class (:background ,monokai-light-red))
      (,monokai-light-256-class  (:background ,monokai-light-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-emphasis
                                   :inverse-video unspecified))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,monokai-light-class(:background unspecified
                                  :foreground ,monokai-light-comments
                                  :inverse-video unspecified))
      (,monokai-light-256-class (:background unspecified
                                       :foreground ,monokai-light-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-orange-lc
                                   :inverse-video t))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-magenta
                                   :inverse-video unspecified))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,monokai-light-class (:background ,monokai-light-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,monokai-light-256-class  (:background ,monokai-light-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-red-lc
                                   :inverse-video t))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,monokai-light-class (:background unspecified
                                   :foreground ,monokai-light-orange
                                   :inverse-video t
                                   :weight bold))
      (,monokai-light-256-class  (:background unspecified
                                        :foreground ,monokai-light-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(wl-highlight-folder-many-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(wl-highlight-folder-path-face
     ((,monokai-light-class (:foreground ,monokai-light-orange))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(wl-highlight-message-citation-header
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(wl-highlight-message-headers-face
     ((,monokai-light-class (:foreground ,monokai-light-red))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(wl-highlight-message-header-contents
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(wl-highlight-message-signature
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(wl-highlight-summary-answemonokai-light-red-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground
                                   :slant italic))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,monokai-light-class (:foreground ,monokai-light-blue))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,monokai-light-class (:foreground ,monokai-light-magenta))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,monokai-light-class (:underline t
                                  :weight bold))
      (,monokai-light-256-class  (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,monokai-light-class (:inherit error))
      (,monokai-light-256-class  (:inherit error))))

   `(weechat-highlight-face
     ((,monokai-light-class (:foreground ,monokai-light-emphasis
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight unspecified
                                   :inverse-video t))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,monokai-light-class (:inherit minibuffer-prompt))
      (,monokai-light-256-class  (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,monokai-light-class (:foreground ,monokai-light-green
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(which-key-note-face
     ((,monokai-light-class (:foreground ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments))))

   `(which-key-command-description-face
     ((,monokai-light-class (:foreground ,monokai-light-foreground))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-foreground))))

   `(which-key-local-map-description-face
     ((,monokai-light-class (:foreground ,monokai-light-yellow-hc))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-yellow-hc))))

   `(which-key-group-description-face
     ((,monokai-light-class (:foreground ,monokai-light-red
                                   :weight bold))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,monokai-light-class (:foreground ,monokai-light-green))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-comments))))

   `(yascroll:thumb-fringe
     ((,monokai-light-class (:foreground ,monokai-light-comments
                                   :background ,monokai-light-comments))
      (,monokai-light-256-class  (:foreground ,monokai-light-256-comments
                                        :background ,monokai-light-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,monokai-light-class (:background ,monokai-light-highlight-line
                                   :box ,monokai-light-emphasis))
      (,monokai-light-256-class  (:background ,monokai-light-256-highlight-line
                                        :box ,monokai-light-256-emphasis)))))

  (custom-theme-set-variables
   'monokai-light
   `(ansi-color-names-vector [,monokai-light-background ,monokai-light-red ,monokai-light-green ,monokai-light-yellow
                                                  ,monokai-light-blue ,monokai-light-magenta ,monokai-light-cyan ,monokai-light-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,monokai-light-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,monokai-light-magenta ,monokai-light-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,monokai-light-highlight-line . 0)
       (,monokai-light-green-lc . 20)
       (,monokai-light-cyan-lc . 30)
       (,monokai-light-blue-lc . 50)
       (,monokai-light-yellow-lc . 60)
       (,monokai-light-orange-lc . 70)
       (,monokai-light-magenta-lc . 85)
       (,monokai-light-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,monokai-light-background)
   `(pos-tip-background-color ,monokai-light-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,monokai-light-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,monokai-light-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,monokai-light-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,monokai-light-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,monokai-light-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,monokai-light-background ,monokai-light-highlight-line
                  ,monokai-light-red-d ,monokai-light-red
                  ,monokai-light-green-d ,monokai-light-green
                  ,monokai-light-yellow-d ,monokai-light-yellow
                  ,monokai-light-blue-d ,monokai-light-blue
                  ,monokai-light-magenta-d ,monokai-light-magenta
                  ,monokai-light-cyan-d ,monokai-light-cyan
                  ,monokai-light-foreground ,monokai-light-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monokai-light)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; monokai-light-theme.el ends here
