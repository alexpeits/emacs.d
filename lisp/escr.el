;;; escr.el --- Tool for taking a screen shot of the Emacs' frames, windows and
;;; regions.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/escr
;; Version: 0.1.0
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This tool allows to take a screen shot of the Emacs' frames, windows and regions.
;;
;; Usage:
;;
;; - Use `escr-frame-screenshot' command to take a screen shot of the current frame.
;;
;; - Use `escr-window-screenshot' command to take a screen shot of the current
;; window.
;;
;; - Use `escr-region-screenshot' command to take a screen shot of the current
;; region.
;;
;; Customization:
;;
;; Set `escr-exclude-fringes' to `t' if you want to exclude fringes from a screen
;; shot. This variable does work only for `escr-region-screenshot'.
;;
;; Set `escr-screenshot-width' to nil if you want to take a screen shot of current
;; window width. And set to integer value if you want limit screen shot to the number
;; of maximum column. This variable does work only for `escr-region-screenshot'.
;;
;; Set `escr-screenshot-directory' to any directory where you would like screen shots
;; to be stored. Default directory is "~/.emacs.d/screenshots/". If directory doesn't
;; exist you'll be prompted to create that directory.
;;
;; Change `escr-screenshot-format' to the desired filename format. Default is
;; `%Y-%m-%d-%H%M%S.png' (Read `format-time-string' documentation for details).
;;
;; Set `escr-screenshot-quality' to desired image quality. Default is 100 (maximum
;; image quality).

;; Installation:

;; Assuming that the file `escr.el' is somewhere on the load path, add the following
;; lines to your `.emacs' file:

;; (require 'escr)
;; (global-set-key (kbd "C-c C-e r") 'escr-region-screenshot)
;; (global-set-key (kbd "C-c C-e f") 'escr-frame-screenshot)
;; (global-set-key (kbd "C-c C-e w") 'escr-window-screenshot)
;;
;; Change the key bindings to your liking.
;;

;;; Code:

(defvar escr-exclude-fringes t
  "Exclude fringes from the screenshot.  It does work only for
`escr-region-screenshot'.")

(defvar escr-screenshot-directory (concat
                                   user-emacs-directory
                                   "screenshots")
  "Path to directory where screenshots are going to be stored.")

(defvar escr-filename-format "%Y-%m-%d-%H%M%S.png"
  "Screenshots filename format.")

(defvar escr-screenshot-width 80
  "Screenshot width.  It does work only for `escr-region-screenshot'.

If nil, then screenshot will be of current window width.
If integer, then that value means how many columns screenshot will contain.")

(defvar escr-screenshot-quality "100"
  "Screenshot image quality.")



(defun escr-region-screenshot ()
  "Make screenshot from the current region.  Please note that it
is possible to make screenshot only from the text which is
visible on the screen."
  (interactive)
  (let ((window-id (frame-parameter (selected-frame) 'window-id))
        (char-height (frame-char-height))
        (char-width (frame-char-width))
        (filename (expand-file-name
                   (concat escr-screenshot-directory
                           "/"
                           (format-time-string escr-filename-format
                                               (current-time)))))
        (window-start-line nil)
        (window-region-beginning-line nil)
        (window-region-end-line nil)
        (screenshot-height 0)
        (screenshot-width 0)
        (screenshot-x (nth 0 (window-pixel-edges)))
        (screenshot-y (nth 1 (window-pixel-edges)))
        (selection-start (region-beginning))
        (selection-end (region-end))
        (current-point (point))
        (crop ""))

    (escr--check-directory)

    (setq window-start-line (line-number-at-pos (window-start)))

    (goto-char selection-start)
    (setq window-region-beginning-line (line-number-at-pos))

    (goto-char selection-end)
    (setq window-region-end-line (line-number-at-pos))

    (goto-char current-point)

    (if (integerp escr-screenshot-width)
        (setq screenshot-width (* char-width escr-screenshot-width))
      (setq screenshot-width (nth 2 (window-pixel-edges))))

    (when escr-exclude-fringes
      (setq screenshot-width (- screenshot-width
                                (nth 0 (window-fringes))))
      (when (null escr-screenshot-width)
        (setq screenshot-width (- screenshot-width
                                  (nth 1 (window-fringes))))))

    (setq screenshot-height (* (+ (- window-region-end-line
                                     window-region-beginning-line)
                                  1)
                               char-height))

    (when escr-exclude-fringes
      (setq screenshot-x (+ screenshot-x
                            (nth 0 (window-fringes)))))

    (setq screenshot-y (+ screenshot-y
                          (* (- window-region-beginning-line
                                window-start-line)
                             char-height)))

    (deactivate-mark t)
    (redisplay t)

    (escr--screenshot screenshot-x
                      screenshot-y
                      screenshot-width
                      screenshot-height)))

(defun escr-window-screenshot ()
  "Make screenshot from the current window."
  (interactive)
  (escr--check-directory)
  (escr--screenshot (nth 0 (window-pixel-edges))
                    (nth 1 (window-pixel-edges))
                    (nth 2 (window-pixel-edges))
                    (nth 3 (window-pixel-edges))))

(defun escr-frame-screenshot ()
  "Make screenshot from the current frame."
  (interactive)
  (escr--check-directory)
  (escr--screenshot 1 1 
                    (frame-pixel-width)
                    (frame-pixel-height)))

(defun escr--screenshot (screenshot-x screenshot-y
                                      screenshot-width
                                      screenshot-height)
  (let ((window-id (frame-parameter (selected-frame) 'window-id))
        (crop (format "%sx%s+%s+%s"
                      screenshot-width
                      screenshot-height
                      screenshot-x
                      screenshot-y))
        (filename (expand-file-name
                   (concat escr-screenshot-directory
                           "/"
                           (format-time-string escr-filename-format
                                               (current-time))))))

    (call-process "import" nil nil nil
                  "-window" window-id
                  "-crop" crop 
                  "-quality" escr-screenshot-quality
                  filename)))

(defun escr--check-directory ()
  (when (not (file-exists-p escr-screenshot-directory))
    (if (y-or-n-p (format "Directory %s does not exist. Create it?"
                          escr-screenshot-directory))
        (make-directory escr-screenshot-directory)
      (error "Please create the directory first."))))


(provide 'escr)

;;; escr.el ends here
