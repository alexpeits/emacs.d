;;;; ui
(require 'my-utils)

;; highlight numbers
(use-package highlight-numbers
  :ensure t
  :config
  (my/add-hooks '(prog-mode-hook css-mode-hook) (highlight-numbers-mode)))

;; visual effect after closing delimiter
(setq show-paren-delay 0.3)

;; show column in modeline
(setq column-number-mode t)

;; disable annoying stuff
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; linum
(use-package linum
  :config
  ;; (global-linum-mode t)
  ;; (add-hook 'prog-mode-hook (lambda () (linum-mode t)))
  ;; (setq linum-format "%4d ")
  (setq linum-format 'dynamic))

; font size & scaling
(setq text-scale-mode-step 1.05)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; highlight trailing whitespace
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; 80-column rule
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-line-column 79)
  (setq whitespace-style '(face lines-tail))
  (diminish 'whitespace-mode "")
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; theme
(setq spacemacs-theme-org-height nil
      spacemacs-theme-comment-bg nil)
(when window-system
   (setq solarized-use-variable-pitch nil
         solarized-height-plus-1 1.0
         solarized-height-plus-2 1.0
         solarized-height-plus-3 1.0
         solarized-height-plus-4 1.0
         solarizedarker-use-variable-pitch nil
         solarizedarker-height-plus-1 1.0
         solarizedarker-height-plus-2 1.0
         solarizedarker-height-plus-3 1.0
         solarizedarker-height-plus-4 1.0
         solarizedarkerbright-use-variable-pitch nil
         solarizedarkerbright-height-plus-1 1.0
         solarizedarkerbright-height-plus-2 1.0
         solarizedarkerbright-height-plus-3 1.0
         solarizedarkerbright-height-plus-4 1.0))

;; (use-package solarized-theme :ensure t :defer t)
;; (use-package zenburn-theme :ensure t :defer t)
;; (defvar zenburn-override-colors-alist '(("zenburn-bg" . "#3B3B3B")))
(defvar my/themes '((my/zenburn . ((theme . zenburn)
                                   (org-block-begin-end-bg . "#454545")
                                   (org-block-fg . "#dcdccc")
                                   (org-block-bg . "#3E3E3E")))
                    (my/solarized-dark . ((theme . solarized-dark)
                                          (org-block-begin-end-bg . "#073642")
                                          (org-block-fg . "#839496")
                                          (org-block-bg . "#002f3b")))
                    (my/solarized-light . ((theme . solarized-light)
                                           (org-block-begin-end-bg . "#eee8d5")
                                           (org-block-fg . "#657b83")
                                           (org-block-bg . "#f7f0dc")))
                    (my/solarized-black-bright . ((theme . solarized-black-bright)
                                                  (org-block-begin-end-bg . "#303030")
                                                  (org-block-fg . "#a1acae")
                                                  (org-block-bg . "#292929")))))

(defvar my/avail-themes
  '(
    my/solarized-black-bright
    ;; my/solarized-dark
    ;; my/solarized-light
    my/zenburn
    ))
(defvar my/current-theme 1)

(defun my/set-theme (&optional theme-name)
  (let* ((theme-name (if (null theme-name) (elt my/avail-themes my/current-theme) theme-name))
         (config (cdr (assoc theme-name my/themes)))
         (theme (cdr (assoc 'theme config)))
         (org-block-begin-end-bg (cdr (assoc 'org-block-begin-end-bg config)))
         (org-block-fg (cdr (assoc 'org-block-fg config)))
         (org-block-bg (cdr (assoc 'org-block-bg config))))
    ;; disable all currently enabled themes (otherwise faces get messed up)
    (mapc 'disable-theme custom-enabled-themes)
    (load-theme theme t)
    ;; set these faces for the specific theme
    (unless (some #'null '(org-block-begin-end-bg org-block-fg org-block-bg))
      (custom-theme-set-faces
       theme
       `(org-block ((t :background ,org-block-bg :foreground ,org-block-fg)))
       `(org-block-begin-line ((t :background ,org-block-begin-end-bg)))
       `(org-block-end-line ((t :background ,org-block-begin-end-bg)))))))

(defun my/toggle-theme ()
  (interactive)
  (let ((next-theme (mod (1+ my/current-theme) (length my/avail-themes))))
    (my/set-theme (elt my/avail-themes next-theme))
    (setq my/current-theme next-theme)))

(defun my/refresh-theme ()
  (interactive)
  (my/set-theme (elt my/avail-themes my/current-theme)))

;; fonts

(defvar my/avail-fonts
  '(
    "Menlo-12"
    "Ubuntu Mono-14"
    "Menlo-13"
    ))
(defvar my/current-font 0)

(defun my/set-font (&optional font)
  (let ((font (if (null font) (elt my/avail-fonts my/current-font) font)))
    (set-frame-font font)))

(defun my/toggle-font ()
  (interactive)
  (let ((next-font (mod (1+ my/current-font) (length my/avail-fonts))))
    (my/set-font (elt my/avail-fonts next-font))
    (setq my/current-font next-font)))

(provide 'my-ui)
