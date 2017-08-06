(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/custom-themes/"))
(setenv "PATH" (concat "/home/alex/.local/bin" path-separator (getenv "PATH")))
;; (setenv "PATH" (concat "/home/alex/.cabal/bin" path-separator (getenv "PATH")))
(add-to-list 'exec-path "/home/alex/.local/bin")
;; (add-to-list 'exec-path "/home/alex/.cabal/bin")
(setq user-full-name "Alex Peitsinis"
      user-mail-address "alexpeitsinis@gmail.com")

;; ----------------
;; Package management
;; ----------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("MELPA Stable" . "https://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
;; activate installed packages
(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(require 'myfuncs)
(require 'myengines)

(use-package cl :ensure t)

(defmacro my/add-hooks (hooks &rest body)
  `(dolist (hook ,hooks)
     (add-hook hook (lambda () ,@body))))

;; ----------------
;; various
;; ----------------

;; remember last position
(if (<= emacs-major-version 24)
    (use-package saveplace
       :ensure t
       :config
       (setq-default save-place t)
    )
  (save-place-mode 1))

;; undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode "")
  )

;; highlight numbers
(use-package highlight-numbers
  :ensure t
  :config
  (my/add-hooks '(prog-mode-hook css-mode-hook) (highlight-numbers-mode))
  )

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'nerd)

  (defun my/neotree-toggle-project ()
    "Open NeoTree using the git root."
    (interactive)
    (neotree-toggle)
    (if (and (neo-global--window-exists-p)
             (projectile-project-p))
        (let ((project-dir (projectile-project-root))
              (file-name (buffer-file-name)))
          (neotree-dir project-dir)
          (neotree-find file-name))))
  )

;; visual effect after closing delimiter
(setq show-paren-delay 0.3)

;; show column in modeline
(setq column-number-mode t)

;; use column width 80 to fill (e.g. with gq)
(setq-default fill-column 80)

;; store all backup and autosave files in
;; one dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; only with this set to nil can org-mode export & open too
(setq process-connection-type nil)

;; i love this
(defalias 'yes-or-no-p #'y-or-n-p)

;; always scroll to the end of compilation buffers
(setq compilation-scroll-output t)

;; vim-like scrolling (emacs=0)
(setq scroll-conservatively 101)

;; some keymaps
(global-set-key (kbd "M-o") 'other-window)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-p") 'find-function-at-point)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; flyspell on pure text buffers
(dolist (hook '(text-mode-hook change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; what it says
(defun my/revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
          (message "Refreshed open files"))


;; read file as string
(defun my/read-file-contents (path)
  (with-temp-buffer
    (insert-file-contents (expand-file-name path))
    (buffer-string)))


;; add env files to conf-mode alist
(add-to-list 'auto-mode-alist '(".env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("env.example\\'" . conf-mode))

;; add extension for restclient.el
(add-to-list 'auto-mode-alist '(".http\\'" . restclient-mode))

;; DocView
(setq doc-view-continuous t)

;; buffer-move
(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-s-h") 'buf-move-left)
  (global-set-key (kbd "C-s-j") 'buf-move-down)
  (global-set-key (kbd "C-s-k") 'buf-move-up)
  (global-set-key (kbd "C-s-l") 'buf-move-right))

;; eyebrowse
(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

;; ----------------
;; UI & themes
;; ----------------


(setq-default indent-tabs-mode nil)
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

(use-package zenburn-theme :ensure t)
(use-package solarized-theme :ensure t)
(defvar zenburn-override-colors-alist '(("zenburn-bg" . "#3B3B3B")))
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

(defvar my/avail-themes '(my/solarized-black-bright
                          my/solarized-dark
                          my/solarized-light
                          my/zenburn))
(defvar my/current-theme 3)

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
  (setq linum-format 'dynamic)
  )

;; always open helm buffers at bottom
(use-package popwin
  :ensure t
  :config
  (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
  (add-hook 'helm-after-initialize-hook (lambda ()
                                            (popwin:display-buffer helm-buffer t)
                                            (popwin-mode -1)))
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
  )

; font size & scaling
(setq text-scale-mode-step 1.05)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; highlight trailing whitespace
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(use-package smartparens
  :ensure t
  :defer t
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :init
  (setq sp-paredit-bindings t
        sp-show-pair-delay 0.2
        ;; fix paren highlighting in normal mode
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (my/add-hooks '(prog-mode-hook comint-mode-hook css-mode-hook) (smartparens-mode))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  ;; don't create a pair with single quote in minibuffer
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-pair "(" nil :post-handlers
	   '(:add (my/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "{" nil :post-handlers
	   '(:add (my/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
	   '(:add (my/smartparens-pair-newline-and-indent "RET")))
  (diminish 'smartparens-mode "")

  ;; keymaps
  (define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)

  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-up-sexp)
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (diminish 'which-key-mode "")
  )

(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "C-\\") #'imenu-list-minor-mode)
  (setq imenu-list-size 30)
  )

;; ----------------
;; term, comint & eshell
;; ----------------

(add-hook 'term-mode-hook (lambda ()
                            (linum-mode 0)
                            (define-key term-raw-map (kbd "M-o") 'other-window)))

;; automatically close term buffers on EOF
(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)))

(setq comint-prompt-read-only t)

(defun my/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map (kbd "C-l") 'my/comint-clear-buffer)))


;; ----------------
;; VCS
;; ----------------

(use-package magit
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

(if (display-graphic-p)
    (use-package diff-hl
      :ensure t
      :config
      (global-diff-hl-mode)
      (diff-hl-flydiff-mode)))


;; ----------------
;; evil
;; ----------------
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  )

(use-package evil
  :ensure t
  :config
  (setq evil-want-C-i-jump nil)
  ;; (setq evil-move-cursor-back nil)  ;; works better with lisp navigation
  (evil-mode 1)

  (defun my/make-emacs-mode (mode)
    "Make `mode' use emacs keybindings."
    (delete mode evil-insert-state-modes)
    (add-to-list 'evil-emacs-state-modes mode))

  ;; emacs mode is default in some modes
  (dolist (mode '(term-mode
                  eshell-mode
                  haskell-error-mode
                  haskell-interactive-mode))
    (my/make-emacs-mode mode))

  ;; don't need C-n, C-p
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  ;; magit
  (evil-define-key 'normal magit-blame-mode-map (kbd "q") 'magit-blame-quit)

  ;; neotree
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "|") 'neotree-enter-vertical-split)
  (evil-define-key 'normal neotree-mode-map (kbd "-") 'neotree-enter-horizontal-split)

  ;; move state to beginning of modeline
  (setq evil-mode-line-format '(before . mode-line-front-space))

  ;; this is needed to be able to use C-h
  (global-set-key (kbd "C-h") 'undefined)
  (define-key evil-emacs-state-map (kbd "C-h") 'help)
  (define-key evil-insert-state-map (kbd "C-k") nil)

  (define-key evil-normal-state-map (kbd "M-.") nil)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-visual-state-map (kbd ";") 'evil-ex)
  (evil-ex-define-cmd "sv" 'split-window-below)

  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)

  (define-key evil-insert-state-map (kbd "C-M-i") 'company-complete)

  (define-key evil-visual-state-map (kbd "<") '(lambda ()
                 (interactive)
                 (progn
                     (call-interactively 'evil-shift-left)
                     (execute-kbd-macro "gv"))))

  (define-key evil-visual-state-map (kbd ">") '(lambda ()
                 (interactive)
                 (progn
                     (call-interactively 'evil-shift-right)
                     (execute-kbd-macro "gv"))))

  ;; evilnc toggles instead of commenting/uncommenting
  (setq evilnc-invert-comment-line-by-line t)

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "]"  'find-tag-other-window
    ";"  'evilnc-comment-or-uncomment-lines
    ")"  'my/fix-theme
    "h"  'help

    "n"  'my/neotree-toggle-project

    "el" 'my/toggle-flycheck-error-list

    "um" 'menu-bar-mode
    "up" 'rainbow-delimiters-mode
    "uh" 'rainbow-mode

    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bb" 'helm-buffers-list
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit

    "tb" 'my/toggle-bg
    "tg" 'global-diff-hl-mode
    "th" 'global-hl-line-mode
    "tl"  'linum-mode
    "ts" 'flycheck-mode
    "tw" 'toggle-truncate-lines
    "tt" 'my/toggle-theme
    "tj" 'my/toggle-jsmodes

    "j"  'my/jump-to-definition

    "pp" 'projectile-persp-switch-project
    "pl" 'persp-next
    "ph" 'persp-prev
    "pr" 'persp-rename
    "pq" 'persp-kill

    "ss" 'swiper

    "Ts" 'helm-themes
    "ff" 'helm-find
    "fa" 'helm-ag
    )
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  )


;; ----------------
;; python
;; ----------------
(defun my/projectile-test-python-project (verbose)
  "Tests a python project (requires it to be a project and
tests to exist in `project_root/tests`"
  (interactive "P")
  (let* ((root (projectile-project-root))
         (testdir (concat (file-name-as-directory root) "tests"))
         (buff (get-buffer-create "*python-test*"))
         (verbosity (cond ((null verbose) "")
                          (t "-v "))))
    (display-buffer buff)
    (projectile-with-default-dir root
      (shell-command (concat "python -m unittest discover " verbosity testdir) buff)
      (let ((compilation-window-height 10))
        (with-current-buffer buff
          (compilation-mode))))))

(use-package pyvenv) ;; this has to be downloaded

(setq python-shell-interpreter "ipython")

(defun eshell/workon (arg) (pyvenv-workon arg))
(defun eshell/deactivate () (pyvenv-deactivate))

(setq python-shell-prompt-detect-failure-warning nil)
(my|define-jump-handlers python-mode)
(my|define-jump-handlers cython-mode anaconda-mode-goto)
(my/make-emacs-mode 'inferior-python-mode)
(my/make-emacs-mode 'anaconda-mode-view-mode)
(add-hook 'python-mode-hook (lambda ()
                              (anaconda-mode)
                              (diminish 'anaconda-mode " An")
                              (anaconda-eldoc-mode)
                              (diminish 'anaconda-eldoc-mode "")
                              (evil-leader/set-key "ct" 'my/projectile-test-python-project)
                              (add-to-list 'my-jump-handlers-python-mode
					   '(anaconda-mode-find-definitions :async t))))

;; ----------------
;; c/c++
;; ----------------
(use-package irony
  :ensure t
  :defer t
  :init
  (use-package ggtags :ensure t)
  (my/add-hooks '(c++-mode-hook c-mode-hook objc-mode-hook)
             (irony-mode)
             (ggtags-mode 1)
             (c-turn-on-eldoc-mode))
  (defvar c-eldoc-includes "-I/usr/include -I/usr/include/python3.5m -I./ -I../")
  :config
  (defun my-irony-mode-hook ()
    (defun irony-snippet-available-p () -1)
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook (lambda ()
                               (my-irony-mode-hook)
                               (irony-cdb-autosetup-compile-options)))
  (use-package company-irony-c-headers :ensure t :defer t)
  )

(my|define-jump-handlers c-mode)
(my|define-jump-handlers c++-mode)
(setq c-default-style "linux"
      c-basic-offset 4)


;; ----------------
;; haskell
;; ----------------

;; (setq haskell-process-type 'stack-ghci)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(setq
 ghc-report-errors nil
 haskell-process-suggest-remove-import-lines t
 haskell-process-auto-import-loaded-modules t
 haskell-process-log t
 haskell-process-type 'stack-ghci
 haskellcompany-ghc-show-info t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)
            (hindent-mode)
            (eldoc-mode)
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
            (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
            (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
            (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
            (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
            (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
            ))
(add-hook 'haskell-cabal-mode-hook
          (lambda ()
            (eldoc-mode)
            (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
            (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
            (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
            (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
            ))
(eval-after-load 'haskell-mode '(progn (defun ghc-check-syntax ())))


;; ----------------
;; js
;; ----------------

(use-package nvm
  :if (file-exists-p "~/.nvm")
  :ensure t
  :config

  (setq my/default-node-version (car (split-string (my/read-file-contents "~/.nvm/alias/default"))))
  (defvar my/current-node-version nil
    "Currently used node version. Set only after a js file is opened")

  (defun my/add-node-to-path (version)
    (let ((pathstr (format (expand-file-name "~/.nvm/versions/node/%s/bin") version)))
      (unless (member pathstr exec-path) (setq exec-path (append exec-path (list pathstr))))))

  (defun my/remove-node-from-path (version)
    (let ((pathstr (format (expand-file-name "~/.nvm/versions/node/%s/bin") version)))
      (setq exec-path (cl-remove-if (lambda (el) (string= el pathstr)) exec-path))))

  (defun my/select-node-version ()
    (completing-read
     "node version: "
     (reverse (mapcar 'car (nvm--installed-versions)))
     nil nil nil nil my/default-node-version))

  (defun my/nvm-use-ver ()
    (interactive)
    (let ((choice (my/select-node-version)))
      (nvm-use choice)
      (unless (null my/current-node-version) (my/remove-node-from-path my/current-node-version))
      (my/add-node-to-path choice)
      (setq my/current-node-version choice)
      ))
  )

(require 'js-doc)
(add-hook 'js2-mode-hook (lambda ()
                           (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
                           (define-key js2-mode-map "\C-c@" 'js-doc-insert-tag)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(my|define-jump-handlers js2-mode)
(my|define-jump-handlers web-mode)

(dolist (mode '("js2"))
  (let ((hook (intern-soft (format "%s-mode-hook" mode)))
        (handler (intern-soft (format "my-jump-handlers-%s-mode" mode))))
    (add-hook hook `(lambda ()
                      (if (and (file-exists-p "~/.nvm")
                               (null my/current-node-version))
                          (my/nvm-use-ver))
                      (setq evil-shift-width 2)
                      (use-package tern :ensure t :config (tern-mode))
                      (add-to-list (quote ,handler) 'tern-find-definition)))))

(setq ;; js2-mode
 js2-basic-offset 2
 js-indent-level 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
(setq js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil)

(defun my/toggle-jsmodes ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((mode major-mode))
      (cond
       ((string= mode "js2-mode") (web-mode))
       ((string= mode "web-mode") (js2-mode))
       ((string= mode "js-mode") (js2-mode))))))


;; ----------------
;; html
;; ----------------
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; ----------------
;; lisps
;; ----------------
;; Common LISP
;; (use-package slime
;;   :ensure t
;;   :defer t
;;   :init
;;   ;; set up slime according to this link
;;   ;; http://www.jonathanfischer.net/modern-common-lisp-on-linux/

;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "sbcl")
;;   (use-package slime-company :ensure t :defer t)
;;   (slime-setup '(slime-fancy slime-company))
;;   )

;; expand macros in another window
(define-key lisp-mode-map (kbd "C-c C-m") '(lambda () (interactive) (macrostep-expand t)))
(my/add-hooks '(lisp-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook) (eldoc-mode))


;; ----------------
;; Clojure
;; ----------------

(defun my/clojure-toggle-ignore-this-sexp ()
  (interactive)
  (let ((sym "#_"))
    (save-excursion
      (sp-backward-up-sexp)
      (if (re-search-backward sym (- (point) 2) t)
          (delete-char 2)
        (insert sym))
      (if (fboundp 'cider-format-defun)
          (cider-format-defun)))))


(add-hook
 'clojure-mode-hook
 (lambda ()
   (eldoc-mode)
   (define-key clojure-mode-map (kbd "C-x C-M-i") #'my/clojure-toggle-ignore-this-sexp)
   ;; (sp-local-pair 'clojure-mode "(" nil :actions '(:rem insert))
   ))

(my/make-emacs-mode 'cider-stacktrace-mode)
(my/make-emacs-mode 'cider-docview-mode)

(add-hook
 'cider-repl-mode-hook
 (lambda ()
   (eldoc-mode)
   (define-key cider-repl-mode-map "\C-c\C-l" 'cider-repl-clear-buffer)))

;; ----------------
;; LaTeX
;; ----------------
(defun my/latex-setup ()
  (defun my/texcount ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" "-nc" this-file)))))
      (string-match "\n$" word-count)
      (message (replace-match "" nil nil word-count))))
  (define-key LaTeX-mode-map "\C-cw" 'my/texcount))

(add-hook 'LaTeX-mode-hook 'my/latex-setup t)

;; ----------------
;; json, yaml, markdown, rst
;; ----------------
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

(use-package yaml-mode :ensure t)


;; ----------------
;; company & completions
;; ----------------
(use-package company
  :ensure t
  :defer t
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.3)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-tern :ensure t)
  (use-package company-irony :ensure t :defer t)
  (use-package company-quickhelp :ensure t)
  (company-quickhelp-mode 1)
  (diminish 'company-mode " Com")
  (eval-after-load "company"
    '(progn
       (add-to-list 'company-backends 'company-anaconda)
       (add-to-list 'company-backends '(company-irony-c-headers company-c-headers company-irony))
       (add-to-list 'company-backends 'company-ghc)
       (add-to-list 'company-backends 'company-tern)
       (add-to-list 'company-backends 'company-files)
       (define-key company-active-map (kbd "C-k") 'company-select-previous)
       (define-key company-active-map (kbd "C-j") 'company-select-next)
       (define-key company-active-map (kbd "C-p") 'company-select-previous)
       (define-key company-active-map (kbd "C-n") 'company-select-next)
       (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key company-active-map (kbd "C-l") 'company-complete-selection)
       (define-key company-active-map (kbd "C-f") 'company-show-location)
       (setq company-minimum-prefix-length 3)
       ))
  )


;; ----------------
;; syntax checking
;; ----------------

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (use-package flymake-yaml :ensure t)
  (use-package flycheck-mypy :ensure t)
  (use-package flycheck-irony :ensure t)
  (use-package flycheck-haskell :ensure t)
  (use-package flycheck-yamllint :ensure t)
  (eval-after-load 'flycheck
    '(progn
       (set-face-background 'flycheck-warning "unspecified-bg")
       (set-face-foreground 'flycheck-warning "unspecified-fg")
       (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
       (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
       (add-hook 'flycheck-mode-hook #'flycheck-yamllint-setup)
      ))
  (define-key global-map (kbd "C-c ! t") 'flycheck-mode)
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*Flycheck errors*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (side            . bottom)
		 (reusable-frames . visible)
		 (window-height   . 0.33)))

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  )


;; ----------------
;; ido mode
;; ----------------
(use-package flx-ido
  :ensure  t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  )


;; ----------------
;; helm
;; ----------------
(use-package helm :ensure t)
;; (use-package helm-config)
(use-package helm-themes)
;; (use-package helm-mode :config (helm-mode 1))
;; (use-package helm-adaptive :config (helm-adaptive-mode 1))
;; (use-package helm-utils :config (helm-popup-tip-mode 1))
;; (use-package helm-sys :config (helm-top-poll-mode 1))
;; (use-package helm-fuzzy-find :ensure t)
;; (use-package helm-ag :ensure t)

;; (setq helm-M-x-fuzzy-match t)
;; (setq helm-locate-fuzzy-match t)

;; ;; Global-map
;; (global-set-key (kbd "M-x")                          'undefined)
;; (global-set-key (kbd "M-x")                          'helm-M-x)
;; (global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
;; (global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
;; (global-set-key (kbd "C-,")                          'helm-calcul-expression)
;; (global-set-key (kbd "C-x C-d")                      'helm-browse-project)
;; (global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
;; (define-key global-map [remap jump-to-register]      'helm-register)
;; (define-key global-map [remap list-buffers]          'helm-mini)
;; (define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
;; (define-key global-map [remap find-tag]              'helm-etags-select)
;; (define-key global-map [remap xref-find-definitions] 'helm-etags-select)
;; (define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
;; (define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
;; (define-key global-map (kbd "M-g i")                 'helm-gid)
;; (define-key global-map (kbd "C-x r p") 'helm-projects-history)
;; (define-key helm-map (kbd "C-j") 'helm-next-line)
;; (define-key helm-map (kbd "C-k") 'helm-previous-line)
;; (define-key helm-map (kbd "C-h") 'helm-find-files-up-one-level)
;; (define-key helm-map (kbd "C-l") (kbd "RET"))

;; ;; splitting
;; (define-key helm-map (kbd "C-c s") 'my/helm-file-switch-other-window-horizontally)
;; (define-key helm-map (kbd "C-c v") 'my/helm-file-switch-other-window-vertically)
;; (define-key helm-buffer-map (kbd "C-c s") 'my/helm-buffer-switch-other-window-horizontally)
;; (define-key helm-buffer-map (kbd "C-c v") 'my/helm-buffer-switch-other-window-vertically)

;; (with-eval-after-load 'helm-files
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;   (define-key helm-find-files-map
;;     (kbd "S-<tab>") 'helm-find-files-up-one-level)
;;   (define-key helm-find-files-map
;;     (kbd "<backtab>") 'helm-find-files-up-one-level)
;;   ;; For terminal.
;;   (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
;;   (define-key helm-find-files-map
;;     (kbd "S-TAB") 'helm-find-files-up-one-level)
;;   (define-key helm-map (kbd "C-z") 'helm-select-action))

;; (when linum-mode
;;   (add-hook 'helm-after-initialize-hook (lambda ()
;;                                           (with-helm-buffer
;;                                             (linum-mode 0)))))

;; (global-set-key (kbd "C-x C-f") 'my/helm-find-files)
;; (diminish 'helm-mode "")

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on)
;;   )

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  )

(use-package perspective :config (persp-mode))
(use-package persp-projectile :ensure t)

(use-package ivy
  :ensure t
  :init
  (use-package counsel :ensure t)
  (use-package swiper :ensure t)
  (use-package counsel-projectile :ensure t)
  :config
  (ivy-mode 1)
  (diminish 'ivy-mode "")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-s-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-x r b") 'counsel-bookmark)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)
  (setq projectile-switch-project-action 'counsel-projectile-find-file)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)  ;; no ^ initially
  (setq ivy-magic-tilde nil)
  )

;; ----------------
;; org mode
;; ----------------
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time
      org-confirm-babel-evaluate nil
      org-clock-into-drawer nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-directory (expand-file-name "~/org/")
      org-default-notes-file (concat org-directory "capture.org")
      org-ellipsis "…"
      org-src-window-setup 'other-window
      ;; org-src-window-setup 'current-window
      )

;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
    '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook 'org-mode-hook
          (lambda ()
            (require 'my-org-blog)
            (define-key org-mode-map (kbd "TAB") 'org-cycle)
            (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)

            (add-to-list
             'org-structure-template-alist
             '("pf" "#+BEGIN_SRC ipython :session :file %file :exports both\n?\n#+END_SRC"))
            (add-to-list
             'org-structure-template-alist
             '("po" "#+BEGIN_SRC ipython :session :exports both\n?\n#+END_SRC"))
            (add-to-list
             'org-structure-template-alist
             '("pr" "#+BEGIN_PREVIEW\n?\n#+END_PREVIEW"))

            (org-bullets-mode 1)
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((python . t)
               (ipython . t)
               (dot . t)
               (restclient . t)
               ;; other languages..
               ))
            (use-package ox-twbs :ensure t)
            (use-package ox-reveal :ensure t)
            (evil-leader/set-key
              "oc" 'org-table-delete-column
              "or" 'org-table-kill-row)))

;; ----------------------------------------------------------------------------------------------

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'myfonts)
(setq x-underline-at-descent-line t)

(if (display-graphic-p)
    (progn
      (my/set-theme))
  (progn
    (load-theme 'monokai)
    (set-face-attribute 'mode-line nil :background "#404040")
    (set-face-attribute 'mode-line-inactive nil :background "#282828")
    ))

(setq linum-format 'dynamic)
(set-face-attribute 'show-paren-match nil :weight 'normal)
(set-face-attribute 'trailing-whitespace nil :background "#602020")
