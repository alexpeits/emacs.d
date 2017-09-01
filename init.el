(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/custom-themes/"))
(setenv "PATH" (concat
                "/usr/local/bin" path-separator
                "~/bin" path-separator
                "~/.local/bin" path-separator
                "~/.cargo/bin" path-separator
                (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.cargo/bin")
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

(require 'my-engines)
(require 'my-utils)
(require 'my-ui)
(require 'my-buffers-windows)

(use-package cl :ensure t)

;; ----------------
;; various
;; ----------------

;; remember last position
(if (<= emacs-major-version 24)
    (use-package saveplace
       :ensure t
       :config
       (setq-default save-place t))
  (save-place-mode 1))

;; undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode ""))

;; use column width 80 to fill (e.g. with gq)
(setq-default fill-column 80)

;; diminish various minor modes
(diminish 'auto-revert-mode "")
(diminish 'eldoc-mode "")

;; store all backup and autosave files in
;; one dir
(defvar my/tempdir "~/.temp")
(setq backup-directory-alist
      `((".*" . ,my/tempdir)))
(setq auto-save-file-name-transforms
      `((".*" ,my/tempdir t)))

;; only with this set to nil can org-mode export & open too
(setq process-connection-type nil)

;; i love this
(defalias 'yes-or-no-p #'y-or-n-p)

;; use spaces
(setq-default indent-tabs-mode nil)

;; always scroll to the end of compilation buffers
(setq compilation-scroll-output t)

;; vim-like scrolling (emacs=0)
(setq scroll-conservatively 101)

;; some keymaps
(global-set-key (kbd "M-o") 'other-window)
;; used in help
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-p") 'find-function-at-point)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; flyspell on pure text buffers
(dolist (hook '(text-mode-hook change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; add env files to conf-mode alist
(add-to-list 'auto-mode-alist '(".env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '(".env.dev\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("env.example\\'" . conf-mode))

;; add extension for restclient.el
(add-to-list 'auto-mode-alist '(".http\\'" . restclient-mode))

;; DocView
(setq doc-view-continuous t)

;; window size when emacs is opened
(setq initial-frame-alist '((width . 223) (height . 73)))

(defun my/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(use-package smartparens
  :ensure t
  :defer t
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :init

  (defun my/smartparens-pair-newline (id action context)
    (save-excursion
      (newline)
      (indent-according-to-mode)))

  (defun my/smartparens-pair-newline-and-indent (id action context)
    (my/smartparens-pair-newline id action context)
    (indent-according-to-mode))

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
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-up-sexp))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (diminish 'which-key-mode ""))

(use-package imenu-list
  :ensure t
  :config
  (global-set-key (kbd "C-\\") #'imenu-list-minor-mode)
  (setq imenu-list-size 30))

;; ----------------
;; term, comint & eshell
;; ----------------

(add-hook 'term-mode-hook
          (lambda ()
            (linum-mode 0)
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (set-face-background 'term (face-attribute 'default :background))))

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

;; eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)))

;; comint
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
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  )

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
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "]"  'find-tag-other-window
    ";"  'evilnc-comment-or-uncomment-lines

    "bb" 'helm-buffers-list
    "bn" 'next-buffer
    "bp" 'previous-buffer

    "el" 'my/toggle-flycheck-error-list

    "fa" 'helm-ag
    "ff" 'helm-find

    "h"  'help

    "j"  'my/jump-to-definition

    "n"  'my/neotree-toggle-project

    "pl" 'persp-next
    "ph" 'persp-prev
    "pp" 'projectile-persp-switch-project
    "pq" 'persp-kill
    "pr" 'persp-rename
    "ps" 'counsel-projectile-ag

    "t8" 'whitespace-mode
    "tf" 'my/toggle-font
    "tg" 'global-diff-hl-mode
    "tj" 'my/toggle-jsmodes
    "tl" 'linum-mode
    "th" 'global-hl-line-mode
    "ts" 'flycheck-mode
    "tt" 'my/toggle-theme
    "tw" 'toggle-truncate-lines

    "uh" 'rainbow-mode
    "um" 'menu-bar-mode
    "up" 'rainbow-delimiters-mode

    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit

    "Ts" 'helm-themes
    ))

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
                  shell-mode
                  haskell-error-mode
                  haskell-interactive-mode
                  dired-mode))
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

  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

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

  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)

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

  (use-package evil-nerd-commenter
    :ensure t
    :config
    ;; evilnc toggles instead of commenting/uncommenting
    (setq evilnc-invert-comment-line-by-line t))

  ;; search with star while in v-mode
  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode))
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))


;; ----------------
;; python
;; ----------------
(use-package pyvenv) ;; this has to be downloaded

(defun my/set-python-interpreter ()
  (setq python-shell-interpreter
        (if (executable-find "ipython")
            "ipython"
          "python")))

(my/set-python-interpreter)
(add-hook 'pyvenv-post-activate-hooks 'my/set-python-interpreter)
(add-hook 'pyvenv-post-deactivate-hooks 'my/set-python-interpreter)

(defun eshell/workon (arg) (pyvenv-workon arg))
(defun eshell/deactivate () (pyvenv-deactivate))

(setq python-shell-prompt-detect-failure-warning nil)
(my|define-jump-handlers python-mode)
(my|define-jump-handlers cython-mode anaconda-mode-goto)
(my/make-emacs-mode 'inferior-python-mode)
(my/make-emacs-mode 'anaconda-mode-view-mode)
(defun my/mode-line-venv ()
  (if (string= major-mode "python-mode")
      (let ((venv (if (null pyvenv-virtual-env-name)
                      ""
                    pyvenv-virtual-env-name)))
        (concat
         " ["
         (propertize venv 'face 'font-lock-function-name-face)
         ;; (propertize venv 'face '(:foreground "plum2" :distant-foreground "plum4"))
         "]"))
    ""))

(setq-default mode-line-format
              '("%e" evil-mode-line-tag mode-line-front-space mode-line-mule-info
                mode-line-client mode-line-modified mode-line-remote
                mode-line-frame-identification mode-line-buffer-identification " "
                mode-line-position
                (vc-mode vc-mode)
                (:eval (my/mode-line-venv))
                " " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(add-hook 'python-mode-hook
          (lambda ()
            (anaconda-mode)
            (diminish 'anaconda-mode "")
            (anaconda-eldoc-mode)
            (diminish 'anaconda-eldoc-mode "")
            (define-key python-mode-map (kbd "C-c C-j") 'counsel-imenu)
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(python-pycompile)))
            (evil-leader/set-key
              "vw" 'pyvenv-workon
              "vd" 'pyvenv-deactivate)
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
  (use-package company-irony-c-headers :ensure t :defer t))

(my|define-jump-handlers c-mode)
(my|define-jump-handlers c++-mode)
(setq c-default-style "linux"
      c-basic-offset 4)


;; ----------------
;; haskell
;; ----------------

(use-package ghc :ensure t :defer t)
(use-package hindent :ensure t :defer t)
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
;; rust
;; ----------------

(use-package rust-mode
  :ensure t
  :config
  (use-package cargo :ensure t)
  (setq cargo-process--custom-path-to-bin "~/.cargo/bin")
  (add-hook 'rust-mode-hook
            (lambda ()
              (cargo-minor-mode)
              ;; (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)
              (racer-mode)
              (eldoc-mode)))
  (defvar my/rust-sysroot  "~/.rustup/toolchains/stable-x86_64-apple-darwin")
  (defvar my/rust-src-path (concat my/rust-sysroot "/lib/rustlib/src/rust/src"))
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path my/rust-src-path)
  (setenv "RUST_SRC_PATH" my/rust-src-path))

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
      )))

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

(add-hook
 'clojure-mode-hook
 (lambda ()
   (eldoc-mode)
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
         ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode :ensure t)


;; ----------------
;; company & completions
;; ----------------

(use-package company
  :ensure t
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.3)
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-tern :ensure t)
  ;; (use-package company-irony :ensure t :defer t)
  (use-package company-quickhelp :ensure t)
  (use-package company-anaconda :ensure t)
  (company-quickhelp-mode 1)
  (diminish 'company-mode "")
  (eval-after-load "company"
    '(progn
       (add-to-list 'company-backends 'company-anaconda)
       ;; (add-to-list 'company-backends '(company-irony-c-headers company-c-headers company-irony))
       (add-to-list 'company-backends 'company-ghc)
       (add-to-list 'company-backends 'company-racer)
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
       (setq company-minimum-prefix-length 3))))


;; ----------------
;; syntax checking
;; ----------------

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (defun my/toggle-flycheck-error-list ()
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window)
      (flycheck-list-errors)))
  (use-package flymake-yaml :ensure t)
  (use-package flycheck-mypy :ensure t)
  (use-package flycheck-irony :ensure t)
  (use-package flycheck-haskell :ensure t)
  (use-package flycheck-rust :ensure t)
  (use-package flycheck-yamllint :ensure t)
  (eval-after-load 'flycheck
    '(progn
       (set-face-background 'flycheck-warning "unspecified-bg")
       (set-face-foreground 'flycheck-warning "unspecified-fg")
       (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
       (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
       (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
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
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

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

;; mobileorg
(setq org-directory "~/org"
      org-mobile-inbox-for-pull "~/org/flagged.org"
      org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
(setq org-time-clocksum-format
    '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(defun my/org-insert-template ()
  (interactive)
  (let* ((templ-dir (expand-file-name "~/.emacs.d/org-templates/"))
         (ls (directory-files templ-dir nil "^[^.]"))
         (file (completing-read "Template: " ls))
         (path (concat templ-dir file)))
    (insert-file-contents path)))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook
 'org-mode-hook
 (lambda ()
   (use-package ox-twbs :ensure t)
   (use-package ox-reveal :ensure t)
   (use-package org-bullets
     :ensure t
     :config
     (org-bullets-mode 1))
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

   (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (ipython . t)
      ;; (dot . t)
      (restclient . t)
      ;; other languages..
      ))))


;; ----------------------------------------------------------------------------------------------

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq x-underline-at-descent-line t)
(my/set-theme)
(my/set-font)
;; (use-package spaceline
  ;; :ensure t
  ;; :init
  ;; (require 'spaceline-config)
  ;; (setq powerline-utf-8-separator-left 65279
        ;; powerline-utf-8-separator-right 65279
        ;; powerline-default-separator 'utf-8
        ;; powerline-height 20
        ;; spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  ;; (spaceline-spacemacs-theme))

(setq linum-format 'dynamic)
(set-face-attribute 'show-paren-match nil :weight 'normal)
(set-face-attribute 'trailing-whitespace nil :background "#602020")
