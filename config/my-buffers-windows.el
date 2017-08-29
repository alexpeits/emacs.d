(require 'my-utils)

;; ----------------
;; projectile
;; ----------------

(use-package projectile
  :ensure t
  :init
  (use-package perspective :ensure t :config (persp-mode))
  (use-package persp-projectile :ensure t)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name)))))


;; ----------------------
;; ivy / counsel / swiper
;; ----------------------
(defun my/swiper (fuzzy)
  (interactive "P")
  (if (null fuzzy)
      (swiper)
    (let* ((temp-builders (copy-alist ivy-re-builders-alist))
          (ivy-re-builders-alist (add-to-list 'temp-builders
                                              '(swiper . ivy--regex-fuzzy))))
      (swiper))))

(use-package ivy
  :ensure t

  :init
  (use-package counsel :ensure t)
  (use-package swiper :ensure t)
  (use-package counsel-projectile :ensure t)
  :config
  (ivy-mode 1)
  (diminish 'ivy-mode "")
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'my/swiper)
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
  (global-set-key
   (kbd "C-x b")
   (lambda (prefix)
     (interactive "P")
     (if (null prefix)
         (if (projectile-project-p)
             (counsel-projectile-switch-to-buffer)
           (ivy-switch-buffer))
       (ivy-switch-buffer))))
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (setq counsel-ag-base-command "ag --vimgrep --nocolor --nogroup %s")
  ;; (add-hook 'projectile-after-switch-project-hook 'counsel-projectile-find-file)
  ;; (setq projectile-switch-project-action 'counsel-projectile-find-file)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)  ;; no ^ initially
  (setq ivy-magic-tilde nil)
  (ivy-set-actions
   'counsel-find-file
   `(("s"
      ,(my/control-function-window-split
        find-file-other-window
        0 nil)
      "split horizontally")
     ("v"
      ,(my/control-function-window-split
        find-file-other-window
        nil 0)
      "split vertically")
     ("n"
      ,(my/execute-f-with-hook
        find-file
        ace-select-window)
      "select window")
     ))
  (ivy-set-actions
   'counsel-projectile-find-file
   `(("s"
      ,(my/control-function-window-split
        counsel-projectile--find-file-other-window-action
        0 nil)
      "split horizontally")
     ("v"
      ,(my/control-function-window-split
        counsel-projectile--find-file-other-window-action
        nil 0)
      "split vertically")
     ("n"
      ,(my/execute-f-with-hook
        counsel-projectile--find-file-action
        ace-select-window)
      "select window")
     ))
  )


;; ----------------
;; helm
;; ----------------
(use-package helm :ensure t)
(use-package helm-themes)

;; ----------------
;; ace-window
;; ----------------
(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always t)
  ;; (global-set-key (kbd "M-p") 'ace-window)
  )

;; ----------------
;; dired
;; ----------------
(defun my/dired-find-file-ace ()
  (interactive)
  (let ((find-file-run-dired t)
        (fname (dired-get-file-for-visit)))
    (ace-select-window)
    (find-file fname)))

(with-eval-after-load 'dired
  (define-key dired-mode-map
    (kbd "C-c v")
    (my/control-function-window-split
     dired-find-file-other-window
     nil 0))
  (define-key dired-mode-map
    (kbd "C-c s")
    (my/control-function-window-split
     dired-find-file-other-window
     0 nil))
  (define-key dired-mode-map
    (kbd "C-c n")
    'my/dired-find-file-ace))

;; neotree
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
          (neotree-find file-name)))))

;; buffer-move
(use-package buffer-move
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (progn
        (global-set-key (kbd "<C-s-268632072>") 'buf-move-left)
        (global-set-key (kbd "<C-s-268632074>") 'buf-move-down)
        (global-set-key (kbd "<C-s-268632075>") 'buf-move-up)
        (global-set-key (kbd "<C-s-268632076>") 'buf-move-right))
    (progn
      (global-set-key (kbd "C-s-h") 'buf-move-left)
      (global-set-key (kbd "C-s-j") 'buf-move-down)
      (global-set-key (kbd "C-s-k") 'buf-move-up)
      (global-set-key (kbd "C-s-l") 'buf-move-right))))

;; eyebrowse
(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

;; popwin, mainly to always open helm buffers at bottom
(use-package popwin
  :ensure t
  :config
  (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
  (add-hook 'helm-after-initialize-hook (lambda ()
                                            (popwin:display-buffer helm-buffer t)
                                            (popwin-mode -1)))
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1))))

;; ----------------
;; ido mode
;; ----------------
(use-package flx-ido
  :ensure  t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(provide 'my-buffers-windows)
