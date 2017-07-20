;; evil modes in modeline
(defface my-evil-state-emacs-face
  '((t (:background "Orange" :foreground "White")))
  "Evil Mode Emacs State Face")

(defface my-evil-state-insert-face
  '((t (:background "DodgerBlue1" :foreground "White")))
  "Evil Mode Insert State Face")

(defface my-evil-state-normal-face
  '((t (:background "Red" :foreground "White")))
  "Evil Mode Normal Stace Face")

(defun evil-generate-mode-line-tag (&optional state)
  "Generate the evil mode-line tag for STATE."
  (let ((tag (evil-state-property state :tag t)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (propertize tag
                    'face (cond
                           ((string= "normal" state)
                            'my-evil-state-normal-face)
                           ((string= "insert" state)
                            'my-evil-state-insert-face)
                           ((string= "emacs" state)
                            'my-evil-state-emacs-face))
                    'help-echo (evil-state-property state :name)
                    'mouse-face 'mode-line-highlight)
      tag)))
