;; (set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Inconsolata-11" nil t)
;; (set-frame-font "Fira Mono-10" nil t)
;; (set-frame-font "Ubuntu Mono-13" nil t)
;; (set-frame-font "Liberation Mono-10.5" nil t)
;; (set-frame-font "DejaVu Sans Mono-10.5" nil t)
(set-frame-font "Menlo for Powerline-10.5" nil t)
;; (set-frame-font "Hack-10.5" nil t)
;; (set-frame-font "Consolas-11" nil t)
;; (set-frame-font "Input-10" nil t)


(defvar my/avail-fonts '("Menlo for Powerline-10.5"
                         "Source Code Pro-10"))
(defvar my/current-font 0)

(defun my/set-font (font)
  (set-frame-font font))

(defun my/toggle-font ()
  (interactive)
  (let ((next-font (mod (1+ my/current-font) (length my/avail-fonts))))
    (my/set-font (elt my/avail-fonts next-font))
    (setq my/current-font next-font)))

(evil-leader/set-key "tf" 'my/toggle-font)

(provide 'myfonts)
