(deftheme leslie-knope
  "I'm big enough to admit that I am often inspired by myself.")

(custom-theme-set-faces
 'leslie-knope
 '(default ((t (:background "White" :foreground "Black"))))
 '(org-block ((t (:background "#fcfcfc" :foreground "#101010"))))
 '(org-block-begin-line ((t (:background "#eeeeee"))))
 '(org-block-end-line ((t (:background "#eeeeee"))))
 '(hl-line ((t (:background "#f5f5f5"))))
 '(my/fci-rule ((t (:background "#dddddd"))))
 '(whitespace-trailing ((t (:background "#602020"))))
 '(flycheck-warning ((t (:underline (:color "orange1" :style wave)))))
 '(flycheck-fringe-warning ((t (:foreground "orange1"))))
 '(flycheck-error ((t (:underline (:color "red1" :style wave)))))
 '(flycheck-fringe-error ((t (:foreground "red1"))))
 '(flycheck-info ((t (:underline (:color "DeepSkyBlue2" :style wave)))))
 '(flycheck-fringe-info ((t (:foreground "DeepSkyBlue2")))))

(provide-theme 'leslie-knope)
