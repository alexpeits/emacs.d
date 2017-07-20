(defun compare-lists (a b)
  (if (every 'seq-empty-p (list a b))
      t
    (let* ((a- (mapcar (lambda (x) (if (null x) 0 x)) a))
           (b- (mapcar (lambda (x) (if (null x) 0 x)) b))
           (x (first a-))
           (y (first b-)))
      (cond ((null x) nil)
            ((null y) t)
            ((= x y) (compare-lists (rest a) (rest b)))
            (t (< x y))))))

(defun compare-dates (a b)
  (apply 'compare-lists
         (mapcar 'reverse (mapcar 'parse-time-string (list a b)))))

(defun get-property (property element &optional default)
  (let ((value (org-element-property property element)))
    (if (null value) default value)))

(defun compare-timesheet-lists (a b)
  (apply 'compare-dates (mapcar 'first (list a b))))
