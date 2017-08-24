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

(defun round-time (time mult)
  "Return a hh:mm formatted time duration as a decimal (string)
denoting hours,rounding it to the nearest multiple of `mult'.

For example, the duration 4:47, with rounding to a quarter of
an hour (mult=25), gives 4,75 hours, and 4:55 gives 5.00"
  (let* ((hm (mapcar 'string-to-int (split-string time ":")))
         (h (car hm))
         (m (cadr hm))
         (perc (round (* 100 (/ m 60.0))))
         (rperc (* mult (/ (+ perc (/ mult 2)) mult))))
    (format "%d.%02d"
            (if (and (= 0 (mod rperc 100))
                     (> rperc perc))
                (1+ h)
              h)
            (mod rperc 100))))
