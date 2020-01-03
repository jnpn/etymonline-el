(setq lexical-binding t)

(defalias '@ 'funcall)

;;; DEBUG

(defun OH? (x) (message "[OH] %S" x) x)

;;; PRELUDE

(defun unpair (f)
  (lambda (pair)
    (let ((hd (car pair))
          (tl (cdr pair)))
      (@ f hd tl))))

(defun unarg (l n)
  (nth n l))

(defun f-assoc (a k)
  (assoc k a))

(defmacro tmpl (args &rest body)
  "usage (tmpl (x y z) <body>)
-> (lambda (x y z) (format <body> x y z))
"
  (if (and (= 1 (length body))
           (stringp (car body)))
      `(lambda ,args
         (format ,@body ,@args))
    (error "body is not a string")))

(provide 'prelude)
