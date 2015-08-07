;;; etymoline_tests.el --- etymonline.el tests...
;;; Commentary:
;;; etymoline_tests

;;; Code:

(require 'dash)
(require 'etymonline)

;;; TESTS

(defmacro deftest (name pred message &rest body)
  "NAME PRED MESSAGE &BODY: naive test macro wrapper."
  (declare (indent 3))
  `(defun ,name ()
     (let* ((test (lambda () ,@body))
	    (res (funcall test))
	    (ans (if (funcall ,pred res) 'PASSED 'FAILED)))
       (message "[%S] %s" ans ,message))))

(deftest test-dom-0 #'identity "len"
  (load-file "./dom.el")
  (length DOM))

(deftest test-dl-0 #'identity "test-dl-0"
  (load-file "./dl.el")
  (length DL))

(deftest test-deftest #'identity "testing deftest"
  (> 4 5))

(deftest test-dl-get-children-then-tag #'identity "test-dl-get-children-then-tag"
  "Zip map ..."
  (load-file "./dl.el")
  (let ((dl DL))
    (-map (lambda (d) (when (consp d) (car d))) (dom/children dl))))

(deftest test-dl-zip-dl-childrens #'identity "test-dl-zip-dl-childrens"
  "Zip map ..."
  (load-file "./dl.el")
  (let* ((dl DL)
	 (cs (dom/children dl)))
    (-map (lambda (p) (car p))
	  (-zip cs (-drop 1 cs)))))

(deftest test-dt-text-first #'identity "..."
  "EXPECT FAILING."
  (let* ((seq-defs (dom/children DL))
	 (pair-defs (-zip seq-defs (-drop 1 seq-defs)))
	 (fst-def (first pair-defs)))
    (list :term (dom/text (car fst-def))
	  :def (dom/text (cdr fst-def)))))

;;;        l = dt dd dt dd...
;;; drop 1 l = dd dt dd dt
;;; zip ...  = (dt dd) (dd dt) ...
;;;            ^- ok   ^- nok
;;; DONE prelude.skip ...

(deftest test-dt-text-all-1 #'identity "test-dt-text-all-1"
  "EXPECT FAILING."
  (let* ((seq-defs (dom/children DL))
	 (pair-defs (-zip seq-defs (-drop 1 seq-defs)))
	 (reify (lambda (def) (list :term (dom/text (car def))
				    :def (dom/text (cdr def))))))
    (-map reify pair-defs)))

(deftest test-dt-text-all-2 #'identity "test-dt-text-all-2"
  "EXPECT SUCCESS."
  (let* ((seq-defs (dom/children DL))
	 (pair-defs (skip (-zip seq-defs (-drop 1 seq-defs))))
	 (reify (lambda (def) (list :term (dom/text (car def))
				    :def (dom/text (cdr def))))))
    (-map reify pair-defs)))

(deftest test-show-all #'identity "show all"
  "EXPECT SUCCESS."
  (let* ((seq-defs (dom/children DL))
	 (pair-defs (skip (-zip seq-defs (-drop 1 seq-defs))))
	 (reify (lambda (def) (list :term (dom/text (car def))
				    :def (dom/text (cdr def)))))
	 (defs (-map reify pair-defs)))
    (with-output-to-temp-buffer (get-buffer-create "WAT")
      (switch-to-buffer-other-window "WAT")
      (-each defs
	(lambda (d) (insert (format "%s -> %s\n" (capitalize (nth 1 d)) (nth 3 d))))))))

(provide 'etymoline_tests)

;;; etymoline_tests.el ends here
