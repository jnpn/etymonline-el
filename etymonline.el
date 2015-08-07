;;; etymonline.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Query Etymonline.com for etymology.
;;;
;;;   url -> html -> dom -> dom' -> text

;;; Code:

(require 'dash)

(setq lexical-binding t)

(defvar *etym/site* "etymonline.com")
(defvar *etym/url* "http://etymonline.com/index.php?search=%s")

;;; HTTP

(defun etym/do-kill-http-header (b)
  "B."
  (with-current-buffer b
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (kill-region (point-min) (point))
    (list 1 (point) b)))


;;; DOM
;;; dom := (tag (attr...) dom-or-text...)

(defsubst dom/tag (d)
  "Tag of D."
  (car d))

(defsubst dom/attrs (d)
  "Attrs of D."
  (cadr d))

(defsubst dom/allchildren (d)
  "D dom -> [dom or text]."
  (cddr d))

(defun dom/nodes (d)
  "D dom -> [dom]."
  (-filter #'consp (cddr d)))

(defun dom/texts (d)
  "D dom -> [dom]."
  (-filter #'stringp (cddr d)))

(defun dom/alltext (d)
  "D dom -> [text]."
  (apply #'concat
	 (-flatten (append (-filter #'stringp (cddr d))
			   (-map #'dom/alltext (dom/nodes d))))))

(defun -flatmap (f l)
  "F L."
  (-flatten-n 1 (-map f l)))

(defun dom/walk (d)
  "D."
  (defun leafp (d) (or (stringp d) (atom d)))
  (defun nodep (d) (not (leafp d)))
  (cond ((leafp d) d)
	((nodep d) (cons d (-flatmap #'dom/walk (dom/nodes d))))))


;;; DOM SELECTOR

(defun dom/simple-select (dom tag)
  "DOM TAG."
  (-filter (lambda (d) (eq (dom/tag d) tag)) (dom/walk dom)))

;;; MAIN

(defun etym/find-results (dom)
  "DOM : dl > (dt dd)... -> (term . definition)."
  (let* ((seq-defs (dom/nodes (car (dom/simple-select dom 'dl))))
	 (pair-defs (skip (-zip seq-defs (-drop 1 seq-defs))))
	 (reify (lambda (def) (list :term (dom/alltext (car def))
				    :def (dom/alltext (cdr def)))))
	 (defs (-map reify pair-defs)))
    defs))

(defun etym/parse (term &rest status)
  "TERM &STATUS."
  (message "[HTTP] %S -> %S" term status)
  (with-current-buffer (current-buffer)
    (etym/do-kill-http-header (current-buffer))
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	   (res (etym/find-results dom)))
      (etym/present-results term res))))

(defun etym/present-results (term defs)
  "TERM DEFS: (TERM term DEF def)."
  (let ((bn (format "%s @ %s" term *etym/site*)))
   (with-output-to-temp-buffer (get-buffer-create bn)
     (switch-to-buffer-other-window bn)
     (-each defs
       (lambda (d)
	 (insert (format "%s -> %s\n" (capitalize (nth 1 d)) (nth 3 d))))))))

(defun etym/main (term)
  "Prompt for TERM and query its etymology."
  (interactive "sTerm: ")
  (let ((callback (lambda (&rest status)
		    (etym/parse term status))))
   (url-retrieve (format *etym/url* term) callback)))


;;; TODO
;;; - history
;;; - thesaurus mode, general definition query on many websites
;;; - formatted presentation (grid layout or simple color scheme)
;;; - tests, cleaning, refactoring


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

(provide 'etymonline)

;;; etymonline.el ends here
