(setq lexical-binding t)

(require 'prelude)

;;; SXML Abstraction Layer
;;; DOM
;;; dom := (tag (attr...) dom-or-text...)

(defsubst dom/tag (d)
  "Tag of D."
  (car d))

(defsubst dom/attrs (d)
  "Attrs of D."
  (cadr d))

(defun dom/class (node)
  (->
   node
   dom/attrs
   (f-assoc 'class)))

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
  (let ((dispatch (lambda (c) (if (atom c) c (dom/alltext c)))))
    (mapconcat dispatch (dom/allchildren d) " ")))

(defun dom/pp (d pn pl)
  "D dom, PN print-node, PL print-leave."
  ;; pn : (t, a..., n...) -> str
  ;; pl : str | int | sym -> str
  (cond ((or (stringp d) (atom d)) (@ pl d))
        ((consp d) (@ pn d (mapconcat
                            (lambda (d) (dom/pp d pn pl))
                            (dom/allchildren d) " ")))))

(defun dom/ppp (d)
  "Default pretty printer D dom."
  (let* ((pp-tagger (lambda (tag) (lambda (sas sns) (format "<%s %s>%s</%s>" tag sas sns tag))))
         (pp-attrs (lambda (as) (mapconcat (-lambda ((k . v)) (format "%s=%S" k v)) as " ")))
         (dpn (lambda (n sns) (@ (@ pp-tagger (dom/tag n)) (@ pp-attrs (dom/attrs n)) sns)))
         (dpl (lambda (l) l)))
    (dom/pp d dpn dpl)))

(defun -flatmap (f l)
  "F L."
  (-flatten-n 1 (-map f l)))

(defun dom/walk (d)
  "D."
  (let* ((leafp (lambda (d) (or (stringp d) (atom d))))
         (nodep (lambda (d) (not (@ leafp d)))))
    (cond ((@ leafp d) d)
          ((@ nodep d) (cons d (-flatmap #'dom/walk (dom/nodes d)))))))

;;; DOM SELECTOR

(defun dom/simple-select (dom tag)
  "DOM TAG."
  (-filter (lambda (d) (eq (dom/tag d) tag)) (dom/walk dom)))

(provide 'dom)
