;;; etymonline.el --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;; Query Etymonline.com for etymology.
;;;
;;;   url -> html -> dom -> dom' -> text

;;; Dependencies:
;;;  - url.el
;;;  - xml.c (libxml)
;;;  - dash.el
;;;  - org.el (org-mode)

;;; Code:

(require 'url)
(require 'xml)
(require 'dash)
(require 'org)

(setq lexical-binding t)

(defvar *etym/site* "etymonline.com")
(defvar *etym/url* "http://etymonline.com/index.php?search=%s")
(defvar *etym/defs-fmt* "--%s:\n\n  %s\n\n")

;;; HELPERS

(defun etym/clean (s)
  (replace-regexp-in-string "" "" s))

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

;;; such stupid, this is not fs walking
;; (defun dom/alltext (d)
;;   "D dom -> [text]."
;;   (apply #'concat
;; 	 (-flatten (append (-filter #'stringp (cddr d))
;; 			   (-map #'dom/alltext (dom/nodes d))))))

;;; such embetterment
(defun dom/alltext (d)
  (let ((dispatch (lambda (c) (if (atom c) c (dom/alltext c)))))
    (mapconcat dispatch (dom/allchildren d) " ")))

(defun dom/pp (d pn pl)
  "D dom, PN print-node, PL print-leave."
  ;; pn : (t, a..., n...) -> str
  ;; pl : str | int | sym -> str
  (cond ((or (stringp d) (atom d)) (funcall pl d))
	((consp d) (funcall pn d (mapconcat
				  (lambda (d) (dom/pp d pn pl))
				  (dom/allchildren d) " ")))))

(defun dom/ppp (d)
  "Default pretty printer D dom."

  (defun pp-tagger (tag) (lambda (sas sns) (format "<%s %s>%s</%s>" tag sas sns tag)))

  (defun pp-attrs (as) (mapconcat (-lambda ((k . v)) (format "%s=%S" k v)) as " "))

  (let ((dpn (lambda (n sns) (funcall (pp-tagger (dom/tag n)) (pp-attrs (dom/attrs n)) sns)))
	(dpl (lambda (l) l)))
    (dom/pp d dpn dpl)))

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
  "DOM : dl > (dt dd)... -> (term definition)..."
  (let* ((first-dl (car (dom/simple-select dom 'dl)))
	 (seq-defs (-map #'dom/alltext (dom/nodes first-dl))))
    (-partition 2 seq-defs)))

(defun etym/parse (buf term)
  "BUF TERM."
  (with-current-buffer buf
    (etym/do-kill-http-header buf)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	   (res (etym/find-results dom)))
      res)))

(defun etym/present-buffer-modeless (term defs)
  "TERM DEFS: (term def)..."
  (let ((bn (format "%s @ %s" term *etym/site*)))
   (with-output-to-temp-buffer (get-buffer-create bn)
     (switch-to-buffer-other-window bn)
     (-each defs
       (-lambda ((sterm sdef))
	 (insert (format *etym/defs-fmt* (capitalize sterm) sdef)))))))

(defun etym/present-buffer (term defs)
  (with-current-buffer (get-buffer-create (format "*Etym/%s*" term))
    (insert (format "* %s\n\n" term))
    (-each defs (-lambda ((dt . dd))
		  (insert (etym/clean (format "** %s\n  %s\n\n" dt dd)))))
    ;;; following sequence packs the presentation modes
    (progn
      (fill-region (point-min) (point-max))
      (org-mode)    ;; previously (outline-mode)
      (show-all)    ;; tried (org-shifttab N) in vain
      (read-only-mode))
    (switch-to-buffer (current-buffer))))

(defun etym/main (term)
  "Prompt for TERM and query its etymology."
  (interactive "sTerm: ")
  (let* ((service (assoc *etym/default-source* *etym/sources*))
	 (url (cdr service)))
    (let* ((buf (url-retrieve-synchronously (format url term)))
	   (res (etym/parse buf term)))
      (etym/present-buffer term res))))


(provide 'etymonline)

;;; etymonline.el ends here
