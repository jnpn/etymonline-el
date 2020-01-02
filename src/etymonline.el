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

;;;
;;; Usage:
;;;  M-x etym/main <TERM> RET
;;;

;;; Code:

(require 'url)
(require 'xml)
(require 'dash)
(require 'org)

(add-to-list 'load-path (expand-file-name "."))

(require 'prelude)
(require 'dom)

;;; SERVICES

(defvar *etym/sources*
  '(("etymonline.com" . "https://www.etymonline.com/search?q=%s")
    ("word.etymonline.com" . "https://www.etymonline.com/word/%s")
    ("old.etymonline.com" . "http://etymonline.com/index.php?search=%s")))

(defvar *etym/default-source* "word.etymonline.com")

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

;;; PRESENTATION ~BACKEND ...

;; Flow: service -> term -> (buf, HTTP, DOM) -> service, term, [(dt, dd)]
;; so presentation is service, term, defs -> ...
(defun etym/simple-view (s te d)
  (let ((root (tmpl (te s d) " -- %s @%s\n\n%s"))
        (def (tmpl (dt dd) "* %s %s\n"))) ;; WARNING see below about "\n"
    (@ root te s
            (mapconcat (lambda (d) (apply def d)) d "\n"))))
;; the trailing \n has nasty implications related to
;; fill-region, and org-mode
;; Without it, definitions aren't seen as paragraphs
;; and fill-region will prefix all lines with a *
;; as it's the first char ?
;; then org mode thinks all lines are outlines.. FAIL

(defvar *etym/default-view* #'etym/simple-view)

;;; MAIN

(defun etym/find-results (dom)
  "DOM : dl > (dt dd)... -> (term definition)..."
  (let* ((first-dl (car (dom/simple-select dom 'dl)))
         (seq-defs (-map #'dom/alltext (dom/nodes first-dl))))
    (-partition 2 seq-defs)))

(defun etym/parse (buf term)
  "BUF TERM."
  (with-current-buffer buf
    (message ">>> %s %s" buf term)
    (etym/do-kill-http-header buf)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (res (etym/find-results dom)))
      res)))

(defun etym/present-buffer (service term defs)
  "SERVICE is the pair (name . url) to which TERM was queried.
Giving the list of pair DEFS (noun . definition)."
  (with-current-buffer (get-buffer-create (format "*Etym/%s*" term))
    (let ((view (@ *etym/default-view* service term defs)))
      (insert view))
    ;;; following sequence packs the presentation modes
    (progn
      (fill-region (point-min) (point-max))
      (org-mode)    ;; previously (outline-mode)
      (outline-show-all)    ;; tried (org-shifttab N) in vain
      (read-only-mode))
    (switch-to-buffer (current-buffer))))

(defun etym/main (term)
  "Prompt for TERM and query its etymology."
  (interactive "sTerm: ")
  (let* ((service (assoc *etym/default-source* *etym/sources*))
	     (url (cdr service)))
    (eww (format url term))))


(provide 'etymonline)

;;; etymonline.el ends here
