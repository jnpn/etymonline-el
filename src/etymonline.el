;;; etymonline.el --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;; Query Etymonline.com for etymology.
;;;
;;;   url -> html -> dom -> dom' -> text
;;;     etym/h etym/d     

;;; Dependencies:
;;;  - url.el
;;;  - xml.c (libxml)
;;;  - dash.el
;;;  - org.el (org-mode)

;;;
;;; Usage:
;;;  M-x etym/query <TERM> RET
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

(defun get-service-url ()
  ":: () -> string"
  (let* ((service (assoc *etym/default-source* *etym/sources*)))
    (cdr service)))

;;; HELPERS

(defun etym/clean (s)
  ":: string -> string"
  (replace-regexp-in-string "

;;; HTTP

(defun etym/do-kill-http-header (b)
  "B. buffer with http response.
kill-region from point-min to first occurence of \n\n
buffer -> buffer <-http>"
  (with-current-buffer b
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (kill-region (point-min) (point))
    (list 1 (point) b)))

;;; PRESENTATION ~BACKEND ...

;; Flow: service -> term -> (buf, HTTP, DOM) -> service, term, [(dt, dd)]
(defun etym/simple-view (s te d)
  "service -> term -> list-of-pairs -> string"
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
  "DOM : dl > (dt dd)... -> (term definition)...
find-results :: dom -> list-of-pairs"
  (let* ((first-dl (car (dom/simple-select dom 'dl)))
         (seq-defs (-map #'dom/alltext (dom/nodes first-dl))))
    (-partition 2 seq-defs)))

(defun etym/parse (buf term)
  "BUF TERM.
parse :: buf -> dom"
  (with-current-buffer buf
    (message ">>> %s %s" buf term)
    (etym/do-kill-http-header buf)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (res (etym/find-results dom)))
      res)))

(defun etym/fill-view (service term defs)
  "SERVICE is the pair (name . url) to which TERM was queried.
Giving the list of pair DEFS (noun . definition)."
  (let ((b (get-buffer-create (format "*Etym/%s*" term))))
    (with-current-buffer b
      (let ((view (@ *etym/default-view* service term defs)))
        (insert view)))
    b))

(defun etym/enhance-view (buf)
  "enhance-view :: buffer -> ()"
  (with-current-buffer buf
    (progn
      (fill-region (point-min) (point-max))
      (org-mode)        ;; previously (outline-mode)
      (outline-show-all) ;; tried (org-shifttab N) in vain
      (read-only-mode))
    (switch-to-buffer (current-buffer))))

(defun etym/query (term)
  "Prompt for TERM and query its etymology."
  (interactive "sTerm: ")
  (let ((url (get-service-url)))
    (eww (format url term))))

(provide 'etymonline)

;;; etymonline.el ends here