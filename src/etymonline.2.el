(require 'dash)
(require 'dash-functional)

(add-to-list 'load-path (expand-file-name "."))
(require 'prelude)
(require 'dom)

(defun ->dom (html-buffer)
  (with-current-buffer html-buffer
    (libxml-parse-html-region (point-min) (point-max))))

(defun dom (term)
  (->
   (get-service-url)
   (format term)
   (url-retrieve-synchronously)
   (etym/do-kill-http-header)
   (unarg 2)
   ->dom))

(defun etym/raw (term)
  (->>
   term
   dom
   dom/walk
   (-filter (lambda (node)
              (let ((k (cdr (dom/class node))))
                (when (and (not (null k))
                           (stringp k))
                  (s-match ".*word--.*" k)))))
   (-map #'dom/alltext)))

(defun etym/struct (term)
  (-let (((def . related) (etym/raw term)))
    (list :def def
          :related related)))

(defun etym/show (term)
  (-let (((_ def _ related) (etym/struct term)))
    (format "--- %s \n\n%s\n\nrelated: %s\n\n--- end." term def related)))

;; (insert (etym/show "money"))

