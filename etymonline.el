;;; etymonline.el --- summary
;;; Commentary:
;;; etymonline

;;; Code:

(require 'dash)

(setq lexical-binding t)

(defvar *etym/url-schema* "http://etymonline.com/index.php?search=%s")


;; GLOBAL url -> html -> dom -> dom' -> text


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

;; (defun dom/children (d)
;;   (-filter (lambda (_) (not (stringp _))) (cddr d)))

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

;; (defun dom/text (dom)
;;   "@TODO DOM."
;;   (treemap (compose #'concat #'text) dom))

(defun dom/alltext (d)
  "D dom -> [text]."
  (apply #'concat
	 (-flatten (append (-filter #'stringp (cddr d))
			   (-map #'dom/alltext (dom/nodes d))))))

(defun dom/walk (d)
  "D."
  (defun leafp (d) (or (stringp d) (atom d)))
  (defun nodep (d) (not (leafp d)))
  (cond ((leafp d) d)
	;; isn't it treeidentity ?
	((nodep d) (-map #'dom/walk (dom/allchildren d)))))

(defun -flatmap (f l)
  "F L."
  (-flatten-n 1 (-map f l)))

(defun dom/walk (d)
  "D."
  (defun leafp (d) (or (stringp d) (atom d)))
  (defun nodep (d) (not (leafp d)))
  (cond ((leafp d) d)
	;; isn't it treeidentity ?
	((nodep d) (cons d (-flatmap #'dom/walk (dom/nodes d))))))

(defvar *html0* (with-current-buffer "html.5.html"
		  (libxml-parse-html-region (point-min)
					    (point-max))))


;;; DOM SELECTOR

;; (defun etym/find-results (dom)
;;   "FAIL. DOM : dl > (dt dd)... -> (term . definition)."
;;   (let* ((defs (-map #'dom/text (etym/select "dl")))
;; 	 (pairs (-zip defs (-drop 1 defs))))
;;     pairs))

;; (defun dom/select (dom selector)
;;   "TODO DOM: dom tree, SELECTOR: css dom selector."
;;   (if (null dom)
;;       nil
;;     (if (eq (dom/tag dom) selector)
;; 	dom
;;       (dom/select ...))))

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

(defun etym/parse (&rest status)
  "B."
  (message "[HTTP] %S" status)
  (with-current-buffer (current-buffer)
    (etym/do-kill-http-header (current-buffer))
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	   (res (etym/find-results dom)))
      ;;(message "%S" res)
      (etym/present-results res)
      )))

(defun etym/present-results (defs)
  "DEFS: (TERM term DEF def)."
  (with-output-to-temp-buffer (get-buffer-create "WAT")
    (switch-to-buffer-other-window "WAT")
    (-each defs
      ;;(-lambda (_ sterm _ sdef) (message "%s -> %s" (capitalize sterm) sdef))
      (lambda (d)
	(insert (format "%s -> %s\n" (capitalize (nth 1 d)) (nth 3 d)))))))

(defun etym/main ()
  "Test."
  (let ((term "night"))
    (url-retrieve (format *etym/url-schema* term)
		  #'etym/parse)))

(etym/main)

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

;; (test-dt-text-all-2)
;; -->
;; ((:term " night-owl (n.)" :def "\"owl which flies at night,\" 1590s; applied since 1846 (American English) to persons who are up or out late at night. Compare , also French  \"prostitute,\" literally \"night-swallow.\"night-hawkhirondelle de nuit")
;;  (:term " night-night" :def "nursery talk, \"good-night,\" 1896; form  is attested from 1876.nighty-night")
;;  (:term " night-light (n.)" :def "1640s, \"faint light visible in the sky at night,\" from  +  (n.). As \"small light used in rooms at night to keep them from total darkness\" from 1851.nightlight")
;;  (:term " night-hawk (n.)" :def "from 1610s in reference to various birds, from  +  (n.). Figurative sense of \"one who stays up and is active at night\" is from 1818.nighthawk")
;;  (:term " night (n.)" :def "Old English  (West Saxon , Anglian , ) \"night, darkness;\" the vowel indicating that the modern word derives from oblique cases (genitive , dative ), from Proto-Germanic  (cognates: Old Saxon and Old High German , Old Frisian and Dutch , German , Old Norse , Gothic ).
;; 
;; The Germanic words are from PIE  \"night\" (cognates: Greek  \"a night,\" Latin , Old Irish , Sanskrit  \"at night,\" Lithuanian  \"night,\" Old Church Slavonic , Russian , Welsh  \"tonight\"), according to Watkins, probably from a verbal root  \"to be dark, be night.\" For spelling with nihtneahtnæhtnehtnihteniht*nakht-nahtnachtNachtnattnahts*nekwt-nuksnoxnochdnaktamnaktisnostinoch'henoid*neg-")
;;  (:term " nightstick (n.)" :def "also , 1887, from  +  (n.). So called because it was carried on night patrols. night-sticknightstick")
;;  (:term " night-watch (n.)" :def "\"guard kept during the night,\" late Old English; see  +  (n.).nightwatch")
;;  (:term " nightclub (n.)" :def "also , \"club open at night,\" 1894, from  +  (n.) in the social sense.night-clubnightclub")
;;  (:term " nightly (adj.)" :def "Old English  \"nocturnal, of the night, at night;\" see  +  (1). As an adverb, Middle English , from the adjective.nihtlicnight-lynihtlich")
;;  (:term " nightjar (n.)" :def "nocturnal bird, goatsucker, 1620s, from  +  (v.). So called for the \"jarring\" sounds made by the male when the female is brooding, which have been described as a \"churring trill that seems to change direction as it rises and falls.\" An Old English word for it was  \"night raven.\"nightjarnihthræfn")
;;  (:term " nightcap (n.)" :def "also , late 14c., \"covering for the head, worn in bed,\" from  +  (n.). In the alcoholic sense, it is attested from 1818. American English sense of \"final event in a sporting contest\" (especially the second game of a baseball double-header) is from 1939.night-capnightcap")
;;  (:term " nightspot (n.)" :def "also , \"nightclub,\" 1936, from  (n.) +  (n.) \"place.\"night spotnightspot")
;;  )

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
	;;(-lambda (_ sterm _ sdef) (message "%s -> %s" (capitalize sterm) sdef))
	(lambda (d) (insert (format "%s -> %s\n" (capitalize (nth 1 d)) (nth 3 d))))))))

(provide 'etymonline)

;;; etymonline.el ends here
