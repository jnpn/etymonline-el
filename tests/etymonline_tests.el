;;; etymonline_tests.el --- summary
;;; Commentary:
;;; etymonline_tests

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

;;; VIEW


(defun test-simple-view ()
  (etym/simple-view
   '("etymonline.com" . "http://...")
   "maison"
   '(("maison" . "french for house, home.")
     ("maisonette" . "petite maison.")
     ("house" . "home, french: maison."))))

(defun test-simple-view-2 ()
  (etym/simple-view
   '("x" . "y")
   "z"
   '(("maisonette (n.)   " "1818, \"small house,\" from French  maisonnette , diminutive of  maison  \"house\" (11c.), from Latin  mansionem  (see  mansion ). Meaning \"a part of a building let separately\" is from 1912.") ("titmouse (n.)   " "small, active bird, early 14c.,  titmose , from  tit  (n.2), expressing something small, + Old English  mase  \"titmouse,\" from Proto-Germanic  *maison  (cognates: Dutch  mees , German  meise ), from adj.  *maisa-  \"little, tiny.\" Spelling influenced 16c. by unrelated  mouse , \"when  mose  had long been obsolete as an independent word\" [OED]. The proper plural is  titmouses .") ("vaudeville (n.)   " "1735, \"a country song,\" especially one for the stage, from French  vaudeville  (16c.), alteration (by influence of  ville  \"town\") of Middle French  vaudevire , said to be from  (chanson du) Vau de Vire  \"(song of the) valley of Vire,\" in the Calvados region of Normandy, first applied to the popular satirical songs of Olivier Basselin, a 15c. poet who lived in Vire. The alternative explanation is that  vaudevire  derives from Middle French dialectal  vauder  \"to go\" +  virer  \"to turn.\" From the popularity of the songs in France grew a form of theatrical entertainment based on parodies of popular opera and drama, interspersed with songs.
 
The  Théatre du Vaudeville  is rich in parodies, which follow rapidly upon every new piece given at the Opera, or at the  Théatre Français . Their parody upon Hamlet is too ludicrous for description, but irresistibly laughable; and the elegaut light ballet of  La Colombe Retrouvée  [The Dove found again], I saw parodied at the  Vaudeville  as  \"La Maison Retrouvée\"  [The House found again], with a breadth of farce quite beyond the genius of Sadler's Wells. Some of the acting here, particularly that of the men, is exquisite; and the orchestra like all the orchestras in Paris is full and excellent. [\"France in 1816,\" by Lady Morgan]
 
As a sort of popular stage variety entertainment show suitable for families, from c. 1881 in U.S., displaced by movies after c. 1914, considered dead from 1932.
"))))

(provide 'etymonline_tests)

;;; etymonline_tests.el ends here
