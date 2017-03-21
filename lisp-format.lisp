;;;; Line Formatter ;;;;

(package Format

;;; Lines ;;;

(defbuil lin lns flns lvl)
(def ind (n . a) (mkdat 'ind a {n n}))
(def wind (n . a) (mkdat 'wind a {n n}))

#|
(def lvlind (n . a)
  (mkdat 'lvlind a {n n}))
|#

(mac lvlind (n fst . rst)
  `(lvl ,fst (ind ,n ,@rst)))

; proc only uses dat property
(def note (a opt)
  (mkdat 'note a {opts opt}))

(def type-to-builder (a)
  (casetyp a
    lin lin
    lns lns
    flns flns
    lvl lvl
    ind ind
    wind wind
    note note
    (err type-to-builder "Unknown type a = $1" a)))

(def map-format (f a)
  (casetyp a
    note (note (f (dat a)) (. a opts))
    (sym str num) (f a)
    (lin lns flns lvl) ((type-to-builder a) @(map f (dat a)))
    (ind wind) ((type-to-builder a) (. a n) @(map f (dat a)))
    (err map-format "Unknown type a = $1" a)))

#|
line:
(prn (proc (lin "" "test" "" "" "test"))) ->
testtest
----------------------
lines:
(prn (proc (lns "" "test" "" "" "test"))) ->

test


test
--------------------
fresh lines:
(prn (proc (flns "" "test" "" "" "test"))) ->
test
test
-------------------
level:
(prn (proc (lin "test" (lvl "test" "" "abc")))) ->
testtest
    
    abc
----------------------
level with indent on next lines:
(prn (proc (lin "test" (lvlind 3 "testing" "abc" "def")))) ->
testtesting
       abc
       def
-----------------
indent:
(prn (proc (lns "test" (ind 3 "testing" (ind 2 "abc" "def")) "hey"))) ->
test
   testing
     abc
     def
hey
-------------------
with indent:
(prn (proc (lns "test" (ind 3 "testing" (wind 2 "abc" "def") "what") "hey"))) ->
test
   testing
  abc
  def
   what
hey
---------------
|#


;;; Output lines ;;;

(defvar *indlvl* 0
        *begline* t
        *linepos* 0
        *indented* nil)

; don't send "\n" to emit
; *begline* is t after \n is printed til the first text is emitted
; *indented* is t after indentation til \n is printed

(def emit (a)
  ;(bugm 'emit a *indlvl* *begline* *linepos* *indented*)
  (zap str a)
  (ifnot *indented* (emitind))
  (ifnot (is a "")
    (pr a)
    (+= *linepos* (len a))
    (= *begline* nil)))

(def emitind ()
  (pr (nof *indlvl* " "))
  (+= *linepos* *indlvl*)
  (= *indented* t))

(def newln ()
  (pr "\n")
  (reset *linepos* *begline* *indented*))

(def freshln ()
  (ifnot *begline* (newln)))

(def resetln ()
  (reset *indlvl* *linepos* *begline* *indented*))

;;; Process lines ;;;

(def proc (a)
  (resetln)
  (tostr (proc1 a)))

; process any type
(def proc1 (a)
  (casetyp a
    lin (proclin a)
    lns (proclns a)
    flns (procflns a)
    lvl (proclvl a)
    ind (procind a)
    wind (procwind a)
    note (proc1 (dat a))
    (sym str num) (emit a)
    (err proc1 "Unknown type a = $1" a)))

; process lin objects
(def proclin (a)
  (runexist proc1 (dat a)))

; process lns objects
(def proclns (a)
  (runexist proc1 (dat a)
    btw (newln)))

; process flns objects
(def procflns (a)
  (runexist proc1 (dat a)
    btw (freshln)))

(def proclvl (a)
  (dyn *indlvl* (if *indented* *linepos* *indlvl*)
    (proclns a)))

(def procind (a)
  (dyn+= *indlvl* (. a n)
    (proclns (lns @(dat a)))))

(def procwind (a)
  (dyn *indlvl* (. a n)
    (proclns (lns @(dat a)))))

(export lin lns flns lvl ind wind lvlind note proc map-format)

)
