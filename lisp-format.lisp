;;;; Line Formatter ;;;;

;;; Lines ;;;

#| line:
(prn (proc (lin "" "test" "" "" "test"))) ->
testtest
|#

(def lin a
  (mkdat 'lin a))

#| lines:
(prn (proc (lns "" "test" "" "" "test"))) ->

test


test
|#

(def lns a
  (mkdat 'lns a))

#| fresh lines:
(prn (proc (flns "" "test" "" "" "test"))) ->
test
test
|#

(def flns a
  (mkdat 'flns a))

#| level:
(prn (proc (lin "test" (lvl "test" "" "abc")))) ->
testtest
    
    abc
|#

(def lvl a
  (mkdat 'lvl a))

#| level with indent on next lines:
(prn (proc (lin "test" (lvlind 3 "testing" "abc" "def")))) ->
testtesting
       abc
       def

(def lvlind (n . a)
  (mkdat 'lvlind a {n n}))
|#

(mac lvlind (n fst . rst)
  `(lvl ,fst (ind ,n ,@rst)))

#| indent:
(prn (proc (lns "test" (ind 3 "testing" (ind 2 "abc" "def")) "hey"))) ->
test
   testing
     abc
     def
hey
|#

(def ind (n . a)
  (mkdat 'ind a {n n}))

#| with indent:
(prn (proc (lns "test" (ind 3 "testing" (wind 2 "abc" "def") "what") "hey"))) ->
test
   testing
  abc
  def
   what
hey
|#

(def wind (n . a)
  (mkdat 'wind a {n n}))

; return object: proc only uses dat property
(def note (a opt)
  (mkdat 'note a {opt opt}))

; applies f to dat property of a
(def calldat (f a)
  (mkdat (typ a) (f (dat a)) a))

#|(over dsp (a)
  (sup (trans a)))

(def trans (a)
  (case a
    obj? (case (typ a)
           'lin `(lin ,@(trans (dat a)))
           'lns `(lns ,@(trans (dat a)))
           'ind `(ind ,(. a n) ,@(trans (dat a)))
           'rt  `(rt ,(. a tp) ,(trans (dat a))))
    lis? (map trans a)
    a))|#

;;; Output lines ;;;

(var *indlvl* 0)
(var *begline* t)
(var *linepos* 0)
(var *indented* nil)

; don't send "\n" to emit
; *begline* is t after \n is printed til the first text is emitted
; *indented* is t after indentation til \n is printed

(def emit (a)
  ;(bugm 'emit a *indlvl* *begline* *linepos* *indented*)
  (unless *indented* (emitind))
  (unless (is a "")
    (pr a)
    (+= *linepos* (len a))
    (= *begline* nil)))

(def emitind ()
  (pr (nof *indlvl* " "))
  (+= *linepos* *indlvl*)
  (= *indented* t))

(def newln ()
  (pr "\n")
  (= *linepos* 0)
  (= *begline* t)
  (= *indented* nil))

(def freshln ()
  (unless *begline* (newln)))

(def resetln ()
  (= *indlvl* 0)
  (= *linepos* 0)
  (= *begline* t)
  (= *indented* nil)
  nil)

;;; Process lines ;;;

(def proc (a)
  (resetln)
  (tostr (proclin (lin a))))

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
  (each x (rflat (dat a))
    (if x (proc1 x))))

; process lns objects
(def proclns (a)
  (var fst t)
  (each x (rflat (dat a))
    (if (no x) (cont))
    (if fst
        (= fst nil)
        (newln))
    (proc1 x)))

; process flns objects
(def procflns (a)
  (var fst t)
  (each x (rflat (dat a))
    (if (no x) (cont))
    (if fst (= fst nil)
        (freshln))
    (proc1 x)))

(def proclvl (a)
  (dyn *indlvl* (if *indented* *linepos* *indlvl*)
    (proclns a)))

(mac dyn+= (a x . bd)
  `(dyn ,a (+ ,a ,x) ,@bd))

(mac dynzap (f a . bd)
  `(dyn ,a (,f ,a) ,@bd))

(def procind (a)
  (dyn+= *indlvl* (. a n)
    (proclns (lns (dat a)))))

(def procwind (a)
  (dyn *indlvl* (. a n)
    (proclns (lns (dat a)))))

(def rflat (a)
  (if (no a) nil
      (atm? (car a)) (cons (car a) (rflat (cdr a)))
      (app (rflat (car a)) (rflat (cdr a)))))
