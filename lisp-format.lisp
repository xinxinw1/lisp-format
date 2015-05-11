;;;; Line Formatter ;;;;

;;; Lines ;;;

(def mklnobj (typ ob)
  (app ob {typ typ}))

#| line:
(prn (proc (lin "" "test" "" "" "test"))) ->
testtest
|#

(def lin a
  (mklnobj 'lin {dat a}))

#| lines:
(prn (proc (lns "" "test" "" "" "test"))) ->

test


test
|#

(def lns a
  (mklnobj 'lns {dat a}))

#| fresh lines:
(prn (proc (flns "" "test" "" "" "test"))) ->
test
test
|#

(def flns a
  (mklnobj 'flns {dat a}))

#| level:
(prn (proc (lin "test" (lvl "test" "" "abc")))) ->
testtest
    
    abc
|#

(def lvl a
  (mklnobj 'lvl {dat a}))

#| level with indent on next lines:
(prn (proc (lin "test" (lvlind 3 "testing" "abc" "def")))) ->
testtesting
       abc
       def

(mac lvlind (n fst . rst)
  `(lvl ,fst (ind ,n ,@rst)))
|#

(def lvlind (n . a)
  (mklnobj 'lvlind {dat a n n}))

#| indent:
(prn (proc (lns "test" (ind 3 "testing" (ind 2 "abc" "def")) "hey"))) ->
test
   testing
     abc
     def
hey
|#

(def ind (n . a)
  (mklnobj 'ind {dat a n n}))

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
  (mklnobj 'wind {dat a n n}))

; return object: proc only uses dat property
(def rt (tp a (o opt {}))
  (mklnobj 'rt {dat a tp tp opt (app {orig a} opt)}))

; applies f to dat property of a
(def mapdat (f a)
  (mklnobj (. a typ) (app a {dat (f (. a dat))})))

(over dsp (a)
  (sup (trans a)))

(def trans (a)
  (case a
    obj? (case (. a typ)
           'lin `(lin ,@(trans (. a dat)))
           'lns `(lns ,@(trans (. a dat)))
           'ind `(ind ,(. a n) ,@(trans (. a dat)))
           'rt  `(rt ,(. a tp) ,(trans (. a dat))))
    lis? (map trans a)
    a))

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
  (case a
    obj?
      (case (. a typ)
        'lin (proclin a)
        'lns (proclns a)
        'flns (procflns a)
        'lvl (proclvl a)
        'ind (procind a)
        'lvlind (proclvlind a)
        'wind (procwind a)
        'rt (proc1 (. a dat))
        (err proc1 "Unknown type a = $1" a))
    syn? (emit (str a))
    str? (emit a)
    (err proc1 "Unknown type a = $1" a)))

; process lin objects
(def proclin (a)
  (each x (rflat (. a dat))
    (unless (no x) (proc1 x))))

; process lns objects
(def proclns (a)
  (var fst t)
  (each x (rflat (. a dat))
    (if (no x) (cont))
    (if fst (= fst nil)
        (newln))
    (proc1 x)))

; process flns objects
(def procflns (a)
  (var fst t)
  (each x (rflat (. a dat))
    (if (no x) (cont))
    (if fst (= fst nil)
        (freshln))
    (proc1 x)))

(def proclvl (a)
  (dyn *indlvl* (if *indented* *linepos* *indlvl*)
    (proclns a)))

(def proclvlind (a)
  (dyn *indlvl* (if *indented* *linepos* *indlvl*)
    (procind a)))

(def procind (a)
  (dyn *indlvl* (+ *indlvl* (. a n))
    (proclns (lns (. a dat)))))

(def procwind (a)
  (dyn *indlvl* (. a n)
    (proclns (lns (. a dat)))))

(def rflat (a)
  (if (no a) nil
      (atm? (car a)) (cons (car a) (rflat (cdr a)))
      (app (rflat (car a)) (rflat (cdr a)))))
