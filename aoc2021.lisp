
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2021 Massimo Zaniboni <mzan@dokmelody.org>

; ## Scope

; Study Common Lisp (CL) using AoC 2021 as exsercise.
; This code makes no justice of CL, because I'm a newbie.

(ql:quickload :trivia)     ;; common macro and functions and optimal pattern matching
(ql:quickload :alexandria) ;; common CL extensions
(ql:quickload :trivial-types)  ;; common types
(ql:quickload :cl-csv)     ;; read CSV files
(ql:quickload :1am)        ;; unit testing framework
(ql:quickload :defstar)    ;; add type annotations
(ql:quickload :series)     ;; like Clojure transducers
(ql:quickload :taps)       ;; additional functions for :series
(ql:quickload :cl-arrows)  ;; Clojure-like threading "->" macro
(ql:quickload :str)       ;; Common string manipulation functions
(ql:quickload :parse-float)
(ql:quickload :iterate)
(ql:quickload :let-plus)  ;; extend "let"

(defpackage :aoc2021
  (:import-from :alexandria)
  (:import-from :trivial-types :proper-list)
  (:use :cl :defstar :trivia :taps :parse-float :iterate :let-plus))

(in-package :aoc2021)

; ;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities

(defmacro ~> (&rest args)
  "A renamed -> threading macro, for avoiding conflict with defstar types."
  `(cl-arrows:-> ,@args))

(defmacro ~>> (&rest args)
  "A renamed ->> threading macro, for avoiding conflict with defstar types."
  `(cl-arrows:->> ,@args))

(defun my-array (n)
  (let ((vect (make-array n :initial-element 0)))
    (iter (for i from 0 below n)
          (after-each
            (setf (elt vect i) i))
          (finally (return vect)))))

(defun grow-vector-upto (vect new-size value)
  (iter (for i from (fill-pointer vect) below new-size)
        (vector-push-extend value vect)))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Day 1

(defun day-1a (fn)
  (let* ((prev nil)
         (c 0)
         (>prev? (lambda (x) (and (not (null prev)) (> x prev))))
         (count-c
           (lambda (x)
              (when (funcall >prev? x) (setf c (+ c 1)))
              (setf prev x)))
        )
    (series:iterate ((x (tap-fn #'parse-integer (tap :lines fn)))) (funcall count-c x))
   c)
)

(day-1a #P"data/day-1a.txt")
 ; => 1121 (11 bits, #x461)

(day-1a #P"data/day-1-test-1.txt")

(defun day-1b (fn)
  (let*
      ((ss1 (~>> fn
                 (tap :lines)
                 (tap-fn #'parse-integer)
                 (take-m-by-n 3 1)))
       (ss2 (~>> ss1
            (tap-fn (lambda (ss) (series:collect-length ss)))
            (take-until (lambda (n) (< n 3)))
            (tap-fn (lambda (x) nil))))
       (ss3 (series:until ss2 (tap-fn (lambda (ss) (series:collect-sum ss)) ss1)))
       (ss4 (~>> ss3
         (take-m-by-n 2 1)
         (tap-fn (lambda (s) (series:collect s)))
         (take-until (lambda (c) (< (length c) 2)))
         (filter (lambda (p) (< (first p) (second p))))))
    )

    (series:collect-length ss4)
))

; (day-1b #P"data/day-1-test-1.txt")
; (day-1b #P"data/day-1a.txt")
 ; => 1065 (11 bits, #x429)

; # LESSON LEARNED
;
; I used "series" that is a stream library.
; The resulting code is not stream-like, probably because I had to add some hints to the compiler.
; "series" are composable.

; ;;;;;;;;;;;;;;;;;;;;;;
; Day 2

(defun day2 ()
  (let ((in (open #P"data/day-2.txt" :if-does-not-exist nil))
        (x 0)
        (y 0))

    (when in
      (loop for line  =  (read-line in nil)
            while line do
               (let* ((cmd-arg (str:split " " line))
                      (cmd (read-from-string (first cmd-arg)))
                      (arg (parse-integer (second cmd-arg))))

                 (ecase cmd
                   ('forward (setf x (+ x arg)))
                   ('up (setf y (- y arg)))
                   ('down (setf y (+ y arg))))

            ))
    (close in))
    (* x y)))

(day2)
 ; => 1693300 (21 bits, #x19D674)
 ; => 1693300 (21 bits, #x19D674)

(defun day2b ()
  (let ((in (open #P"data/day-2.txt" :if-does-not-exist nil))
        (aim 0)
        (x 0)
        (y 0))

    (when in
      (loop for line  =  (read-line in nil)
            while line do
               (let* ((cmd-arg (str:split " " line))
                      (cmd (read-from-string (first cmd-arg)))
                      (arg (parse-integer (second cmd-arg))))

                 (ecase cmd
                   ('forward
                    (progn
                      (setf y (+ y (* aim arg)))
                      (setf x (+ x arg))))
                   ('up (setf aim (- aim arg)))
                   ('down (setf aim (+ aim arg))))

            ))
    (close in))
    (* x y)))

; (day2b)
; => 1857958050 (31 bits, #x6EBE30A2)

; # LESSON LEARNED
;
; This time I used explicit LOOP.
; LOOP is a foreign DSL, because it does not use the common Lisp syntax.
; It seems rather powerfull.
; CL favours explicit LOOPS respect recursion.

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Day 3a

(defun count-bits->gamma-epsilon (max-count count-bits)
  (iter (for cb in-vector count-bits downto 0)
        (for e first 1 then (* e 2))
        (with mc = (/ max-count 2))
        (with gamma = 0)
        (with epsilon = 0)
        (after-each
           (cond
             ((> cb mc) (setf gamma (+ gamma e)))
             (t (setf epsilon (+ epsilon e)))))
       (finally (return (values (* gamma epsilon) gamma epsilon)))))

(defun day3a (fn)
    (iter (for line in-file fn using #'read-line)
          (count line into count-lines)
          (with count-bits = (make-array 15 :fill-pointer 0 :adjustable t :element-type 'fixnum))
          (if-first-time (grow-vector-upto count-bits (length line) 0))
          (after-each
            (iter (for c in-string line)
                  (for i from 0 )
                  (when (char= c #\1) (incf (elt count-bits i)))))
          (finally
           (return (count-bits->gamma-epsilon count-lines count-bits)))))

(day3a #P"data/day-3.txt")
 ; => 3847100, 2635, 1460

(day3a #P"data/day-3-test.txt")
 ; => 198, 22, 9

; # LESSON LEARNED
;
; I used the "iterate" macro, instead of LOOP.
; I prefer it to LOOP: more lispy-like, but very powerful.

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Day 3b

; I store the strings in a trie, so I can choose in an efficient way the more frequent character.

(defclass trie ()
  (
   (parent
    :type (or trie null)
    :documentation "The parent trie."
    :initform nil
    :accessor trie-parent
    :initarg :parent)
   (child-0
    :type (or trie null)
    :documentation "The children with prefix 0."
    :accessor trie-child-0
    :initform nil)
   (child-1
    :type (or trie null)
    :documentation "The children with prefix 1."
    :accessor trie-child-1
    :initform nil)
   (count-children
    :type integer
    :documentation "Count children nodes."
    :initform 0
    :accessor trie-count-children)
   )
  (:documentation "A trie of 0|1, counting the number of strings.")
  )

(defun* (trie-child -> (or trie null)) ((self trie) (prefix fixnum))
  "Select the child using a number as prefix."
  (if (zerop prefix)
      (trie-child-0 self)
      (trie-child-1 self)))

(defun* (trie-child-count -> integer) ((self trie) (prefix fixnum))
  "Count the children of each brach."
  (let ((child (trie-child self prefix)))
    (if (null child) 0 (trie-count-children child))))

(defun* (trie-add-prefix! -> trie) ((self trie) (prefix number))
  (let* ((child
           (if (zerop prefix)
               (trie-child-0 self)
               (trie-child-1 self)))
         (new-child
            (if (null child)
                (make-instance 'trie :parent self)
                child)))
    (if (zerop prefix)
        (setf (trie-child-0 self) new-child)
        (setf (trie-child-1 self) new-child))

    new-child))

(defun* (trie-show -> null) ((out stream) (self trie) (prefix string))
  (format out "(~s:~d " prefix (trie-count-children self))

  (when (not (null (trie-child-0 self)))
    (trie-show out (trie-child-0 self) (concatenate 'string prefix "0")))

   (when (not (null (trie-child-1 self)))
    (trie-show out (trie-child-1 self) (concatenate 'string prefix "1")))

  (format out " )"))

(defun* (char01->number -> number) ((c character))
  (cond
    ((char= c #\0) 0)
    ((char= c #\1) 1)
    (t (error "Unexpected character"))))

(defun* (trie-insert-string! -> null) ((self trie) (cs string))
  "Insert a string in the trie."
  (iter (for c in-string cs)
        (for tt initially self then (trie-add-prefix! tt (char01->number c)))
        (finally
          (iter (for ttt first tt then (trie-parent ttt))
            (while ttt)
            (after-each (incf (trie-count-children ttt))))
         (return nil))))

(defun* (file->trie -> trie) ((fn pathname))
  "Read numbers from file and produce the trie."
    (iter (for line in-file fn using #'read-line)
          (with tt = (make-instance 'trie))
          (after-each (trie-insert-string! tt line))
          (finally (return tt))))

(defun* (seq01-rev->number -> number) ((self (proper-list (or number null))))
  "Convert a sequence of 0 and 1 in reverse order, to a number."
  (iter (for c in-sequence self)
        (for e first 1 then (* e 2))
        (if (not (null c)) (sum (* e c)))))

(defun* (trie-rating -> number) ((self trie) (follow-max? boolean))
  "Return oxygen or CO2 scrubber generator, following max or min digits."

  (iter (for tt01
             initially (cons self nil)
             then (let* ((tt (car tt01))
                         (child0 (trie-child-0 tt))
                         (child1 (trie-child-1 tt))
                         (c0 (trie-child-count tt 0))
                         (c1 (trie-child-count tt 1)))
                   (cond
                     ((and (zerop c0) (zerop c1)) (cons nil nil))
                     ((zerop c0) (cons child1 1))
                     ((zerop c1) (cons child0 0))
                     (t (if follow-max?
                            (if (>= c1 c0) (cons child1 1) (cons child0 0))
                            (if (<= c0 c1) (cons child0 0) (cons child1 1)))))))
        (while (car tt01))
        (collect (cdr tt01) at beginning into seq01-rev)
        (finally
           (return (seq01-rev->number seq01-rev)))))

(defun day3b (fn)
  (let* ((trie (file->trie fn))
         (oxygen (trie-rating trie t))
         (co2 (trie-rating trie nil)))
     (values (* oxygen co2) oxygen co2 (format nil "~B" oxygen) (format nil "~B" co2))))

(day3b #P"data/day-3-test.txt")
 ; => 230, 23, 10, "10111", "1010"

(day3b #P"data/day-3.txt")
 ; => 4105235, 2735, 1501, "101010101111", "10111011101"


; # LESSON LEARNED
;
; Iterate macro seems very powerful and nice to use.
