
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2021 Massimo Zaniboni <mzan@dokmelody.org>

; # Scope

; Study Common Lisp (CL) using AoC 2021 as exsercise.
;
; This code should be read like a diary: first exsercise should reflect my poor knowledge of CL,
; and then improve with time.

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
(ql:quickload :let-plus)          ;; extend "let"
(ql:quickload :array-operations)  ;; rich management of arrays

(defpackage :aoc2021
  (:import-from :alexandria)
  (:import-from :trivial-types :proper-list :tuple)
  (:use :cl :defstar :trivia :taps :parse-float :iterate :let-plus))

(in-package :aoc2021)

; # Utilities

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

(defun nil-min2 (x y)
  (if (null x)
      y
      (if (null y)
          x
          (min x y))))

(defun nil<= (x y)
  (if (null x)
      y
      (if (null y)
          x
          (<= x y))))

(defun nil>= (x y)
  (if (null x)
      y
      (if (null y)
          x
          (>= x y))))

; # Day 1

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

; ## LESSONS LEARNED
;
; I used "series" that is a stream library.
; The resulting code is not stream-like, probably because I had to add some hints to the compiler.
; "series" are composable.

; # Day 2

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
                   (forward (setf x (+ x arg)))
                   (up (setf y (- y arg)))
                   (down (setf y (+ y arg))))

            ))
    (close in))
    (* x y)))

(day2)
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
                   ('down (setf aim (+ aim arg))))))
    (close in))
    (* x y)))

; (day2b)
; => 1857958050 (31 bits, #x6EBE30A2)

; ## LESSON LEARNED
;
; This time I used explicit LOOP.
; LOOP is a foreign DSL, because it does not use the common Lisp syntax.
; It seems rather powerfull.
; CL favours explicit LOOPS respect recursion.

; # Day 3a

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

; ## LESSON LEARNED
;
; I used the "iterate" macro, instead of LOOP.
; I prefer it to LOOP: more lispy-like, but very powerful.

; # Day 3b

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

  (unless (null (trie-child-0 self))
    (trie-show out (trie-child-0 self) (concatenate 'string prefix "0")))

   (unless (null (trie-child-1 self))
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


; ## LESSON LEARNED
;
; Iterate macro seems very powerful and nice to use.

; # Day 4

; ## Solution design
;
; The idea is to replace numbers with their order of extraction.
; So a row with numbers "3 10 8"
; is replaced with extration order "8 1 2",
; hence it will be completed at 8th extraction.
; 8 is the winner-extraction.
;
; This approach simplifies comparison between rows and boards,
; because it suffices to store the maximum and minimum winniner-extraction.

; The extraction order of numbers.
(deftype extraction () `fixnum)

(defclass board ()
  (
   (number->extraction
    :type hash-table
    :documentation "Map a number to its extraction order."
    :accessor board-number->extraction
    :initarg :number->exctraction)
   (extraction->number
    :type (vector extraction)
    :documentation "From an extraction order like 3, to the corresponding number."
    :accessor board-extraction->number
    :initarg :extraction->number)
   (content
    :type (vector extraction)
    :documentation "The numbers inside the board, saved in extraction order format."
    :accessor board-content
    :initform (make-array 25 :element-type 'fixnum :fill-pointer 0 :adjustable t))
   (winner-extraction
    :type (or extraction null)
    :documentation "The board winning extraction, i.e. the first extraction completing a row."
    :initform nil
    :accessor board-winner-extraction)
   )
  (:documentation "A Bingo board where numbers are expressed according their extraction order.")
  )

(defun* (board-show -> string) ((self board))
  (with-output-to-string (out)
    (iter (for e in-sequence (board-content self))
          (for n = (aref (board-extraction->number self) e))
          (with we = (board-winner-extraction self))
          (for extracted? = (or (null we) (<= e we)))
          (for winner? = (and (not (null we)) (= e we)))
          (for box = (if winner?
                         #\*
                         (if extracted?
                             #\+
                             #\ )))
          (after-each (format out " ~a~a " n box)))))

(defun* (parse-board-extractions -> (tuple hash-table (vector extraction))) ((in stream))
  "Parse a line with the extractions."
  (let* ((nn (mapcar #'parse-integer (str:split "," (read-line in))))
         (extractions (make-array
                         (length nn)
                         :element-type 'fixnum
                         :initial-contents nn))

         (numbers (iter (for i index-of-vector extractions)
                        (with hash = (make-hash-table :size (length extractions)))
                        (after-each (setf (gethash (aref extractions i) hash) i))
                        (finally (return hash)))))

    (read-line in nil nil) ; skip an empty line
    (list numbers extractions)))

(defun* (board-add-row! -> null) ((self board) (row sequence) &key ((add-to-content? boolean) t))
  "Add a row (or column) to the board and maintain board-state."
  (iter (for n in-sequence row)
        (for e = (gethash n (board-number->extraction self) -1))
        (with never-win = nil)
        (maximize e into row-winner)
        ; a row win when the last (maximum) number is extracted
        (after-each
          (if (= e -1)
            (setf never-win t)
            (when add-to-content? (vector-push-extend e (board-content self)))))
        (finally
           (unless never-win
             (setf (board-winner-extraction self) (nil-min2 row-winner (board-winner-extraction self))))
           nil)))
             ; the board win at the first (minimum) winning row

(defun* (parse-board! -> boolean)  ((self board) (in stream))
  "Start with a blank line and then complete the board. Return nil if there is no board to parse."
  (iter (for rs in-stream in using #'read-line)
        (for row = (map 'list #'parse-integer (str:split " " rs :omit-nulls t)))
        (for curr-row from 0)
        (for is-there-board? initially nil then t)
        (with cols = nil)
        (until (null row))
        (if-first-time
          (let ((d (length row)))
            (setf cols (make-array (list d d) :element-type 'fixnum))))
        (after-each
           (board-add-row! self row)
           (iter (for n in-sequence row)
                 (for curr-col from 0)
                 (setf (aref cols curr-col curr-row) n)))
        (finally
          (when cols
            (let+ (((col-i _) (array-dimensions cols)))
               (iter (for i from 0 below col-i)
                     (after-each (board-add-row! self (aops:sub cols i) :add-to-content? nil)))))

             (return is-there-board?))))

(defun* (board-winner-number -> (or fixnum null)) ((self board))
  (let* ((we (board-winner-extraction self)))
    (if (null we)
        nil
        (aref (board-extraction->number self) we))))

(defun* (board-score -> fixnum) ((self board))
  "Calculate the score according the rule of the exsercise."
  (iter (for e in-sequence (board-content self))
        (for n = (aref (board-extraction->number self) e))
        (with we = (board-winner-extraction self))
        (with wn = (board-winner-number self))
        (for mn = (if (null we)
                      n  ; TODO probably not correct: should take the last extracted number
                      (if (<= e we)
                          0 ; if the extraction is before the winner
                          n ; number not yet extracted
                      )))
        (sum mn into smn)
        (finally (return (* smn wn)))))

(defun select-board (fn &key select-best?)
  (with-open-file (in fn)
    (let+ (((ne en) (parse-board-extractions in)))

      (iter (for b = (make-instance 'board :number->exctraction ne :extraction->number en))
            (for b? = (parse-board! b in))
            (with best-b = nil)
            (with best-extraction = nil)
            (while b?)
            (for b-extraction = (board-winner-extraction b))
            (after-each
               (let ((is-best? (if select-best?
                                   (nil<= b-extraction best-extraction)
                                   (nil>= b-extraction best-extraction))))
                  (when is-best?
                     (setf best-extraction b-extraction)
                     (setf best-b b))))
            (finally (return (board-score best-b)))))))

(defun day4a (fn) (select-board fn :select-best? t))

(defun day4b (fn) (select-board fn :select-best? nil))

(day4a #P"data/day-4-test.txt")
 ; => 4512 (13 bits, #x11A0)

(day4a #P"data/day-4.txt")
 ; => 45031 (16 bits, #xAFE7)

(day4b #P"data/day-4-test.txt")
 ; => 1924 (11 bits, #x784)

(day4b #P"data/day-4.txt")
 ; => 2568 (12 bits, #xA08)
