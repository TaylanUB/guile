;;; base.scm --- The R7RS base library

;;      Copyright (C) 2013, 2014 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(define-library (scheme base)
  (export + * - / = < > <= >=

          (rename serious-condition? error-object?)
          (rename condition-message error-object-message)
          (rename condition-irritants error-object-irritants)

          read-error?
          file-error?

          (rename truncate-quotient quotient)
          (rename truncate-remainder remainder)
          (rename floor-remainder modulo)

          abs and append apply assoc assq assv begin binary-port? boolean?
          boolean=? bytevector
          bytevector-append bytevector-copy bytevector-copy! bytevector-length
          bytevector-u8-ref bytevector-u8-set! bytevector? caar cadr
          call-with-current-continuation call-with-port call-with-values
          call/cc car case cdar cddr cdr ceiling char->integer char-ready?
          char<=? char<? char=? char>=? char>? char? close-input-port
          close-output-port close-port complex? cond cond-expand cons
          current-error-port current-input-port current-output-port
          define define-record-type define-syntax define-values denominator
          do dynamic-wind else eof-object eof-object? eq? equal? eqv? error
          even? exact-integer-sqrt exact-integer? exact exact?
          expt features floor floor-quotient floor-remainder floor/
          flush-output-port for-each gcd get-output-bytevector
          get-output-string guard if include include-ci inexact inexact?
          input-port-open? input-port? integer->char integer? lambda lcm
          length let let* let*-values let-syntax let-values letrec letrec*
          letrec-syntax list list->string list->vector list-copy list-ref
          list-set! list-tail list? make-bytevector make-list make-parameter
          make-string make-vector map max member memq memv min
          negative? newline not null? number->string number? numerator odd?
          open-input-bytevector open-input-string open-output-bytevector
          open-output-string or output-port-open? output-port? pair?
          parameterize peek-char peek-u8 port? positive? procedure? quasiquote
          quote raise raise-continuable rational? rationalize
          read-bytevector read-bytevector! read-char read-line read-string
          read-u8 real? reverse round set! set-car! set-cdr! square
          string string->list string->number string->symbol string->utf8
          string->vector string-append string-copy string-copy! string-fill!
          string-for-each string-length string-map string-ref string-set!
          string<=? string<? string=? string>=? string>? string? substring
          symbol->string symbol=? symbol? syntax-error syntax-rules
          textual-port? truncate truncate-quotient truncate-remainder truncate/
          u8-ready? unless unquote unquote-splicing utf8->string values vector
          vector->list vector->string vector-append vector-copy vector-copy!
          vector-fill! vector-for-each vector-length vector-map vector-ref
          vector-set! vector? when with-exception-handler write-bytevector
          write-char write-string write-u8 zero?)

  (import (rename (rnrs base)
                  (error r6rs-error)
                  (map r6rs-map)
                  (for-each r6rs-for-each)
                  (vector-map r6rs-vector-map)
                  (vector-for-each r6rs-vector-for-each)
                  (string-for-each r6rs-string-for-each)
                  (vector->list r6rs-vector->list)
                  (vector-fill! r6rs-vector-fill!))
          (rename (srfi srfi-1)
                  (map srfi-1-map))
          (rnrs control)
          (rnrs exceptions)
          (rnrs conditions)
          (srfi srfi-6)
          (srfi srfi-9)
          (srfi srfi-11)
          (srfi srfi-39)
          (rnrs io simple)

          (only (srfi srfi-43)
                vector-append
                vector-copy
                vector-copy!
                vector-fill!
                vector->list)

          (rename (rnrs io ports)
                  (flush-output-port r6rs-flush-output-port)
                  (binary-port? r6rs-binary-port?)
                  (textual-port? r6rs-textual-port?))

          (rename (rnrs bytevectors)
                  (utf8->string      r6rs-utf8->string)
                  (string->utf8      r6rs-string->utf8)
                  (bytevector-copy   r6rs-bytevector-copy)
                  (bytevector-copy!  r6rs-bytevector-copy!))

          (rename (srfi srfi-13)
                  (string-map srfi-13-string-map)
                  (string-for-each srfi-13-string-for-each))

          (rename (only (guile)
                        case-lambda
                        define-values
                        define*
                        list-set!
                        exact-integer?
                        floor/
                        floor-quotient
                        floor-remainder
                        truncate/
                        truncate-quotient
                        truncate-remainder
                        syntax-error
                        port-closed?
                        char-ready?
                        %set-port-property!
                        %port-property
                        %cond-expand-features
                        scm-error)
                  ;; guile's char-ready? actually does the job of u8-ready?
                  (char-ready?  u8-ready?))

          (only (ice-9 rdelim) read-line))

  (begin
    (define (features)
      %cond-expand-features)     ; XXX also include per-module features?

    (define (error msg . objs)
      (apply r6rs-error #f msg objs))

    (define (square z)
      (* z z))

    ;; XXX FIXME When Guile's 'char-ready?' is fixed, this will need
    ;; adjustment.
    (define char-ready? u8-ready?)

    ;; We cannot use the versions of 'map' from Guile core or SRFI-1,
    ;; because this map needs to (1) use 'reverse' instead of 'reverse!'
    ;; and (2) support lists of differing lengths.
    (define map
      (let ()
        (define (check-procedure f)
          (if (not (procedure? f))
              (scm-error 'wrong-type-arg "map"
                         "Not a procedure: ~S" (list f) #f)))

        (define (no-finite-list-error ls)
          (scm-error 'wrong-type-arg "map"
                     "No finite list: ~S" ls #f))

        ;; 'min*' is like 'min', but treats #f as an exact infinity,
        ;; for purposes of finding the minimum length of the
        ;; possibly-circular lists.
        (define (min* a b)
          (cond ((not a) b)
                ((not b) a)
                (else (min a b))))

        (case-lambda
          ((f l)
           (check-procedure f)
           (if (not (length+ l))
               (no-finite-list-error (list l)))
           (let map1 ((l l) (out '()))
             (if (pair? l)
                 (map1 (cdr l) (cons (f (car l)) out))
                 (reverse out))))

          ((f l1 l2)
           (check-procedure f)
           (let ((len (min* (length+ l1) (length+ l2))))
             (if (not len)
                 (no-finite-list-error (list l1 l2)))
             (let map2 ((len len) (l1 l1) (l2 l2) (out '()))
               (if (zero? len)
                   (reverse out)
                   (map2 (- len 1) (cdr l1) (cdr l2)
                         (cons (f (car l1) (car l2))
                               out))))))

          ((f . ls)
           (check-procedure f)
           (let ((len (reduce min* #f (map length+ ls))))
             (if (not len)
                 (no-finite-list-error ls))
             (let mapn ((len len) (ls ls) (out '()))
               (if (zero? len)
                   (reverse out)
                   (mapn (- len 1) (map cdr ls)
                         (cons (apply f (map car ls)) out)))))))))

    (define* (vector->string v #:optional (start 0) (end (vector-length v)))
      (string-tabulate (lambda (i)
                         (vector-ref v (+ i start)))
                       (- end start)))

    (define* (string->vector s #:optional (start 0) (end (string-length s)))
      (let ((v (make-vector (- end start))))
        (let loop ((i 0) (j start))
          (when (< j end)
            (vector-set! v i (string-ref s j))
            (loop (+ i 1) (+ j 1))))
        v))

    (define string-map
      (case-lambda
        ((proc s) (srfi-13-string-map proc s))
        ((proc s1 s2)
         (let* ((len (min (string-length s1)
                          (string-length s2)))
                (result (make-string len)))
           (let loop ((i 0))
             (when (< i len)
               (string-set! result i
                            (proc (string-ref s1 i)
                                  (string-ref s2 i)))
               (loop (+ i 1))))
           result))
        ((proc . strings)
         (let* ((len (apply min (map string-length strings)))
                (result (make-string len)))
           (let loop ((i 0))
             (when (< i len)
               (string-set! result i
                            (apply proc (map (lambda (s)
                                               (string-ref s i))
                                             strings)))
               (loop (+ i 1))))
           result))))

    (define string-for-each
      (case-lambda
        ((proc s) (srfi-13-string-for-each proc s))
        ((proc s1 s2)
         (let ((len (min (string-length s1)
                         (string-length s2))))
           (let loop ((i 0))
             (when (< i len)
               (proc (string-ref s1 i)
                     (string-ref s2 i))
               (loop (+ i 1))))))
        ((proc . strings)
         (let ((len (apply min (map string-length strings))))
           (let loop ((i 0))
             (when (< i len)
               (apply proc (map (lambda (s)
                                  (string-ref s i))
                                strings))
               (loop (+ i 1))))))))

    (define vector-map
      (case-lambda
        ((proc v) (r6rs-vector-map proc v))
        ((proc v1 v2)
         (let* ((len (min (vector-length v1)
                          (vector-length v2)))
                (result (make-vector len)))
           (let loop ((i 0))
             (when (< i len)
               (vector-set! result i
                            (proc (vector-ref v1 i)
                                  (vector-ref v2 i)))
               (loop (+ i 1))))
           result))
        ((proc . vs)
         (let* ((len (apply min (map vector-length vs)))
                (result (make-vector len)))
           (let loop ((i 0))
             (when (< i len)
               (vector-set! result i
                            (apply proc (map (lambda (v)
                                               (vector-ref v i))
                                             vs)))
               (loop (+ i 1))))
           result))))

    (define vector-for-each
      (case-lambda
        ((proc v) (r6rs-vector-for-each proc v))
        ((proc v1 v2)
         (let ((len (min (vector-length v1)
                         (vector-length v2))))
           (let loop ((i 0))
             (when (< i len)
               (proc (vector-ref v1 i)
                     (vector-ref v2 i))
               (loop (+ i 1))))))
        ((proc . vs)
         (let ((len (apply min (map vector-length vs))))
           (let loop ((i 0))
             (when (< i len)
               (apply proc (map (lambda (v)
                                  (vector-ref v i))
                                vs))
               (loop (+ i 1))))))))

    (define (bytevector . u8-list)
      (u8-list->bytevector u8-list))

    (define (bytevector-append . bvs)
      (let* ((total-len (apply + (map bytevector-length bvs)))
             (result (make-bytevector total-len)))
        (let loop ((i 0) (bvs bvs))
          (when (not (null? bvs))
            (let* ((bv (car bvs))
                   (len (bytevector-length bv)))
              (r6rs-bytevector-copy! bv 0 result i len)
              (loop (+ i len) (cdr bvs)))))
        result))

    (define bytevector-copy
      (case-lambda
        ((bv)
         (r6rs-bytevector-copy bv))
        ((bv start)
         (let* ((len (- (bytevector-length bv) start))
                (result (make-bytevector len)))
           (r6rs-bytevector-copy! bv start result 0 len)
           result))
        ((bv start end)
         (let* ((len (- end start))
                (result (make-bytevector len)))
           (r6rs-bytevector-copy! bv start result 0 len)
           result))))

    (define bytevector-copy!
      (case-lambda
        ((to at from)
         (r6rs-bytevector-copy! from 0 to at
                                (bytevector-length from)))
        ((to at from start)
         (r6rs-bytevector-copy! from start to at
                                (- (bytevector-length from) start)))
        ((to at from start end)
         (r6rs-bytevector-copy! from start to at
                                (- end start)))))

    (define utf8->string
      (case-lambda
        ((bv) (r6rs-utf8->string bv))
        ((bv start)
         (r6rs-utf8->string (bytevector-copy bv start)))
        ((bv start end)
         (r6rs-utf8->string (bytevector-copy bv start end)))))

    (define string->utf8
      (case-lambda
        ((s) (r6rs-string->utf8 s))
        ((s start)
         (r6rs-string->utf8 (substring s start)))
        ((s start end)
         (r6rs-string->utf8 (substring s start end)))))

    (define (binary-port? obj)
      (and (port? obj) (r6rs-binary-port? obj)))

    (define (textual-port? obj)
      (and (port? obj) (r6rs-textual-port? obj)))

    (define* (flush-output-port #:optional (port (current-output-port)))
      (r6rs-flush-output-port port))

    (define (open-input-bytevector bv)
      (open-bytevector-input-port bv))

    (define (open-output-bytevector)
      (call-with-values
          (lambda () (open-bytevector-output-port))
        (lambda (port proc)
          (%set-port-property! port 'get-output-bytevector proc)
          port)))

    (define (get-output-bytevector port)
      (let ((proc (%port-property port 'get-output-bytevector)))
        (unless proc
          (error "get-output-bytevector: port not created by open-output-bytevector"))
        (proc)))

    (define* (peek-u8 #:optional (port (current-input-port)))
      (lookahead-u8 port))

    (define* (read-u8 #:optional (port (current-input-port)))
      (get-u8 port))

    (define* (write-u8 byte #:optional (port (current-output-port)))
      (put-u8 port byte))

    (define* (read-bytevector k #:optional (port (current-input-port)))
      (get-bytevector-n port k))

    (define* (read-bytevector! bv
                               #:optional
                               (port (current-input-port))
                               (start 0)
                               (end (bytevector-length bv)))
      (get-bytevector-n! port bv start (- end start)))

    (define* (write-bytevector bv
                               #:optional
                               (port (current-output-port))
                               (start 0)
                               (end (bytevector-length bv)))
      (put-bytevector port bv start (- end start)))

    (define read-string
      (case-lambda
        ((k) (get-string-n (current-input-port) k))
        ((k port) (get-string-n port k))))

    (define write-string
      (case-lambda
        ((s) (put-string (current-output-port) s))
        ((s port)
         (put-string port s))
        ((s port start)
         (put-string port s start))
        ((s port start end)
         (put-string port s start (- end start)))))

    (define write-bytevector
      (case-lambda
        ((bv) (put-bytevector (current-output-port) bv))
        ((bv port)
         (put-bytevector port bv))
        ((bv port start)
         (put-bytevector port bv start))
        ((bv port start end)
         (put-bytevector port bv start (- end start)))))

    (define (input-port-open? port)
      (unless (input-port? port)
        (error "input-port-open?: not an input port" port))
      (not (port-closed? port)))

    (define (output-port-open? port)
      (unless (output-port? port)
        (error "output-port-open?: not an output port" port))
      (not (port-closed? port)))

    (define (read-error? obj)
      (or (lexical-violation? obj)
          (i/o-read-error? obj)))

    (define (file-error? obj)
      (or (i/o-file-protection-error? obj)
          (i/o-file-is-read-only-error? obj)
          (i/o-file-already-exists-error? obj)
          (i/o-file-does-not-exist-error? obj)
          (i/o-filename-error? obj)))))  ; XXX is this needed?
