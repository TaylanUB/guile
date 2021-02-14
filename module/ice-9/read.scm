;;; Scheme reader
;;; Copyright (C) 1995-1997,1999-2001,2003-2004,2006-2012,2014-2021
;;;   Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Implementation of Scheme's "read".
;;;
;;; Code:


;; While porting read.c to Scheme, I found these expressions that result
;; in undesirable behavior in the C reader.  Most all of them are also
;; present in the Scheme reader.  Probably I should fix all of them, but
;; I would first like to prove that the Scheme reader is good enough.
;;
;; (call-with-input-string "," read)
;; (read-disable 'square-brackets), then (call-with-input-string "]" read)
;; (call-with-input-string "(#tru1)" read) => '(#t ru1)
;; (call-with-input-string "(#true1)" read) => '(#t 1)
;; (call-with-input-string "(#fAlse)" read) => '(#f Alse)
;; (call-with-input-string "(#f1 #f2 #f3)" read) => error reading array
;; #:   foo
;; #:#|what|#foo
;; #@-(1 2 3) => #(1 2 3)
;; (#*10101010102) => (#*1010101010 2)

(define-module (ice-9 read)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:replace (read))

(define read-hash-procedures
  (fluid->parameter %read-hash-procedures))

(define (read-hash-procedure ch)
  (assq-ref (read-hash-procedures) ch))

(define (read-hash-extend ch proc)
  (let ((alist (read-hash-procedures)))
    (read-hash-procedures
     (if proc
         (assq-set! alist ch proc)
         (assq-remove! alist ch)))))

(define bitfield:record-positions? 0)
(define bitfield:case-insensitive? 2)
(define bitfield:keyword-style 4)
(define bitfield:r6rs-escapes? 6)
(define bitfield:square-brackets? 8)
(define bitfield:hungry-eol-escapes? 10)
(define bitfield:curly-infix? 12)
(define bitfield:r7rs-symbols? 14)
(define read-option-bits 16)

(define read-option-mask #b11)
(define read-option-inherit #b11)
(define read-options-inherit-all (1- (ash 1 read-option-bits)))

(define keyword-style-hash-prefix 0)
(define keyword-style-prefix 1)
(define keyword-style-postfix 2)

(define (compute-reader-options port)
  (let ((options (read-options))
        (port-options (or (%port-property port 'port-read-options)
                          read-options-inherit-all)))
    (define-syntax-rule (option field exp)
      (let ((port-option (logand port-options (ash read-option-mask field))))
        (if (= port-option (ash read-option-inherit field))
            exp
            port-option)))
    (define (bool key field)
      (option field
              (if (memq key options) (ash 1 field) 0)))
    (define (enum key values field)
      (option field
              (ash (assq-ref values (and=> (memq key options) cadr)) field)))
    (logior (bool 'positions bitfield:record-positions?)
            (bool 'case-insensitive bitfield:case-insensitive?)
            (enum 'keyword-style '((#f . 0) (prefix . 1) (postfix . 2))
                  bitfield:keyword-style)
            (bool 'r6rs-hex-escapes bitfield:r6rs-escapes?)
            (bool 'square-brackets bitfield:square-brackets?)
            (bool 'hungry-eol-escapes bitfield:hungry-eol-escapes?)
            (bool 'curly-infix bitfield:curly-infix?)
            (bool 'r7rs-symbols bitfield:r7rs-symbols?))))

(define (set-option options field new)
  (logior new (logand options (lognot (ash #b11 field)))))

(define (set-port-read-option! port field value)
  (let ((options (or (%port-property port 'port-read-options)
                     read-options-inherit-all))
        (new (ash value field)))
    (%set-port-property! port 'port-read-options
                         (set-option options field new)
                         )))

(define* (read #:optional (port (current-input-port)))
  ;; init read options
  (define opts (compute-reader-options port))
  (define (enabled? field)
    (not (zero? (logand (ash 1 field) opts))))
  (define (set-reader-option! field value)
    (set! opts (set-option opts field value))
    (set-port-read-option! port field value))
  (define (record-positions?) (enabled? bitfield:record-positions?))
  (define (case-insensitive?) (enabled? bitfield:case-insensitive?))
  (define (keyword-style) (logand read-option-mask
                                  (ash opts (- bitfield:keyword-style))))
  (define (r6rs-escapes?) (enabled? bitfield:r6rs-escapes?))
  (define (square-brackets?) (enabled? bitfield:square-brackets?))
  (define (hungry-eol-escapes?) (enabled? bitfield:hungry-eol-escapes?))
  (define (curly-infix?) (enabled? bitfield:curly-infix?))
  (define (r7rs-symbols?) (enabled? bitfield:r7rs-symbols?))
  (define neoteric 0)
  (define (next) (get-char port))
  (define (peek) (lookahead-char port))
  (define filename (port-filename port))
  (define (get-pos) (cons (port-line port) (port-column port)))

  (define (annotate line column datum)
    ;; FIXME: Return a syntax object instead, so we can avoid the
    ;; srcprops side table.
    (when (and (supports-source-properties? datum)
               ;; Line or column can be invalid via set-port-column! or
               ;; ungetting chars beyond start of line.
               (<= 0 line)
               (<= 1 column))
      ;; We always capture the column after one char of lookahead;
      ;; subtract off that lookahead value.
      (set-source-properties! datum `((filename . ,filename)
                                      (line . ,line)
                                      (column . ,(1- column)))))
    datum)

  (define (input-error msg . args)
    (apply error msg args))

  (define (read-semicolon-comment)
    (let ((ch (next)))
      (cond
       ((eof-object? ch) ch)
       ((eqv? ch #\newline) (next))
       (else (read-semicolon-comment)))))

  (define-syntax-rule (take-until first pred)
    (let ((acc (open-output-string)))
      (put-char acc first)
      (let lp ()
        (let ((ch (peek)))
          (cond
           ((or (eof-object? ch)
                (pred ch))
            (get-output-string acc))
           (else
            (put-char acc ch)
            (next)
            (lp)))))))
  (define-syntax-rule (take-while first pred)
    (take-until first (lambda (ch) (not (pred ch)))))

  (define (delimiter? ch)
    (or (memv ch '(#\( #\) #\; #\"
                   #\space #\return #\ff #\newline #\tab))
        (and (memv ch '(#\[ #\])) (or (square-brackets?) (curly-infix?)))
        (and (memv ch '(#\{ #\})) (curly-infix?))))

  (define (read-token ch)
    (take-until ch delimiter?))

  (define (read-mixed-case-symbol ch)
    (let* ((str (read-token ch))
           (len (string-length str)))
      (cond
       ((and (eq? (keyword-style) keyword-style-postfix)
             (> len 0) (eqv? #\: (string-ref str (1- len))))
        (let ((str (substring str 0 (1- len))))
          (symbol->keyword
           (string->symbol
            (if (case-insensitive?)
                (string-downcase str)
                str)))))
       (else
        (string->symbol
         (if (case-insensitive?)
             (string-downcase str)
             str))))))

  (define (read-parenthesized rdelim)
    (define (finish-curly-infix ret)
      ;; Perform syntactic transformations on {...} lists.
      (define (extract-infix-list ls)
        (and (pair? ls)
             (let ((x (car ls))
                   (ls (cdr ls)))
               (and (pair? ls)
                    (let ((op (car ls))
                          (ls (cdr ls)))
                      (if (null? ls)
                          (list op x)
                          (let ((tail (extract-infix-list ls)))
                            (and tail
                                 (equal? op (car tail))
                                 (cons* op x (cdr tail))))))))))
      (cond
       ((or (not (eqv? rdelim #\}))) ret) ; Only on {...} lists.
       ((null? ret) ret)                  ; {} => ()
       ((null? (cdr ret)) (car ret))      ; {x} => x
       ((null? (cddr ret)) ret)           ; {x y} => (x y)
       ((extract-infix-list ret))   ; {x + y + ... + z} => (+ x y ... z)
       (else (cons '$nfx$ ret))))   ; {x y . z} => ($nfx$ x y . z)
    (define curly? (eqv? rdelim #\}))
    (finish-curly-infix
     (let lp ((ch (next-non-whitespace)))
       (when (eof-object? ch)
         (input-error "unexpected end of input while searching for " rdelim))
       (cond
        ((eqv? ch rdelim) '())
        ((or (eqv? ch #\))
             (and (eqv? ch #\]) (or (square-brackets?) (curly-infix?)))
             (and (eqv? ch #\}) (curly-infix?)))
         (input-error "mismatched close paren" ch))
        (else
         (let ((expr (read-expr ch)))
           ;; Note that it is possible for scm_read_expression to
           ;; return `.', but not as part of a dotted pair: as in
           ;; #{.}#.  Indeed an example is here!
           (if (and (eqv? ch #\.) (eq? expr '#{.}#))
               (let* ((tail (read-expr (next-non-whitespace)))
                      (close (next-non-whitespace)))
                 (unless (eqv? close rdelim)
                   (input-error "missing close paren" rdelim))
                 tail)
               (cons expr (lp (next-non-whitespace))))))))))

  (define (hex-digit ch)
    (case ch
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (- (char->integer ch) (char->integer #\0)))
      ((#\a #\b #\c #\d #\e #\f)
       (+ 10 (- (char->integer ch) (char->integer #\a))))
      ((#\A #\B #\C #\D #\E #\F)
       (+ 10 (- (char->integer ch) (char->integer #\A))))
      (else #f)))
    
  (define (read-r6rs-hex-escape)
    (let ((ch (next)))
      (cond
       ((hex-digit ch) =>
        (lambda (res)
          (let lp ((res res))
            (let ((ch (next)))
              (cond
               ((hex-digit ch) => (lambda (digit) (lp (+ (* 16 res) digit))))
               ((eqv? ch #\;) (integer->char res))
               (else
                (input-error "invalid character in escape sequence: ~S" ch)))))))
       (else
        (input-error "invalid character in escape sequence: ~S" ch)))))

  (define (read-fixed-hex-escape len)
    (let lp ((len len) (res 0))
      (if (zero? len)
          (integer->char res)
          (let ((ch (next)))
            (cond
             ((hex-digit ch) =>
              (lambda (digit)
                (lp (1- len) (+ (* res 16) digit))))
             (else
              (input-error "invalid character in escape sequence: ~S" ch)))))))

  (define (read-string rdelim)
    (let ((acc (open-output-string)))
      (let lp ()
        (let ((ch (next)))
          (cond
           ((eof-object? ch)
            (input-error "unexpected end of input while reading string"))
           ((eqv? ch rdelim)
            (get-output-string acc))
           ((eqv? ch #\\)
            (let ((ch (next)))
              (when (eof-object? ch)
                (input-error "unexpected end of input while reading string"))
              (case ch
                ((#\newline)
                 (when (hungry-eol-escapes?)
                   ;; Skip intraline whitespace before continuing.
                   (let lp ()
                     (let ((ch (peek)))
                       (unless (or (eof-object? ch)
                                   (eqv? ch #\tab)
                                   (eq? (char-general-category ch) 'Zs))
                         (next)
                         (lp))))))
                ;; Accept "\(" for use at the beginning of
                ;; lines in multiline strings to avoid
                ;; confusing emacs lisp modes.
                ((#\| #\\ #\() (put-char acc ch))
                ((#\0)         (put-char acc #\nul))
                ((#\f)         (put-char acc #\ff))
                ((#\n)         (put-char acc #\newline))
                ((#\r)         (put-char acc #\return))
                ((#\t)         (put-char acc #\tab))
                ((#\a)         (put-char acc #\alarm))
                ((#\v)         (put-char acc #\vtab))
                ((#\b)         (put-char acc #\backspace))
                ((#\x)
                 (let ((ch (if (or (r6rs-escapes?) (eqv? rdelim #\|))
                               (read-r6rs-hex-escape)
                               (read-fixed-hex-escape 2))))
                   (put-char acc ch)))
                ((#\u)
                 (put-char acc (read-fixed-hex-escape 4)))
                ((#\U)
                 (put-char acc (read-fixed-hex-escape 8)))
                (else
                 (unless (eqv? ch rdelim)
                   (input-error "invalid character in escape sequence: ~S" ch))
                 (put-char acc ch)))
              (lp)))
           (else
            (put-char acc ch)
            (lp)))))))

  (define (read-character)
    (let ((ch (next)))
      (cond
       ((eof-object? ch)
        (input-error "unexpected end of input after #\\"))
       (else
        (let* ((tok (read-token ch))
               (len (string-length tok)))
          (define dotted-circle #\x25cc)
          (define r5rs-charnames
            '(("space" . #\x20) ("newline" . #\x0a)))
          (define r6rs-charnames
            '(("nul" . #\x00) ("alarm" . #\x07) ("backspace" . #\x08)
              ("tab" . #\x09) ("linefeed" . #\x0a) ("vtab" . #\x0b)
              ("page" . #\x0c) ("return" . #\x0d) ("esc" . #\x1b)
              ("delete" . #\x7f)))
          (define r7rs-charnames
            '(("escape" . #\x1b)))
          (define C0-control-charnames
            '(("nul" . #\x00) ("soh" . #\x01) ("stx" . #\x02)
              ("etx" . #\x03) ("eot" . #\x04) ("enq" . #\x05)
              ("ack" . #\x06) ("bel" . #\x07) ("bs"  . #\x08)
              ("ht"  . #\x09) ("lf"  . #\x0a) ("vt"  . #\x0b)
              ("ff"  . #\x0c) ("cr"  . #\x0d) ("so"  . #\x0e)
              ("si"  . #\x0f) ("dle" . #\x10) ("dc1" . #\x11)
              ("dc2" . #\x12) ("dc3" . #\x13) ("dc4" . #\x14)
              ("nak" . #\x15) ("syn" . #\x16) ("etb" . #\x17)
              ("can" . #\x18) ("em"  . #\x19) ("sub" . #\x1a)
              ("esc" . #\x1b) ("fs"  . #\x1c) ("gs"  . #\x1d)
              ("rs"  . #\x1e) ("us"  . #\x1f) ("sp"  . #\x20)
              ("del" . #\x7f)))
          (define alt-charnames
            '(("null" . #\x0) ("nl" . #\x0a) ("np" . #\x0c)))
          ;; Although R6RS and R7RS charnames specified as being
          ;; case-sensitive, Guile matches them case-insensitively, like
          ;; other char names.
          (define (named-char tok alist)
            (let lp ((alist alist))
              (and (pair? alist)
                   (if (string-ci=? tok (caar alist))
                       (cdar alist)
                       (lp (cdr alist))))))
          (cond
           ((= len 1) ch)
           ((and (= len 2) (eqv? (string-ref tok 1) dotted-circle))
            ;; Ignore dotted circles, which may be used to keep
            ;; combining characters from combining with the backslash in
            ;; #\charname.
            ch)
           ((and (<= (char->integer #\0) (char->integer ch) (char->integer #\7))
                 (string->number tok 8))
            ;; Specifying a codepoint as an octal value.
            => integer->char)
           ((and (eqv? ch #\x) (> len 1)
                 (string->number (substring tok 1) 16))
            ;; Specifying a codepoint as an hexadecimal value.  Skip
            ;; initial "x".
            => integer->char)
           ((named-char tok r5rs-charnames))
           ((named-char tok r6rs-charnames))
           ((named-char tok r7rs-charnames))
           ((named-char tok C0-control-charnames))
           ((named-char tok alt-charnames))
           (else
            (input-error "unknown character name ~a" tok))))))))

  (define (read-vector)
    (list->vector (read-parenthesized #\))))

  (define (read-srfi-4-vector ch)
    (read-array ch))

  (define (maybe-read-boolean-tail tail)
    (let ((len (string-length tail)))
      (let lp ((i 0))
        (or (= i len)
            (let ((ch (peek)))
              (and (not (eof-object? ch))
                   (eqv? (char-downcase ch) (string-ref tail i))
                   (or (begin
                         (next)
                         (lp (1+ i)))
                       (begin
                         (unget-char port ch)
                         #f))))))))

  (define (read-false-or-srfi-4-vector)
    (let ((ch (peek)))
      (if (or (eqv? ch #\3)
              (eqv? ch #\6))
          (read-srfi-4-vector #\f)
          (begin
            (maybe-read-boolean-tail "alse")
            #f))))

  (define (read-bytevector)
    (define (expect ch)
      (unless (eqv? (next) ch)
        (input-error "invalid bytevector prefix" ch)))
    (expect #\u)
    (expect #\8)
    (expect #\()
    (u8-list->bytevector (read-parenthesized #\))))

  ;; FIXME: We should require a terminating delimiter.
  (define (read-bitvector)
    (list->bitvector
     (let lp ()
       (let ((ch (peek)))
         (case ch
           ((#\0) (next) (cons #f (lp)))
           ((#\1) (next) (cons #t (lp)))
           (else '()))))))

  (define (read-boolean ch)
    ;; Historically, Guile hasn't required a delimiter after #f / #t.
    ;; When the longer #false / #true forms were added, we kept this
    ;; behavior.  It is terrible and we should change it!!
    (case ch
      ((#\t #\T)
       (maybe-read-boolean-tail "rue")
       #t)
      (else
       (maybe-read-boolean-tail "alse")
       #f)))

  (define (read-keyword)
    (let ((ch (next-non-whitespace)))
      (when (eof-object? ch)
        (input-error "end of input while reading keyword"))
      (let ((expr (read-expr ch)))
        (unless (symbol? expr)
          (input-error "keyword prefix #: not followed by a symbol: ~a"
                       expr))
        (symbol->keyword expr))))

  (define (read-array ch)
    (define (read-decimal-integer ch alt)
      ;; This parser has problems but it's what Guile's read.c does.  Any
      ;; fix should come later and to both of them.
      (define (decimal-digit ch)
        (and (not (eof-object? ch))
             (let ((digit (- (char->integer ch) (char->integer #\0))))
               (and (<= 0 digit 9) digit))))
      (let-values (((sign ch) (if (eqv? ch #\-)
                                  (values -1 (next))
                                  (values 1 ch))))
        (let lp ((ch ch) (res #f))
          (cond
           ((decimal-digit ch)
            => (lambda (digit)
                 (lp (next) (if res (+ (* 10 res) digit) digit))))
           (else
            (values ch (if res (* res sign) alt)))))))
    (define (read-rank ch)
      (let-values (((ch rank) (read-decimal-integer ch 1)))
        (when (< rank 0)
          (input-error "array rank must be non-negative"))
        (when (eof-object? ch)
          (input-error "unexpected end of input while reading array"))
        (values ch rank)))
    (define (read-tag ch)
      (let lp ((ch ch) (chars '()))
        (when (eof-object? ch)
          (input-error "unexpected end of input while reading array"))
        (if (memv ch '(#\( #\@ @\:))
            (values ch
                    (if (null? chars)
                        #t
                        (string->symbol (list->string (reverse chars)))))
            (lp (next) (cons ch chars)))))
    (define (read-dimension ch)
      (let*-values (((ch lbnd) (if (eqv? ch #\@)
                                   (read-decimal-integer (next) 0)
                                   (values ch 0)))
                    ((ch len) (if (eqv? ch #\:)
                                  (read-decimal-integer (next) 0)
                                  (values ch #f))))
        (when (and len (< len 0))
          (input-error "array length must be non-negative"))
        (when (eof-object? ch)
          (input-error "unexpected end of input while reading array"))
        (values ch
                (if len
                    (if (zero? lbnd)
                        len
                        (list lbnd (+ lbnd (1- len))))
                    lbnd))))
    (define (read-shape ch alt)
      (if (memv ch '(#\@ @\:))
          (let*-values (((ch head) (read-dimension ch))
                        ((ch tail) (read-shape ch '())))
            (values ch (cons head tail)))
          (values ch alt)))
    (define (read-elements ch rank)
      (unless (eqv? ch #\()
        (input-error "missing '(' in vector or array literal"))
      (let ((elts (read-parenthesized #\))))
        (if (zero? rank)
            (begin
              ;; Handle special print syntax of rank zero arrays; see
              ;; scm_i_print_array for a rationale.
              (when (null? elts)
                (input-error "too few elements in array literal, need 1"))
              (unless (null? (cdr elts))
                (input-error "too many elements in array literal, need 1"))
              (car elts))
            elts)))
    (let*-values (((ch rank) (read-rank ch))
                  ((ch tag) (read-tag ch))
                  ((ch shape) (read-shape ch rank))
                  ((elts) (read-elements ch rank)))
      (when (and (pair? shape) (not (eqv? (length shape) rank)))
        (input-error
         "the number of shape specifications must match the array rank"))
      (list->typed-array tag shape elts)))

  (define (read-number-and-radix ch)
    (let ((tok (string-append "#" (read-token ch))))
      (or (string->number tok)
          (input-error "unknown # object"))))

  (define (read-extended-symbol)
    (define (next-not-eof)
      (let ((ch (next)))
        (when (eof-object? ch)
          (input-error "end of input while reading symbol"))
        ch))
    (string->symbol
     (list->string
      (let lp ((saw-brace? #f))
        (let ((ch (next-not-eof)))
          (cond
           (saw-brace?
            (if (eqv? ch #\#)
                '()
                (cons #\} (lp #f))))
           ((eqv? ch #\})
            (lp #t))
           ((eqv? ch #\\)
            ;; It used to be that print.c would print extended-read-syntax
            ;; symbols with backslashes before "non-standard" chars, but
            ;; this routine wouldn't do anything with those escapes.
            ;; Bummer.  What we've done is to change print.c to output
            ;; R6RS hex escapes for those characters, relying on the fact
            ;; that the extended read syntax would never put a `\' before
            ;; an `x'.  For now, we just ignore other instances of
            ;; backslash in the string.
            (let* ((ch (next-not-eof))
                   (ch (if (eqv? ch #\x)
                           (read-r6rs-hex-escape)
                           ch)))
              (cons ch (lp #f))))
           (else
            (cons ch (lp #f)))))))))

  (define (read-nil)
    ;; Have already read "#\n" -- now read "il".
    (let ((id (read-mixed-case-symbol #\n)))
      (unless (eq? id 'nil)
        (input-error "unexpected input while reading #nil: ~a" id))
      #nil))

  (define (read-sharp)
    (let* ((ch (next)))
      (cond
       ((eof-object? ch)
        (input-error "unexpected end of input after #"))
       ((read-hash-procedure ch)
        => (lambda (proc) (proc ch)))
       (else
        (case ch
          ((#\\) (read-character))
          ((#\() (read-vector))
          ((#\s #\u \c) (read-srfi-4-vector ch))
          ((#\f) (read-false-or-srfi-4-vector))
          ((#\v) (read-bytevector))
          ((#\*) (read-bitvector))
          ((#\t #\T #\F) (read-boolean ch))
          ((#\:) (read-keyword))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\@)
           (read-array ch))
          ((#\i #\e #\b #\B #\o #\O #\d #\D #\x #\X #\I #\E)
           (read-number-and-radix ch))
          ((#\{) (read-extended-symbol))
          ((#\') (list 'syntax (read-expr (next-non-whitespace))))
          ((#\`) (list 'quasisyntax (read-expr (next-non-whitespace))))
          ((#\,)
           (if (eqv? #\@ (peek))
               (begin
                 (next)
                 (list 'unsyntax-splicing (read-expr (next-non-whitespace))))
               (list 'unsyntax (read-expr (next-non-whitespace)))))
          ((#\n) (read-nil))
          (else
           (input-error "Unknown # object: ~S" ch)))))))

  (define (read-number ch)
    (let* ((str (read-token ch)))
      (or (string->number str)
          (string->symbol (if (case-insensitive?)
                              (string-downcase str)
                              str)))))

  (define (read-expr* ch)
    (case ch
      ((#\{)
       (cond
        ((curly-infix?)
         (set! neoteric (1+ neoteric))
         (let ((expr (read-parenthesized #\})))
           (set! neoteric (1- neoteric))
           expr))
        (else
         (read-mixed-case-symbol ch))))
      ((#\[)
       (cond
        ((square-brackets?)
         (read-parenthesized #\]))
        ((curly-infix?)
         ;; The syntax of neoteric expressions requires that '[' be a
         ;; delimiter when curly-infix is enabled, so it cannot be part
         ;; of an unescaped symbol.  We might as well do something
         ;; useful with it, so we adopt Kawa's convention:  [...] =>
         ;; ($bracket-list$ ...)
         ;; FIXME: source locations for this cons
         (cons '$bracket-list$ (read-parenthesized #\])))
        (else
         (read-mixed-case-symbol ch))))
      ((#\()
       (read-parenthesized #\)))
      ((#\")
       (read-string ch))
      ((#\|)
       (if (r7rs-symbols?)
           (string->symbol (read-string ch))
           (read-mixed-case-symbol ch)))
      ((#\')
       (list 'quote (read-expr (next-non-whitespace))))
      ((#\`)
       (list 'quasiquote (read-expr (next-non-whitespace))))
      ((#\,)
       (cond
        ((eqv? #\@ (peek))
         (next)
         (list 'unquote-splicing (read-expr (next-non-whitespace))))
        (else
         (list 'unquote (read-expr (next-non-whitespace))))))
      ((#\#)
       ;; FIXME: read-sharp should recur if we read a comment
       (read-sharp))
      ((#\))
       (input-error "unexpected \")\""))
      ((#\})
       (if (curly-infix?)
           (input-error "unexpected \"}\"")
           (read-mixed-case-symbol ch)))
      ((#\])
       (if (square-brackets?)
           (input-error "unexpected \"]\"")
           (read-mixed-case-symbol ch)))
      ((#f)
       ;; EOF.
       the-eof-object)
      ((#\:)
       (if (eq? (keyword-style) keyword-style-prefix)
           ;; FIXME: Don't skip whitespace here.
           (let ((ch (next-non-whitespace)))
             (when (eof-object? ch)
               (input-error "unexpected end of input while reading :keyword"))
             (symbol->keyword (read-expr ch)))
           (read-mixed-case-symbol ch)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\.)
       (read-number ch))
      (else
       (read-mixed-case-symbol ch))))

  (define (read-neoteric ch)
    (let lp ((expr (read-expr ch)))
      ;; 'expr' is the first component of the neoteric expression.  If
      ;; the next character is '(', '[', or '{', (without any
      ;; intervening whitespace), we use it to construct a new
      ;; expression, and loop.  For example:
      ;; f{n - 1}(x) => ((f (- n 1)) x).
      (case (peek)
        ((#\() ;; e(...) => (e ...)
         (next)
         (lp (cons expr (read-parenthesized #\)))))
        ((#\[) ;; e[...] => ($bracket-apply$ e ...)
         (next)
         (lp (cons* '$bracket-apply$ expr (read-parenthesized #\]))))
        ((#\{) ;; e{} => (e); e{...} => (e {...})
         (next)
         (let ((args (read-parenthesized #\})))
           (lp (if (null? args)
                   (list expr)
                   (list expr args)))))
        (else
         expr))))

  (define (read-expr ch)
    (let ((line (port-line port))
          (column (port-column port)))
      (annotate
       line
       column
       (if (zero? neoteric)
           (read-expr* ch)
           (read-neoteric ch)))))

  (define (read-directive)
    (let ((ch (next)))
      (cond
       ((eof-object? ch)
        (input-error "unexpected end of input after #!"))
       (else
        (string->symbol
         (take-while ch (lambda (ch)
                          (or (eqv? ch #\-)
                              (char-alphabetic? ch)
                              (char-numeric? ch)))))))))

  (define (skip-scsh-comment)
    (let lp ((ch (next)))
      (cond
       ((eof-object? ch)
        (input-error "unexpected end of input while looking for !#"))
       ((eqv? ch #\!)
        (let ((ch (next)))
          (if (eqv? ch #\#)
              (next)
              (lp ch))))
       (else
        (lp (next))))))

  (define (process-shebang)
    ;; After having read #!, we complete either with #!r6rs,
    ;; #!fold-case, #!no-fold-case, #!curly-infix,
    ;; #!curly-infix-and-bracket-lists, or a SCSH block comment
    ;; terminated by !#.
    (let ((sym (read-directive)))
      (cond
       ((eq? sym 'r6rs)
        (set-reader-option! bitfield:case-insensitive? 0)
        (set-reader-option! bitfield:r6rs-escapes? 1)
        (set-reader-option! bitfield:square-brackets? 1)
        (set-reader-option! bitfield:keyword-style keyword-style-hash-prefix)
        (set-reader-option! bitfield:hungry-eol-escapes? 1)
        (next))
       ((eq? sym 'fold-case)
        (set-reader-option! bitfield:case-insensitive? 1)
        (next))
       ((eq? sym 'no-fold-case)
        (set-reader-option! bitfield:case-insensitive? 0)
        (next))
       ((eq? sym 'curly-infix)
        (set-reader-option! bitfield:curly-infix? 1)
        (next))
       ((eq? sym 'curly-infix-and-bracket-lists)
        (set-reader-option! bitfield:curly-infix? 1)
        (set-reader-option! bitfield:square-brackets? 0)
        (next))
       (else
        (skip-scsh-comment)))))

  (define (skip-eol-comment)
    (let ((ch (next)))
      (cond
       ((eof-object? ch) ch)
       ((eq? ch #\newline) (next))
       (else (skip-eol-comment)))))

  ;; Unlike SCSH-style block comments, SRFI-30/R6RS block comments may be
  ;; nested.
  (define (skip-r6rs-block-comment)
    ;; We have read #|, now looking for |#.
    (let ((ch (next)))
      (when (eof-object? ch)
        (input-error "unterminated `#| ... |#' comment"))
      (cond
       ((and (eqv? ch #\|) (eqv? (peek) #\#))
        ;; Done.
        (next)
        (values))
       ((and (eqv? ch #\#) (eqv? (peek) #\|))
        ;; A nested comment.
        (next)
        (skip-r6rs-block-comment)
        (skip-r6rs-block-comment))
       (else
        (skip-r6rs-block-comment)))))

  (define (next-non-whitespace)
    (let lp ((ch (next)))
      (case ch
        ((#\;)
         (lp (skip-eol-comment)))
        ((#\#)
         (case (peek)
           ((#\!)
            (next)
            (lp (process-shebang)))
           ((#\;)
            (next)
            (let ((ch (next-non-whitespace)))
              (when (eof-object? ch)
                (input-error "no expression after #; comment"))
              (read-expr ch))
            (next-non-whitespace))
           ((#\|)
            (if (read-hash-procedure #\|)
                ch
                (begin
                  (next)
                  (skip-r6rs-block-comment)
                  (next-non-whitespace))))
           (else ch)))
        ((#\space #\return #\ff #\newline #\tab)
         (next-non-whitespace))
        (else ch))))

  (let ((ch (next-non-whitespace)))
    (if (eof-object? ch)
        ch
        (read-expr ch))))
