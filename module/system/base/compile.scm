;;; High-level compiler interface

;; Copyright (C) 2001,2005,2008-2013,2016,2020 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-module (system base compile)
  #:use-module (system base language)
  #:use-module (system base message)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:export (compiled-file-name
            compile-file
            compile-and-load
            read-and-compile
            compile
            decompile))


;;;
;;; Compiler
;;;

(define (call-once thunk)
  (let ((entered #f))
    (dynamic-wind
        (lambda ()
          (when entered
            (error "thunk may only be entered once: ~a" thunk))
          (set! entered #t))
        thunk
        (lambda () #t))))

;; emacs: (put 'call-with-output-file/atomic 'scheme-indent-function 1)
(define* (call-with-output-file/atomic filename proc #:optional reference)
  (let* ((template (string-append filename ".XXXXXX"))
         (tmp (mkstemp! template "wb")))
    (call-once
     (lambda ()
       (with-throw-handler #t
         (lambda ()
           (proc tmp)
           ;; Chmodding by name instead of by port allows this chmod to
           ;; work on systems without fchmod, like MinGW.
           (let ((perms (or (false-if-exception (stat:perms (stat reference)))
                            (lognot (umask)))))
             (chmod template (logand #o0666 perms)))
           (close-port tmp)
           (rename-file template filename))
         (lambda args
           (close-port tmp)
           (delete-file template)))))))

(define (ensure-language x)
  (if (language? x)
      x
      (lookup-language x)))

;; Throws an exception if `dir' is not writable.  The mkdir occurs
;; before the check, so that we avoid races (possibly due to parallel
;; compilation).
;;
(define (ensure-directory dir)
  (catch 'system-error
    (lambda ()
      (mkdir dir))
    (lambda (k subr fmt args rest)
      (let ((errno (and (pair? rest) (car rest))))
        (cond
         ((eqv? errno EEXIST)
          ;; Assume it's a writable directory, to avoid TOCTOU errors,
          ;; as well as UID/EUID mismatches that occur with access(2).
          #t)
         ((eqv? errno ENOENT)
          (ensure-directory (dirname dir))
          (ensure-directory dir))
         (else
          (throw k subr fmt args rest)))))))

;;; This function is among the trickiest I've ever written. I tried many
;;; variants. In the end, simple is best, of course.
;;;
;;; After turning this around a number of times, it seems that the
;;; desired behavior is that .go files should exist in a path, for
;;; searching. That is orthogonal to this function. For writing .go
;;; files, either you know where they should go, in which case you tell
;;; compile-file explicitly, as in the srcdir != builddir case; or you
;;; don't know, in which case this function is called, and we just put
;;; them in your own ccache dir in ~/.cache/guile/ccache.
;;;
;;; See also boot-9.scm:load.
(define (compiled-file-name file)
  ;; FIXME: would probably be better just to append SHA1(canon-path)
  ;; to the %compile-fallback-path, to avoid deep directory stats.
  (define (canonical->suffix canon)
    (cond
     ((string-prefix? "/" canon) canon)
     ((and (> (string-length canon) 2)
           (eqv? (string-ref canon 1) #\:))
      ;; Paths like C:... transform to /C...
      (string-append "/" (substring canon 0 1) (substring canon 2)))
     (else canon)))
  (define (compiled-extension)
    (cond ((or (null? %load-compiled-extensions)
               (string-null? (car %load-compiled-extensions)))
           (warn "invalid %load-compiled-extensions"
                 %load-compiled-extensions)
           ".go")
          (else (car %load-compiled-extensions))))
  (and %compile-fallback-path
       (let ((f (string-append
                 %compile-fallback-path
                 (canonical->suffix (canonicalize-path file))
                 (compiled-extension))))
         (and (false-if-exception (ensure-directory (dirname f)))
              f))))

(define (validate-options opts)
  (define (validate-warnings warnings)
    (match warnings
      (() (values))
      ((w . warnings)
       (unless (lookup-warning-type w)
         (warning 'unsupported-warning #f w))
       (validate-warnings warnings))))
  (match opts
    (() (values))
    ((kw arg . opts)
     (match kw
       (#:warnings (validate-warnings arg))
       ((? keyword?) (values))
       (_
        ;; Programming error.
        (warn "malformed options list: not a keyword" kw)))
     (validate-options opts))
    (_
     ;; Programming error.
     (warn "malformed options list: expected keyword and arg pair" opts))))

(define* (compile-file file #:key
                       (output-file #f)
                       (from (current-language))
                       (to 'bytecode)
                       (env (default-environment from))
                       (opts '())
                       (canonicalization 'relative))
  (validate-options opts)
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (let* ((comp (or output-file (compiled-file-name file)
                     (error "failed to create path for auto-compiled file"
                            file)))
           (in (open-input-file file))
           (enc (file-encoding in)))
      ;; Choose the input encoding deterministically.
      (set-port-encoding! in (or enc "UTF-8"))

      (ensure-directory (dirname comp))
      (call-with-output-file/atomic comp
        (lambda (port)
          ((language-printer (ensure-language to))
           (read-and-compile in #:env env #:from from #:to to #:opts
                             (cons* #:to-file? #t opts))
           port))
        file)
      comp)))

(define* (compile-and-load file #:key (from (current-language)) (to 'value)
                           (env (current-module)) (opts '())
                           (canonicalization 'relative))
  (validate-options opts)
  (with-fluids ((%file-port-name-canonicalization canonicalization))
    (read-and-compile (open-input-file file)
                      #:from from #:to to #:opts opts
                      #:env env)))


;;;
;;; Compiler interface
;;;

(define (compile-passes from to opts)
  (match (lookup-compilation-order from to)
    (((langs . passes) ...) passes)
    (_ (error "no way to compile" from "to" to))))

(define (compile-fold passes exp env opts)
  (let lp ((passes passes) (x exp) (e env) (cenv env) (first? #t))
    (match passes
      (() (values x e cenv))
      ((pass . passes)
       (receive (x e new-cenv) (pass x e opts)
         (lp passes x e (if first? new-cenv cenv) #f))))))

(define (find-language-joint from to)
  (match (lookup-compilation-order from to)
    (((langs . passes) ...)
     (or (let lp ((langs langs))
           (match langs
             (() #f)
             ((lang . langs)
              (or (lp langs)
                  (and (language-joiner lang)
                       lang)))))
         to))
    (_ (error "no way to compile" from "to" to))))

(define (default-language-joiner lang)
  (lambda (exps env)
    (match exps
      ((exp) exp)
      (_
       (error
        "Multiple expressions read and compiled, but language has no joiner"
        lang)))))

(define (read-and-parse lang port cenv)
  (let ((exp ((language-reader lang) port cenv)))
    (cond
     ((eof-object? exp) exp)
     ((language-parser lang) => (lambda (parse) (parse exp)))
     (else exp))))

(define* (read-and-compile port #:key
                           (from (current-language))
                           (to 'bytecode)
                           (env (default-environment from))
                           (opts '()))
  (let ((from (ensure-language from))
        (to (ensure-language to)))
    (let ((joint (find-language-joint from to)))
      (parameterize ((current-language from))
        (let lp ((exps '()) (env #f) (cenv env))
          (let ((x (read-and-parse (current-language) port cenv)))
            (cond
             ((eof-object? x)
              (close-port port)
              (compile ((or (language-joiner joint)
                            (default-language-joiner joint))
                        (reverse exps)
                        env)
                       #:from joint #:to to
                       ;; env can be false if no expressions were read.
                       #:env (or env (default-environment joint))
                       #:opts opts))
             (else
              ;; compile-fold instead of compile so we get the env too
              (receive (jexp jenv jcenv)
                  (compile-fold (compile-passes (current-language) joint opts)
                                x cenv opts)
                (lp (cons jexp exps) jenv jcenv))))))))))

(define* (compile x #:key
                  (from (current-language))
                  (to 'value)
                  (env (default-environment from))
                  (opts '()))
  (validate-options opts)
  (receive (exp env cenv)
      (compile-fold (compile-passes from to opts) x env opts)
    exp))


;;;
;;; Decompiler interface
;;;

(define (decompile-passes from to opts)
  (match (lookup-decompilation-order from to)
    (((langs . passes) ...) passes)
    (_ (error "no way to decompile" from "to" to))))

(define (decompile-fold passes exp env opts)
  (match passes
    (() (values exp env))
    ((pass . passes)
     (receive (exp env) (pass exp env opts)
       (decompile-fold passes exp env opts)))))

(define* (decompile x #:key
                    (env #f)
                    (from 'tree-il)
                    (to 'scheme)
                    (opts '()))
  (decompile-fold (decompile-passes from to opts)
                  x
                  env
                  opts))
