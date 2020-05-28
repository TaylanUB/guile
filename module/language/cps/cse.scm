;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; Common subexpression elimination for CPS.
;;;
;;; Code:

(define-module (language cps cse)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:export (eliminate-common-subexpressions))

(define (compute-available-expressions succs kfun clobbers)
  "Compute and return a map of LABEL->ANCESTOR..., where ANCESTOR... is
an intset containing ancestor labels whose value is available at LABEL."
  (let ((init (intmap-map (lambda (label succs) #f) succs))
        (kill clobbers)
        (gen (intmap-map (lambda (label succs) (intset label)) succs))
        (subtract (lambda (in-1 kill-1)
                    (if in-1
                        (intset-subtract in-1 kill-1)
                        empty-intset)))
        (add intset-union)
        (meet (lambda (in-1 in-1*)
                (if in-1
                    (intset-intersect in-1 in-1*)
                    in-1*))))
    (let ((in (intmap-replace init kfun empty-intset))
          (out init)
          (worklist (intset kfun)))
      (solve-flow-equations succs in out kill gen subtract add meet worklist))))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define-syntax-rule (make-worklist-folder* seed ...)
  (lambda (f worklist seed ...)
    (let lp ((worklist worklist) (seed seed) ...)
      (call-with-values (lambda () (intset-pop worklist))
        (lambda (worklist i)
          (if i
              (call-with-values (lambda () (f i seed ...))
                (lambda (i* seed ...)
                  (let add ((i* i*) (worklist worklist))
                    (match i*
                      (() (lp worklist seed ...))
                      ((i . i*) (add i* (intset-add worklist i)))))))
              (values seed ...)))))))

(define worklist-fold*
  (case-lambda
    ((f worklist seed)
     ((make-worklist-folder* seed) f worklist seed))))

(define (compute-truthy-expressions conts kfun)
  "Compute a \"truth map\", indicating which expressions can be shown to
be true and/or false at each label in the function starting at KFUN.
Returns an intmap of intsets.  The even elements of the intset indicate
labels that may be true, and the odd ones indicate those that may be
false.  It could be that both true and false proofs are available."
  (define (true-idx label) (ash label 1))
  (define (false-idx label) (1+ (ash label 1)))

  (define (propagate boolv succ out)
    (let* ((in (intmap-ref boolv succ (lambda (_) #f)))
           (in* (if in (intset-union in out) out)))
      (if (eq? in in*)
          (values '() boolv)
          (values (list succ)
                  (intmap-add boolv succ in* (lambda (old new) new))))))

  (define (visit-cont label boolv)
    (let ((in (intmap-ref boolv label)))
      (define (propagate0)
        (values '() boolv))
      (define (propagate1 succ)
        (propagate boolv succ in))
      (define (propagate2 succ0 succ1)
        (let*-values (((changed0 boolv) (propagate boolv succ0 in))
                      ((changed1 boolv) (propagate boolv succ1 in)))
          (values (append changed0 changed1) boolv)))
      (define (propagate-branch succ0 succ1)
        (let*-values (((changed0 boolv)
                       (propagate boolv succ0
                                  (intset-add in (false-idx label))))
                      ((changed1 boolv)
                       (propagate boolv succ1
                                  (intset-add in (true-idx label)))))
          (values (append changed0 changed1) boolv)))

      (match (intmap-ref conts label)
        (($ $kargs names vars term)
         (match term
           (($ $continue k)   (propagate1 k))
           (($ $branch kf kt) (propagate-branch kf kt))
           (($ $prompt k kh)  (propagate2 k kh))
           (($ $throw)        (propagate0))))
        (($ $kreceive arity k)
         (propagate1 k))
        (($ $kfun src meta self tail clause)
         (if clause
             (propagate1 clause)
             (propagate0)))
        (($ $kclause arity kbody kalt)
         (if kalt
             (propagate2 kbody kalt)
             (propagate1 kbody)))
        (($ $ktail) (propagate0)))))

  (worklist-fold* visit-cont
                  (intset kfun)
                  (intmap-add empty-intmap kfun empty-intset)))

(define (eliminate-common-subexpressions-in-fun kfun conts out)
  (let* ((effects (synthesize-definition-effects (compute-effects conts)))
         (clobbers (compute-clobber-map effects))
         (succs (compute-successors conts kfun))
         (preds (invert-graph succs))
         (avail (compute-available-expressions succs kfun clobbers))
         (truthy-labels (compute-truthy-expressions conts kfun))
         (equiv-set (make-hash-table)))
    (define (true-idx idx) (ash idx 1))
    (define (false-idx idx) (1+ (ash idx 1)))
    (define (subst-var var-substs var)
      (intmap-ref var-substs var (lambda (var) var)))
    (define (subst-vars var-substs vars)
      (let lp ((vars vars))
        (match vars
          (() '())
          ((var . vars) (cons (subst-var var-substs var) (lp vars))))))

    (define (compute-term-key term)
      (match term
        (($ $continue k src exp)
         (match exp
           (($ $const val)                       (cons 'const val))
           (($ $prim name)                       (cons 'prim name))
           (($ $fun body)                        #f)
           (($ $rec names syms funs)             #f)
           (($ $const-fun label)                 #f)
           (($ $code label)                      (cons 'code label))
           (($ $call proc args)                  #f)
           (($ $callk k proc args)               #f)
           (($ $primcall name param args)        (cons* name param args))
           (($ $values args)                     #f)))
        (($ $branch kf kt src op param args)     (cons* op param args))
        (($ $prompt)                             #f)
        (($ $throw)                              #f)))

    (define (add-var-substs label defs out var-substs)
      (match (trivial-intset (intmap-ref preds label))
        (#f var-substs)
        (pred
         (match (intmap-ref out pred)
           (($ $kargs _ _ ($ $continue _ _ ($ $values vals)))
            ;; FIXME: Eliminate predecessor entirely, retargetting its
            ;; predecessors.
            (fold (lambda (def var var-substs)
                    (intmap-add var-substs def var))
                  var-substs defs vals))
           (($ $kargs _ _ term)
            (match (compute-term-key term)
              (#f #f)
              (term-key
               (let ((fx (intmap-ref effects pred)))
                 ;; Add residualized definition to the equivalence set.
                 ;; Note that expressions that allocate a fresh object
                 ;; or change the current fluid environment can't be
                 ;; eliminated by CSE (though DCE might do it if the
                 ;; value proves to be unused, in the allocation case).
                 (when (and (not (causes-effect? fx &allocation))
                            (not (effect-clobbers? fx (&read-object &fluid))))
                   (let ((equiv (hash-ref equiv-set term-key '())))
                     (hash-set! equiv-set term-key (acons pred defs equiv)))))
               ;; If the predecessor defines auxiliary definitions, as
               ;; `cons' does for the results of `car' and `cdr', define
               ;; those as well.
               (add-auxiliary-definitions! pred defs var-substs term-key)))
            var-substs)
           (_
            var-substs)))))

    (define (add-auxiliary-definitions! label defs var-substs term-key)
      (let ((defs (and defs (subst-vars var-substs defs))))
        (define (add-def! aux-key var)
          (let ((equiv (hash-ref equiv-set aux-key '())))
            (hash-set! equiv-set aux-key
                       (acons label (list var) equiv))))
        (define-syntax add-definitions
          (syntax-rules (<-)
            ((add-definitions)
             #f)
            ((add-definitions
              ((def <- op arg ...) (aux <- op* arg* ...) ...)
              . clauses)
             (match term-key
               (('op arg ...)
                (match defs
                  (#f
                   ;; If the successor is a control-flow join, don't
                   ;; pretend to know the values of its defs.
                   #f)
                  ((def) (add-def! (list 'op* arg* ...) aux) ...)))
               (_ (add-definitions . clauses))))
            ((add-definitions
              ((op arg ...) (aux <- op* arg* ...) ...)
              . clauses)
             (match term-key
               (('op arg ...)
                (add-def! (list 'op* arg* ...) aux) ...)
               (_ (add-definitions . clauses))))))
        (add-definitions
         ((scm-set! p s i x)               (x <- scm-ref p s i))
         ((scm-set!/tag p s x)             (x <- scm-ref/tag p s))
         ((scm-set!/immediate p s x)       (x <- scm-ref/immediate p s))
         ((word-set! p s i x)              (x <- word-ref p s i))
         ((word-set!/immediate p s x)      (x <- word-ref/immediate p s))
         ((pointer-set!/immediate p s x)   (x <- pointer-ref/immediate p s))

         ((u <- scm->f64 #f s)             (s <- f64->scm #f u))
         ((s <- f64->scm #f u)             (u <- scm->f64 #f s))
         ((u <- scm->u64 #f s)             (s <- u64->scm #f u))
         ((s <- u64->scm #f u)             (u <- scm->u64 #f s)
          (u <- scm->u64/truncate #f s))
         ((s <- u64->scm/unlikely #f u)    (u <- scm->u64 #f s)
          (u <- scm->u64/truncate #f s))
         ((u <- scm->s64 #f s)             (s <- s64->scm #f u))
         ((s <- s64->scm #f u)             (u <- scm->s64 #f s))
         ((s <- s64->scm/unlikely #f u)    (u <- scm->s64 #f s))
         ((u <- untag-fixnum #f s)         (s <- s64->scm #f u)
          (s <- tag-fixnum #f u))
         ;; NB: These definitions rely on U having top 2 bits equal to
         ;; 3rd (sign) bit.
         ((s <- tag-fixnum #f u)           (u <- scm->s64 #f s)
          (u <- untag-fixnum #f s))
         ((s <- u64->s64 #f u)             (u <- s64->u64 #f s))
         ((u <- s64->u64 #f s)             (s <- u64->s64 #f u))

         ((u <- untag-char #f s)           (s <- tag-char #f u))
         ((s <- tag-char #f u)             (u <- untag-char #f s)))))

    (define (rename-uses term var-substs)
      (define (subst-var var)
        (intmap-ref var-substs var (lambda (var) var)))
      (define (rename-exp exp)
        (rewrite-exp exp
          ((or ($ $const) ($ $prim) ($ $fun) ($ $rec) ($ $const-fun) ($ $code))
           ,exp)
          (($ $call proc args)
           ($call (subst-var proc) ,(map subst-var args)))
          (($ $callk k proc args)
           ($callk k (and proc (subst-var proc)) ,(map subst-var args)))
          (($ $primcall name param args)
           ($primcall name param ,(map subst-var args)))
          (($ $values args)
           ($values ,(map subst-var args)))))
      (rewrite-term term
        (($ $branch kf kt src op param args)
         ($branch kf kt src op param ,(map subst-var args)))
        (($ $continue k src exp)
         ($continue k src ,(rename-exp exp)))
        (($ $prompt k kh src escape? tag)
         ($prompt k kh src escape? (subst-var tag)))
        (($ $throw src op param args)
         ($throw src op param ,(map subst-var args)))))

    (define (visit-label label cont out var-substs)
      (define (add cont)
        (intmap-add! out label cont))
      (match cont
        (($ $kargs names vars term)
         (let* ((var-substs (add-var-substs label vars out var-substs))
                (term (rename-uses term var-substs)))
           (define (residualize)
             (add (build-cont ($kargs names vars ,term))))
           (define (eliminate k src vals)
             (add (build-cont ($kargs names vars
                                ($continue k src ($values vals))))))

           (values
            (match (compute-term-key term)
              (#f (residualize))
              (term-key
               (let ((avail (intmap-ref avail label)))
                 (let lp ((candidates (hash-ref equiv-set term-key '())))
                   (match candidates
                     (()
                      ;; No available expression; residualize.
                      (residualize))
                     (((candidate . vars) . candidates)
                      (cond
                       ((not (intset-ref avail candidate))
                        ;; This expression isn't available here; try
                        ;; the next one.
                        (lp candidates))
                       (else
                        (match term
                          (($ $continue k src)
                           ;; Yay, a match; eliminate the expression.
                           (eliminate k src vars))
                          (($ $branch kf kt src)
                           (let* ((bool (intmap-ref truthy-labels label))
                                  (t (intset-ref bool (true-idx candidate)))
                                  (f (intset-ref bool (false-idx candidate))))
                             (if (eqv? t f)
                                 ;; Can't fold the branch; keep on
                                 ;; looking for another candidate.
                                 (lp candidates)
                                 ;; Nice, the branch folded.
                                 (eliminate (if t kt kf) src '())))))))))))))
            var-substs)))
        (_ (values (add cont) var-substs))))

    ;; Because of the renumber pass, the labels are numbered in reverse
    ;; post-order, so the intmap-fold will visit definitions before
    ;; uses.
    (intmap-fold visit-label conts out empty-intmap)))

(define (fold-renumbered-functions f conts seed)
  ;; Precondition: CONTS has been renumbered, and therefore functions
  ;; contained within it are topologically sorted, and the conts of each
  ;; function's body are numbered sequentially after the function's
  ;; $kfun.
  (define (next-function-body kfun)
    (match (intmap-ref conts kfun (lambda (_) #f))
      (#f #f)
      ((and cont ($ $kfun))
       (let lp ((k (1+ kfun)) (body (intmap-add! empty-intmap kfun cont)))
         (match (intmap-ref conts k (lambda (_) #f))
           ((or #f ($ $kfun))
            (persistent-intmap body))
           (cont
            (lp (1+ k) (intmap-add! body k cont))))))))

  (let fold ((kfun 0) (seed seed))
    (match (next-function-body kfun)
      (#f seed)
      (conts
       (fold (1+ (intmap-prev conts)) (f kfun conts seed))))))

(define (eliminate-common-subexpressions conts)
  (let ((conts (renumber conts 0)))
    (persistent-intmap
     (fold-renumbered-functions eliminate-common-subexpressions-in-fun
                                conts empty-intmap))))
