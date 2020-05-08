;;; TREE-IL -> GLIL compiler

;; Copyright (C) 2001,2008-2014,2016,2018-2020 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language tree-il analyze)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (system base syntax)
  #:use-module (system base message)
  #:use-module (system vm program)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:export (analyze-tree
            unused-variable-analysis
            unused-toplevel-analysis
            shadowed-toplevel-analysis
            unbound-variable-analysis
            macro-use-before-definition-analysis
            arity-analysis
            format-analysis
            make-analyzer))

;;;
;;; Tree analyses for warnings.
;;;

(define-record-type <tree-analysis>
  (make-tree-analysis down up post init)
  tree-analysis?
  (down tree-analysis-down)  ;; (lambda (x result env locs) ...)
  (up   tree-analysis-up)    ;; (lambda (x result env locs) ...)
  (post tree-analysis-post)  ;; (lambda (result env) ...)
  (init tree-analysis-init)) ;; arbitrary value

(define (analyze-tree analyses tree env)
  "Run all tree analyses listed in ANALYSES on TREE for ENV, using
`tree-il-fold'.  Return TREE.  The down and up procedures of each
analysis are passed a ``location stack', which is the stack of
`tree-il-src' values for each parent tree (a list); it can be used to
approximate source location when accurate information is missing from a
given `tree-il' element."

  (define (traverse proc update-locs)
    ;; Return a tree traversing procedure that returns a list of analysis
    ;; results prepended by the location stack.
    (lambda (x results)
      (let ((locs (update-locs x (car results))))
        (cons locs ;; the location stack
              (map (lambda (analysis result)
                     ((proc analysis) x result env locs))
                   analyses
                   (cdr results))))))

  ;; Extending and shrinking the location stack.
  (define (extend-locs x locs) (cons (tree-il-src x) locs))
  (define (shrink-locs x locs) (cdr locs))

  (let ((results
         (tree-il-fold (traverse tree-analysis-down extend-locs)
                       (traverse tree-analysis-up   shrink-locs)
                       (cons '() ;; empty location stack
                             (map tree-analysis-init analyses))
                       tree)))

    (for-each (lambda (analysis result)
                ((tree-analysis-post analysis) result env))
              analyses
              (cdr results)))

  tree)


;;;
;;; Unused variable analysis.
;;;

;; <binding-info> records are used during tree traversals in
;; `unused-variable-analysis'.  They contain a list of the local vars
;; currently in scope, and a list of locals vars that have been referenced.
(define-record-type <binding-info>
  (make-binding-info vars refs)
  binding-info?
  (vars binding-info-vars)  ;; ((GENSYM NAME LOCATION) ...)
  (refs binding-info-refs)) ;; (GENSYM ...)

(define (gensym? sym)
  ;; Return #t if SYM is (likely) a generated symbol.
  (string-any #\space (symbol->string sym)))

(define unused-variable-analysis
  ;; Report unused variables in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; Going down into X: extend INFO's variable list
     ;; accordingly.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info))
           (src  (tree-il-src x)))
       (define (extend inner-vars inner-names)
         (fold (lambda (var name vars)
                 (vhash-consq var (list name src) vars))
               vars
               inner-vars
               inner-names))

       (record-case x
         ((<lexical-ref> gensym)
          (make-binding-info vars (vhash-consq gensym #t refs)))
         ((<lexical-set> gensym)
          (make-binding-info vars (vhash-consq gensym #t refs)))
         ((<lambda-case> req opt inits rest kw gensyms)
          (let ((names `(,@req
                         ,@(or opt '())
                         ,@(if rest (list rest) '())
                         ,@(if kw (map cadr (cdr kw)) '()))))
            (make-binding-info (extend gensyms names) refs)))
         ((<let> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         ((<letrec> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         ((<fix> gensyms names)
          (make-binding-info (extend gensyms names) refs))
         (else info))))

   (lambda (x info env locs)
     ;; Leaving X's scope: shrink INFO's variable list
     ;; accordingly and reported unused nested variables.
     (let ((refs (binding-info-refs info))
           (vars (binding-info-vars info)))
       (define (shrink inner-vars refs)
         (vlist-for-each
          (lambda (var)
            (let ((gensym (car var)))
              ;; Don't report lambda parameters as unused.
              (if (and (memq gensym inner-vars)
                       (not (vhash-assq gensym refs))
                       (not (lambda-case? x)))
                  (let ((name (cadr var))
                        ;; We can get approximate source location by going up
                        ;; the LOCS location stack.
                        (loc  (or (caddr var)
                                  (find pair? locs))))
                    (if (and (not (gensym? name))
                             (not (eq? name '_)))
                        (warning 'unused-variable loc name))))))
          vars)
         (vlist-drop vars (length inner-vars)))

       ;; For simplicity, we leave REFS untouched, i.e., with
       ;; names of variables that are now going out of scope.
       ;; It doesn't hurt as these are unique names, it just
       ;; makes REFS unnecessarily fat.
       (record-case x
         ((<lambda-case> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<let> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<letrec> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         ((<fix> gensyms)
          (make-binding-info (shrink gensyms refs) refs))
         (else info))))

   (lambda (result env) #t)
   (make-binding-info vlist-null vlist-null)))


;;;
;;; Unused top-level variable analysis.
;;;

;; <reference-graph> record top-level definitions that are made, references to
;; top-level definitions and their context (the top-level definition in which
;; the reference appears), as well as the current context (the top-level
;; definition we're currently in).  The second part (`refs' below) is
;; effectively a graph from which we can determine unused top-level definitions.
(define-record-type <reference-graph>
  (make-reference-graph refs defs toplevel-context)
  reference-graph?
  (defs             reference-graph-defs) ;; ((NAME . LOC) ...)
  (refs             reference-graph-refs) ;; ((REF-CONTEXT REF ...) ...)
  (toplevel-context reference-graph-toplevel-context)) ;; NAME | #f

(define (graph-reachable-nodes root refs reachable)
  ;; Add to REACHABLE the nodes reachable from ROOT in graph REFS.  REFS is a
  ;; vhash mapping nodes to the list of their children: for instance,
  ;; ((A -> (B C)) (B -> (A)) (C -> ())) corresponds to
  ;;
  ;;  ,-------.
  ;;  v       |
  ;;  A ----> B
  ;;  |
  ;;  v
  ;;  C
  ;;
  ;; REACHABLE is a vhash of nodes known to be otherwise reachable.

  (let loop ((root   root)
             (path   vlist-null)
             (result reachable))
    (if (or (vhash-assq root path)
            (vhash-assq root result))
        result
        (let* ((children (or (and=> (vhash-assq root refs) cdr) '()))
               (path     (vhash-consq root #t path))
               (result   (fold (lambda (kid result)
                                 (loop kid path result))
                               result
                               children)))
          (fold (lambda (kid result)
                  (vhash-consq kid #t result))
                result
                children)))))

(define (graph-reachable-nodes* roots refs)
  ;; Return the list of nodes in REFS reachable from the nodes listed in ROOTS.
  (vlist-fold (lambda (root+true result)
                (let* ((root      (car root+true))
                       (reachable (graph-reachable-nodes root refs result)))
                  (vhash-consq root #t reachable)))
              vlist-null
              roots))

(define (partition* pred vhash)
  ;; Partition VHASH according to PRED.  Return the two resulting vhashes.
  (let ((result
         (vlist-fold (lambda (k+v result)
                       (let ((k  (car k+v))
                             (v  (cdr k+v))
                             (r1 (car result))
                             (r2 (cdr result)))
                         (if (pred k)
                             (cons (vhash-consq k v r1) r2)
                             (cons r1 (vhash-consq k v r2)))))
                     (cons vlist-null vlist-null)
                     vhash)))
    (values (car result) (cdr result))))

(define unused-toplevel-analysis
  ;; Report unused top-level definitions that are not exported.
  (let ((add-ref-from-context
         (lambda (graph name)
           ;; Add an edge CTX -> NAME in GRAPH.
           (let* ((refs     (reference-graph-refs graph))
                  (defs     (reference-graph-defs graph))
                  (ctx      (reference-graph-toplevel-context graph))
                  (ctx-refs (or (and=> (vhash-assq ctx refs) cdr) '())))
             (make-reference-graph (vhash-consq ctx (cons name ctx-refs) refs)
                                   defs ctx)))))
    (define (macro-variable? name env)
      (and (module? env)
           (let ((var (module-variable env name)))
             (and var (variable-bound? var)
                  (macro? (variable-ref var))))))

    (make-tree-analysis
     (lambda (x graph env locs)
       ;; Going down into X.
       (let ((ctx  (reference-graph-toplevel-context graph))
             (refs (reference-graph-refs graph))
             (defs (reference-graph-defs graph)))
         (record-case x
           ((<toplevel-ref> name src)
            (add-ref-from-context graph name))
           ((<toplevel-define> name src)
            (let ((refs refs)
                  (defs (vhash-consq name (or src (find pair? locs))
                                     defs)))
              (make-reference-graph refs defs name)))
           ((<toplevel-set> name src)
            (add-ref-from-context graph name))
           (else graph))))

     (lambda (x graph env locs)
       ;; Leaving X's scope.
       (record-case x
         ((<toplevel-define>)
          (let ((refs (reference-graph-refs graph))
                (defs (reference-graph-defs graph)))
            (make-reference-graph refs defs #f)))
         (else graph)))

     (lambda (graph env)
       ;; Process the resulting reference graph: determine all private definitions
       ;; not reachable from any public definition.  Macros
       ;; (syntax-transformers), which are globally bound, never considered
       ;; unused since we can't tell whether a macro is actually used; in
       ;; addition, macros are considered roots of the graph since they may use
       ;; private bindings.  FIXME: The `make-syntax-transformer' calls don't
       ;; contain any literal `toplevel-ref' of the global bindings they use so
       ;; this strategy fails.
       (define (exported? name)
         (if (module? env)
             (module-variable (module-public-interface env) name)
             #t))

       (let-values (((public-defs private-defs)
                     (partition* (lambda (name)
                                   (or (exported? name)
                                       (macro-variable? name env)))
                                 (reference-graph-defs graph))))
         (let* ((roots     (vhash-consq #f #t public-defs))
                (refs      (reference-graph-refs graph))
                (reachable (graph-reachable-nodes* roots refs))
                (unused    (vlist-filter (lambda (name+src)
                                           (not (vhash-assq (car name+src)
                                                            reachable)))
                                         private-defs)))
           (vlist-for-each (lambda (name+loc)
                             (let ((name (car name+loc))
                                   (loc  (cdr name+loc)))
                               (if (not (gensym? name))
                                   (warning 'unused-toplevel loc name))))
                           unused))))

     (make-reference-graph vlist-null vlist-null #f))))


;;;
;;; Shadowed top-level definition analysis.
;;;

(define shadowed-toplevel-analysis
  ;; Report top-level definitions that shadow previous top-level
  ;; definitions from the same compilation unit.
  (make-tree-analysis
   (lambda (x defs env locs)
     ;; Going down into X.
     (record-case x
                  ((<toplevel-define> name src)
                   (match (vhash-assq name defs)
                     ((_ . previous-definition)
                      (warning 'shadowed-toplevel src name
                               (toplevel-define-src previous-definition))
                      defs)
                     (#f
                      (vhash-consq name x defs))))
                  (else defs)))

   (lambda (x defs env locs)
     ;; Leaving X's scope.
     defs)

   (lambda (defs env)
     #t)

   vlist-null))


;;;
;;; Unbound variable analysis.
;;;

;; <toplevel-info> records are used during tree traversal in search of
;; possibly unbound variable.  They contain a list of references to
;; potentially unbound top-level variables, and a list of the top-level
;; defines that have been encountered.
(define-record-type <toplevel-info>
  (make-toplevel-info refs defs)
  toplevel-info?
  (refs  toplevel-info-refs)  ;; ((VARIABLE-NAME . LOCATION) ...)
  (defs  toplevel-info-defs)) ;; (VARIABLE-NAME ...)

(define (goops-toplevel-definition proc args env)
  ;; If call of PROC to ARGS is a GOOPS top-level definition, return
  ;; the name of the variable being defined; otherwise return #f.  This
  ;; assumes knowledge of the current implementation of `define-class' et al.
  (define (toplevel-define-arg args)
    (match args
      ((($ <const> _ (and (? symbol?) exp)) _)
       exp)
      (_ #f)))

  (match proc
    (($ <module-ref> _ '(oop goops) 'toplevel-define! #f)
     (toplevel-define-arg args))
    (($ <toplevel-ref> _ _ 'toplevel-define!)
     ;; This may be the result of expanding one of the GOOPS macros within
     ;; `oop/goops.scm'.
     (and (eq? env (resolve-module '(oop goops)))
          (toplevel-define-arg args)))
    (_ #f)))

(define unbound-variable-analysis
  ;; Report possibly unbound variables in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; Going down into X.
     (let* ((refs (toplevel-info-refs info))
            (defs (toplevel-info-defs info))
            (src  (tree-il-src x)))
       (define (bound? name)
         (or (and (module? env)
                  (module-variable env name))
             (vhash-assq name defs)))

       (record-case x
         ((<toplevel-ref> name src)
          (if (bound? name)
              info
              (let ((src (or src (find pair? locs))))
                (make-toplevel-info (vhash-consq name src refs)
                                    defs))))
         ((<toplevel-set> name src)
          (if (bound? name)
              (make-toplevel-info refs defs)
              (let ((src (find pair? locs)))
                (make-toplevel-info (vhash-consq name src refs)
                                    defs))))
         ((<toplevel-define> name)
          (make-toplevel-info (vhash-delq name refs)
                              (vhash-consq name #t defs)))

         ((<call> proc args)
          ;; Check for a dynamic top-level definition, as is
          ;; done by code expanded from GOOPS macros.
          (let ((name (goops-toplevel-definition proc args
                                                 env)))
            (if (symbol? name)
                (make-toplevel-info (vhash-delq name refs)
                                    (vhash-consq name #t defs))
                (make-toplevel-info refs defs))))
         (else
          (make-toplevel-info refs defs)))))

   (lambda (x info env locs)
     ;; Leaving X's scope.
     info)

   (lambda (toplevel env)
     ;; Post-process the result.
     (vlist-for-each (match-lambda
                       ((name . loc)
                        (warning 'unbound-variable loc name)))
                     (vlist-reverse (toplevel-info-refs toplevel))))

   (make-toplevel-info vlist-null vlist-null)))


;;;
;;; Macro use-before-definition analysis.
;;;

;; <macro-use-info> records are used during tree traversal in search of
;; possibly uses of macros before they are defined.  They contain a list
;; of references to top-level variables, and a list of the top-level
;; macro definitions that have been encountered.  Any definition which
;; is a macro should in theory be expanded out already; if that's not
;; the case, the program likely has a bug.
(define-record-type <macro-use-info>
  (make-macro-use-info uses defs)
  macro-use-info?
  (uses  macro-use-info-uses)  ;; ((VARIABLE-NAME . LOCATION) ...)
  (defs  macro-use-info-defs))  ;; ((VARIABLE-NAME . LOCATION) ...)

(define macro-use-before-definition-analysis
  ;; Report possibly unbound variables in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; Going down into X.
     (define (nearest-loc src)
       (or src (find pair? locs)))
     (define (add-use name src)
       (match info
         (($ <macro-use-info> uses defs)
          (make-macro-use-info (vhash-consq name src uses) defs))))
     (define (add-def name src)
       (match info
         (($ <macro-use-info> uses defs)
          (make-macro-use-info uses (vhash-consq name src defs)))))
     (define (macro? x)
       (match x
         (($ <primcall> _ 'make-syntax-transformer) #t)
         (_ #f)))
     (match x
       (($ <toplevel-ref> src mod name)
        (add-use name (nearest-loc src)))
       (($ <toplevel-set> src mod name)
        (add-use name (nearest-loc src)))
       (($ <toplevel-define> src mod name (? macro?))
        (add-def name (nearest-loc src)))
       (_ info)))

   (lambda (x info env locs)
     ;; Leaving X's scope.
     info)

   (lambda (info env)
     ;; Post-process the result.
     (match info
       (($ <macro-use-info> uses defs)
        (vlist-for-each
         (match-lambda
           ((name . use-loc)
            (when (vhash-assq name defs)
              (warning 'macro-use-before-definition use-loc name))))
         (vlist-reverse (macro-use-info-uses info))))))

   (make-macro-use-info vlist-null vlist-null)))


;;;
;;; Arity analysis.
;;;

;; <arity-info> records contain information about lexical definitions of
;; procedures currently in scope, top-level procedure definitions that have
;; been encountered, and calls to top-level procedures that have been
;; encountered.
(define-record-type <arity-info>
  (make-arity-info toplevel-calls lexical-lambdas toplevel-lambdas)
  arity-info?
  (toplevel-calls   toplevel-procedure-calls) ;; ((NAME . CALL) ...)
  (lexical-lambdas  lexical-lambdas)          ;; ((GENSYM . DEFINITION) ...)
  (toplevel-lambdas toplevel-lambdas))        ;; ((NAME . DEFINITION) ...)

(define (validate-arity proc call lexical?)
  ;; Validate the argument count of CALL, a tree-il call of
  ;; PROC, emitting a warning in case of argument count mismatch.

  (define (filter-keyword-args keywords allow-other-keys? args)
    ;; Filter keyword arguments from ARGS and return the resulting list.
    ;; KEYWORDS is the list of allowed keywords, and ALLOW-OTHER-KEYS?
    ;; specified whethere keywords not listed in KEYWORDS are allowed.
    (let loop ((args   args)
               (result '()))
      (if (null? args)
          (reverse result)
          (let ((arg (car args)))
            (if (and (const? arg)
                     (or (memq (const-exp arg) keywords)
                         (and allow-other-keys?
                              (keyword? (const-exp arg)))))
                (loop (if (pair? (cdr args))
                          (cddr args)
                          '())
                      result)
                (loop (cdr args)
                      (cons arg result)))))))

  (define (arities proc)
    ;; Return the arities of PROC, which can be either a tree-il or a
    ;; procedure.
    (define (len x)
      (or (and (or (null? x) (pair? x))
               (length x))
          0))
    (cond ((program? proc)
           (values (procedure-name proc)
                   (map (lambda (a)
                          (list (length (or (assq-ref a 'required) '()))
                                (length (or (assq-ref a 'optional) '()))
                                (and (assq-ref a 'rest) #t)
                                (map car (or (assq-ref a 'keyword) '()))
                                (assq-ref a 'allow-other-keys?)))
                        (program-arguments-alists proc))))
          ((procedure? proc)
           (if (struct? proc)
               ;; An applicable struct.
               (arities (struct-ref proc 0))
               ;; An applicable smob.
               (let ((arity (procedure-minimum-arity proc)))
                 (values (procedure-name proc)
                         (list (list (car arity) (cadr arity) (caddr arity)
                                     #f #f))))))
          (else
           (let loop ((name    #f)
                      (proc    proc)
                      (arities '()))
             (if (not proc)
                 (values name (reverse arities))
                 (record-case proc
                   ((<lambda-case> req opt rest kw alternate)
                    (loop name alternate
                          (cons (list (len req) (len opt) rest
                                      (and (pair? kw) (map car (cdr kw)))
                                      (and (pair? kw) (car kw)))
                                arities)))
                   ((<lambda> meta body)
                    (loop (assoc-ref meta 'name) body arities))
                   (else
                    (values #f #f))))))))

  (let ((args (call-args call))
        (src  (tree-il-src call)))
    (call-with-values (lambda () (arities proc))
      (lambda (name arities)
        (define matches?
          (find (lambda (arity)
                  (pmatch arity
                    ((,req ,opt ,rest? ,kw ,aok?)
                     (let ((args (if (pair? kw)
                                     (filter-keyword-args kw aok? args)
                                     args)))
                       (if (and req opt)
                           (let ((count (length args)))
                             (and (>= count req)
                                  (or rest?
                                      (<= count (+ req opt)))))
                           #t)))
                    (else #t)))
                arities))

        (if (not matches?)
            (warning 'arity-mismatch src
                     (or name (with-output-to-string (lambda () (write proc))))
                     lexical?)))))
  #t)

(define arity-analysis
  ;; Report arity mismatches in the given tree.
  (make-tree-analysis
   (lambda (x info env locs)
     ;; Down into X.
     (define (extend lexical-name val info)
       ;; If VAL is a lambda, add NAME to the lexical-lambdas of INFO.
       (let ((toplevel-calls   (toplevel-procedure-calls info))
             (lexical-lambdas  (lexical-lambdas info))
             (toplevel-lambdas (toplevel-lambdas info)))
         (record-case val
           ((<lambda> body)
            (make-arity-info toplevel-calls
                             (vhash-consq lexical-name val
                                          lexical-lambdas)
                             toplevel-lambdas))
           ((<lexical-ref> gensym)
            ;; lexical alias
            (let ((val* (vhash-assq gensym lexical-lambdas)))
              (if (pair? val*)
                  (extend lexical-name (cdr val*) info)
                  info)))
           ((<toplevel-ref> name)
            ;; top-level alias
            (make-arity-info toplevel-calls
                             (vhash-consq lexical-name val
                                          lexical-lambdas)
                             toplevel-lambdas))
           (else info))))

     (let ((toplevel-calls   (toplevel-procedure-calls info))
           (lexical-lambdas  (lexical-lambdas info))
           (toplevel-lambdas (toplevel-lambdas info)))

       (record-case x
         ((<toplevel-define> name exp)
          (record-case exp
            ((<lambda> body)
             (make-arity-info toplevel-calls
                              lexical-lambdas
                              (vhash-consq name exp toplevel-lambdas)))
            ((<toplevel-ref> name)
             ;; alias for another toplevel
             (let ((proc (vhash-assq name toplevel-lambdas)))
               (make-arity-info toplevel-calls
                                lexical-lambdas
                                (vhash-consq (toplevel-define-name x)
                                             (if (pair? proc)
                                                 (cdr proc)
                                                 exp)
                                             toplevel-lambdas))))
            (else info)))
         ((<let> gensyms vals)
          (fold extend info gensyms vals))
         ((<letrec> gensyms vals)
          (fold extend info gensyms vals))
         ((<fix> gensyms vals)
          (fold extend info gensyms vals))

         ((<call> proc args src)
          (record-case proc
            ((<lambda> body)
             (validate-arity proc x #t)
             info)
            ((<toplevel-ref> name)
             (make-arity-info (vhash-consq name x toplevel-calls)
                              lexical-lambdas
                              toplevel-lambdas))
            ((<lexical-ref> gensym)
             (let ((proc (vhash-assq gensym lexical-lambdas)))
               (if (pair? proc)
                   (record-case (cdr proc)
                     ((<toplevel-ref> name)
                      ;; alias to toplevel
                      (make-arity-info (vhash-consq name x toplevel-calls)
                                       lexical-lambdas
                                       toplevel-lambdas))
                     (else
                      (validate-arity (cdr proc) x #t)
                      info))

                   ;; If GENSYM wasn't found, it may be because it's an
                   ;; argument of the procedure being compiled.
                   info)))
            (else info)))
         (else info))))

   (lambda (x info env locs)
     ;; Up from X.
     (define (shrink name val info)
       ;; Remove NAME from the lexical-lambdas of INFO.
       (let ((toplevel-calls   (toplevel-procedure-calls info))
             (lexical-lambdas  (lexical-lambdas info))
             (toplevel-lambdas (toplevel-lambdas info)))
         (make-arity-info toplevel-calls
                          (if (vhash-assq name lexical-lambdas)
                              (vlist-tail lexical-lambdas)
                              lexical-lambdas)
                          toplevel-lambdas)))

     (let ((toplevel-calls   (toplevel-procedure-calls info))
           (lexical-lambdas  (lexical-lambdas info))
           (toplevel-lambdas (toplevel-lambdas info)))
       (record-case x
         ((<let> gensyms vals)
          (fold shrink info gensyms vals))
         ((<letrec> gensyms vals)
          (fold shrink info gensyms vals))
         ((<fix> gensyms vals)
          (fold shrink info gensyms vals))

         (else info))))

   (lambda (result env)
     ;; Post-processing: check all top-level procedure calls that have been
     ;; encountered.
     (let ((toplevel-calls   (toplevel-procedure-calls result))
           (toplevel-lambdas (toplevel-lambdas result)))
       (vlist-for-each
        (lambda (name+call)
          (let* ((name (car name+call))
                 (call (cdr name+call))
                 (proc
                  (or (and=> (vhash-assq name toplevel-lambdas) cdr)
                      (and (module? env)
                           (false-if-exception
                            (module-ref env name)))))
                 (proc*
                  ;; handle toplevel aliases
                  (if (toplevel-ref? proc)
                      (let ((name (toplevel-ref-name proc)))
                        (and (module? env)
                             (false-if-exception
                              (module-ref env name))))
                      proc)))
            (cond ((lambda? proc*)
                   (validate-arity proc* call #t))
                  ((procedure? proc*)
                   (validate-arity proc* call #f)))))
        toplevel-calls)))

   (make-arity-info vlist-null vlist-null vlist-null)))


;;;
;;; `format' argument analysis.
;;;

(define &syntax-error
  ;; The `throw' key for syntax errors.
  (gensym "format-string-syntax-error"))

(define (format-string-argument-count fmt)
  ;; Return the minimum and maxium number of arguments that should
  ;; follow format string FMT (or, ahem, a good estimate thereof) or
  ;; `any' if the format string can be followed by any number of
  ;; arguments.

  (define (drop-group chars end)
    ;; Drop characters from CHARS until "~END" is encountered.
    (let loop ((chars  chars)
               (tilde? #f))
      (if (null? chars)
          (throw &syntax-error 'unterminated-iteration)
          (if tilde?
              (if (eq? (car chars) end)
                  (cdr chars)
                  (loop (cdr chars) #f))
              (if (eq? (car chars) #\~)
                  (loop (cdr chars) #t)
                  (loop (cdr chars) #f))))))

  (define (digit? char)
    ;; Return true if CHAR is a digit, #f otherwise.
    (memq char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

  (define (previous-number chars)
    ;; Return the previous series of digits found in CHARS.
    (let ((numbers (take-while digit? chars)))
      (and (not (null? numbers))
           (string->number (list->string (reverse numbers))))))

  (let loop ((chars       (string->list fmt))
             (state       'literal)
             (params      '())
             (conditions  '())
             (end-group   #f)
             (min-count 0)
             (max-count 0))
    (if (null? chars)
        (if end-group
            (throw &syntax-error 'unterminated-conditional)
            (values min-count max-count))
        (case state
          ((tilde)
           (case (car chars)
             ((#\~ #\% #\& #\t #\T #\_ #\newline #\( #\) #\! #\| #\/ #\q #\Q)
                        (loop (cdr chars) 'literal '()
                              conditions end-group
                              min-count max-count))
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\, #\: #\@ #\+ #\- #\#)
                        (loop (cdr chars)
                              'tilde (cons (car chars) params)
                              conditions end-group
                              min-count max-count))
             ((#\v #\V) (loop (cdr chars)
                              'tilde (cons (car chars) params)
                              conditions end-group
                              (+ 1 min-count)
                              (+ 1 max-count)))
             ((#\p #\P) (let* ((colon?    (memq #\: params))
                               (min-count (if colon?
                                              (max 1 min-count)
                                              (+ 1 min-count))))
                          (loop (cdr chars) 'literal '()
                                conditions end-group
                                min-count
                                (if colon?
                                    (max max-count min-count)
                                    (+ 1 max-count)))))
             ((#\[)
              (loop chars 'literal '() '()
                    (let ((selector (previous-number params))
                          (at?      (memq #\@ params)))
                      (lambda (chars conds)
                        ;; end of group
                        (let ((mins (map car conds))
                              (maxs (map cdr conds))
                              (sel? (and selector
                                         (< selector (length conds)))))
                          (if (and (every number? mins)
                                   (every number? maxs))
                              (loop chars 'literal '() conditions end-group
                                    (+ min-count
                                       (if sel?
                                           (car (list-ref conds selector))
                                           (+ (if at? 0 1)
                                              (if (null? mins)
                                                  0
                                                  (apply min mins)))))
                                    (+ max-count
                                       (if sel?
                                           (cdr (list-ref conds selector))
                                           (+ (if at? 0 1)
                                              (if (null? maxs)
                                                  0
                                                  (apply max maxs))))))
                              (values 'any 'any))))) ;; XXX: approximation
                    0 0))
             ((#\;)
              (if end-group
                  (loop (cdr chars) 'literal '()
                        (cons (cons min-count max-count) conditions)
                        end-group
                        0 0)
                  (throw &syntax-error 'unexpected-semicolon)))
             ((#\])
              (if end-group
                  (end-group (cdr chars)
                             (reverse (cons (cons min-count max-count)
                                            conditions)))
                  (throw &syntax-error 'unexpected-conditional-termination)))
             ((#\{)     (if (memq #\@ params)
                            (values min-count 'any)
                            (loop (drop-group (cdr chars) #\})
                                  'literal '()
                                  conditions end-group
                                  (+ 1 min-count) (+ 1 max-count))))
             ((#\*)     (if (memq #\@ params)
                            (values 'any 'any) ;; it's unclear what to do here
                            (loop (cdr chars)
                                  'literal '()
                                  conditions end-group
                                  (+ (or (previous-number params) 1)
                                     min-count)
                                  (+ (or (previous-number params) 1)
                                     max-count))))
             ((#\? #\k #\K)
              ;; We don't have enough info to determine the exact number
              ;; of args, but we could determine a lower bound (TODO).
              (values 'any 'any))
             ((#\^)
              (values min-count 'any))
             ((#\h #\H)
                        (let ((argc (if (memq #\: params) 2 1)))
                          (loop (cdr chars) 'literal '()
                                conditions end-group
                                (+ argc min-count)
                                (+ argc max-count))))
             ((#\')
              (if (null? (cdr chars))
                  (throw &syntax-error 'unexpected-termination)
                  (loop (cddr chars) 'tilde (cons (cadr chars) params)
                        conditions end-group min-count max-count)))
             (else      (loop (cdr chars) 'literal '()
                              conditions end-group
                              (+ 1 min-count) (+ 1 max-count)))))
          ((literal)
           (case (car chars)
             ((#\~)     (loop (cdr chars) 'tilde '()
                              conditions end-group
                              min-count max-count))
             (else      (loop (cdr chars) 'literal '()
                              conditions end-group
                              min-count max-count))))
          (else (error "computer bought the farm" state))))))

(define (proc-ref? exp proc special-name env)
  "Return #t when EXP designates procedure PROC in ENV.  As a last
resort, return #t when EXP refers to the global variable SPECIAL-NAME."

  (define special?
    (cut eq? <> special-name))

  (match exp
    (($ <toplevel-ref> _ _ (? special?))
     ;; Allow top-levels like: (define G_ (cut gettext <> "my-domain")).
     #t)
    (($ <toplevel-ref> _ _ name)
     (let ((var (module-variable env name)))
       (and var (variable-bound? var)
            (eq? (variable-ref var) proc))))
    (($ <module-ref> _ _ (? special?))
     #t)
    (($ <module-ref> _ module name public?)
     (let* ((mod (if public?
                     (false-if-exception (resolve-interface module))
                     (resolve-module module #:ensure #f)))
            (var (and mod (module-variable mod name))))
       (and var (variable-bound? var) (eq? (variable-ref var) proc))))
    (($ <lexical-ref> _ (? special?))
     #t)
    (_ #f)))

(define gettext? (cut proc-ref? <> gettext 'G_ <>))
(define ngettext? (cut proc-ref? <> ngettext 'N_ <>))

(define (const-fmt x env)
  ;; Return the literal format string for X, or #f.
  (match x
    (($ <const> _ (? string? exp))
     exp)
    (($ <call> _ (? (cut gettext? <> env))
        (($ <const> _ (? string? fmt))))
     ;; Gettexted literals, like `(G_ "foo")'.
     fmt)
    (($ <call> _ (? (cut ngettext? <> env))
        (($ <const> _ (? string? fmt)) ($ <const> _ (? string?)) _ ..1))
     ;; Plural gettextized literals, like `(N_ "singular" "plural" n)'.

     ;; TODO: Check whether the singular and plural strings have the
     ;; same format escapes.
     fmt)
    (_ #f)))

(define format-analysis
  ;; Report arity mismatches in the given tree.
  (make-tree-analysis
   (lambda (x res env locs)
     ;; Down into X.
     (define (check-format-args args loc)
       (pmatch args
         ((,port ,fmt . ,rest)
          (guard (const-fmt fmt env))
          (if (and (const? port)
                   (not (boolean? (const-exp port))))
              (warning 'format loc 'wrong-port (const-exp port)))
          (let ((fmt   (const-fmt fmt env))
                (count (length rest)))
            (catch &syntax-error
              (lambda ()
                (let-values (((min max)
                              (format-string-argument-count fmt)))
                  (and min max
                       (or (and (or (eq? min 'any) (>= count min))
                                (or (eq? max 'any) (<= count max)))
                           (warning 'format loc 'wrong-format-arg-count
                                    fmt min max count)))))
              (lambda (_ key)
                (warning 'format loc 'syntax-error key fmt)))))
         ((,port ,fmt . ,rest)
          (if (and (const? port)
                   (not (boolean? (const-exp port))))
              (warning 'format loc 'wrong-port (const-exp port)))

          (match fmt
            (($ <const> loc* (? (negate string?) fmt))
             (warning 'format (or loc* loc) 'wrong-format-string fmt))

            ;; Warn on non-literal format strings, unless they refer to
            ;; a lexical variable named "fmt".
            (($ <lexical-ref> _ fmt)
             #t)
            ((? (negate const?))
             (warning 'format loc 'non-literal-format-string))))
         (else
          (warning 'format loc 'wrong-num-args (length args)))))

     (define (check-simple-format-args args loc)
       ;; Check the arguments to the `simple-format' procedure, which is
       ;; less capable than that of (ice-9 format).

       (define allowed-chars
         '(#\A #\S #\a #\s #\~ #\%))

       (define (format-chars fmt)
         (let loop ((chars  (string->list fmt))
                    (result '()))
           (match chars
             (()
              (reverse result))
             ((#\~ opt rest ...)
              (loop rest (cons opt result)))
             ((_ rest ...)
              (loop rest result)))))

       (match args
         ((port ($ <const> _ (? string? fmt)) _ ...)
          (let ((opts (format-chars fmt)))
            (or (every (cut memq <> allowed-chars) opts)
                (begin
                  (warning 'format loc 'simple-format fmt
                           (find (negate (cut memq <> allowed-chars)) opts))
                  #f))))
         ((port (= (cut const-fmt <> env) (? string? fmt)) args ...)
          (check-simple-format-args `(,port ,(make-const loc fmt) ,args) loc))
         (_ #t)))

     (define (resolve-toplevel name)
       (and (module? env)
            (false-if-exception (module-ref env name))))

     (match x
       (($ <call> src ($ <toplevel-ref> _ _ name) args)
        (let ((proc (resolve-toplevel name)))
          (if (or (and (eq? proc (@ (guile) simple-format))
                       (check-simple-format-args args
                                                 (or src (find pair? locs))))
                  (eq? proc (@ (ice-9 format) format)))
              (check-format-args args (or src (find pair? locs))))))
       (($ <call> src ($ <module-ref> _ '(ice-9 format) 'format) args)
        (check-format-args args (or src (find pair? locs))))
       (($ <call> src ($ <module-ref> _ '(guile)
                         (or 'format 'simple-format))
           args)
        (and (check-simple-format-args args
                                       (or src (find pair? locs)))
             (check-format-args args (or src (find pair? locs)))))
       (_ #t))
     #t)

   (lambda (x _ env locs)
     ;; Up from X.
     #t)

   (lambda (_ env)
     ;; Post-processing.
     #t)

   #t))

(define %warning-passes
  `(#(unused-variable             3 ,unused-variable-analysis)
    #(unused-toplevel             2 ,unused-toplevel-analysis)
    #(shadowed-toplevel           2 ,shadowed-toplevel-analysis)
    #(unbound-variable            1 ,unbound-variable-analysis)
    #(macro-use-before-definition 1 ,macro-use-before-definition-analysis)
    #(arity-mismatch              1 ,arity-analysis)
    #(format                      1 ,format-analysis)))

(define (make-analyzer warning-level warnings)
  (define (enabled-for-level? level) (<= level warning-level))
  (let ((analyses (filter-map (match-lambda
                               (#(kind level analysis)
                                (and (or (enabled-for-level? level)
                                         (memq kind warnings))
                                     analysis)))
                              %warning-passes)))
    (lambda (exp env)
      (analyze-tree analyses exp env))))
