;;; GDB debugging support for Guile.
;;;
;;; Copyright 2014, 2015 Free Software Foundation, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guile-gdb)
  #:use-module (system base types)
  #:use-module ((gdb) #:hide (symbol?))
  #:use-module (gdb printing)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:export (%gdb-memory-backend
            display-vm-frames))

;;; Commentary:
;;;
;;; This file defines GDB extensions to pretty-print 'SCM' objects, and
;;; to walk Guile's virtual machine stack.
;;;
;;; This file is installed under a name that follows the convention that
;;; allows GDB to auto-load it anytime the user is debugging libguile
;;; (info "(gdb) objfile-gdbdotext file").
;;;
;;; Code:

(define (type-name-from-descriptor descriptor-array type-number)
  "Return the name of the type TYPE-NUMBER as seen in DESCRIPTOR-ARRAY, or #f
if the information is not available."
  (let ((descriptors (lookup-global-symbol descriptor-array)))
    (and descriptors
         (let ((code (type-code (symbol-type descriptors))))
           (or (= TYPE_CODE_ARRAY code)
               (= TYPE_CODE_PTR code)))
         (let* ((type-descr (value-subscript (symbol-value descriptors)
                                             type-number))
                (name       (value-field type-descr "name")))
           (value->string name)))))

(define (scm-value->integer value)
  "Return the integer value of VALUE, which is assumed to be a GDB value
corresponding to an 'SCM' object."
  (let ((type (type-strip-typedefs (value-type value))))
    (cond ((= (type-code type) TYPE_CODE_UNION)
           ;; SCM_DEBUG_TYPING_STRICTNESS = 2
           (value->integer (value-field (value-field value "n")
                                        "n")))
          (else
           ;; SCM_DEBUG_TYPING_STRICTNESS = 1
           (value->integer value)))))

(define %gdb-memory-backend
  ;; The GDB back-end to access the inferior's memory.
  (let ((void* (type-pointer (lookup-type "void"))))
    (define (dereference-word address)
      ;; Return the word at ADDRESS.
      (value->integer
       (value-dereference (value-cast (make-value address)
                                      (type-pointer void*)))))

    (define (open address size)
      ;; Return a port to the SIZE bytes starting at ADDRESS.
      (if size
          (open-memory #:start address #:size size)
          (open-memory #:start address)))

    (define (type-name kind number)
      ;; Return the type name of KIND type NUMBER.
      (type-name-from-descriptor (case kind
                                   ((smob) "scm_smobs")
                                   ((port) "scm_ptobs"))
                                 number))

    (memory-backend dereference-word open type-name)))


;;;
;;; GDB pretty-printer registration.
;;;

(define (make-scm-pretty-printer-worker obj)
  (define (list->iterator list)
    (make-iterator list list
                   (let ((n 0))
                     (lambda (iter)
                       (match (iterator-progress iter)
                         (() (end-of-iteration))
                         ((elt . list)
                          (set-iterator-progress! iter list)
                          (let ((name (format #f "[~a]" n)))
                            (set! n (1+ n))
                            (cons name (object->string elt)))))))))
  (cond
   ((string? obj)
    (make-pretty-printer-worker
     "string" ; display hint
     (lambda (printer) obj)
     #f))
   ((and (array? obj)
         (match (array-shape obj)
           (((0 _)) #t)
           (_ #f)))
    (make-pretty-printer-worker
     "array" ; display hint
     (lambda (printer)
       (let ((tag (array-type obj)))
         (case tag
           ((#t) "#<vector>")
           ((b) "#<bitvector>")
           (else (format #f "#<~avector>" tag)))))
     (lambda (printer)
       (list->iterator (array->list obj)))))
   ((inferior-struct? obj)
    (make-pretty-printer-worker
     "array" ; display hint
     (lambda (printer)
       (format #f "#<struct ~a>" (inferior-struct-name obj)))
     (lambda (printer)
       (list->iterator (inferior-struct-fields obj)))))
   (else
    (make-pretty-printer-worker
     #f                                 ; display hint
     (lambda (printer)
       (object->string obj))
     #f))))

(define %scm-pretty-printer
  (make-pretty-printer
   "SCM"
   (lambda (pp value)
     (let ((name (type-name (value-type value))))
       (and (and name (string=? name "SCM"))
            (make-scm-pretty-printer-worker
             (scm->object (scm-value->integer value) %gdb-memory-backend)))))))

(define* (register-pretty-printer #:optional objfile)
  (prepend-pretty-printer! objfile %scm-pretty-printer))

(register-pretty-printer)


;;;
;;; VM stack walking.
;;;

(define (find-vm-engine-frame)
  "Return the bottom-most frame containing a call to the VM engine."
  (define (vm-engine-frame? frame)
    (let ((sym (frame-function frame)))
      (and sym
           (member (symbol-name sym)
                   '("vm_debug_engine" "vm_regular_engine")))))

  (let loop ((frame (newest-frame)))
    (and frame
         (if (vm-engine-frame? frame)
             frame
             (loop (frame-older frame))))))

(define (vm-stack-pointer)
  "Return the current value of the VM stack pointer or #f."
  (let ((frame (find-vm-engine-frame)))
    (and frame
         (frame-read-var frame "sp"))))

(define (vm-frame-pointer)
  "Return the current value of the VM frame pointer or #f."
  (let ((frame (find-vm-engine-frame)))
    (and frame
         (frame-read-var frame "fp"))))

(define* (display-vm-frames #:optional (port (current-output-port)))
  "Display the VM frames on PORT."
  (define (display-objects start end)
    ;; Display all the objects (arguments and local variables) located
    ;; between START and END.
    (let loop ((number  0)
               (address start))
      (when (and (> start 0) (<= address end))
        (let ((object (dereference-word %gdb-memory-backend address)))
          ;; TODO: Push onto GDB's value history.
          (format port "  slot ~a -> ~s~%"
                  number (scm->object object %gdb-memory-backend)))
        (loop (+ 1 number) (+ address %word-size)))))

  (let loop ((number 0)
             (sp     (value->integer (vm-stack-pointer)))
             (fp     (value->integer (vm-frame-pointer))))
    (unless (zero? fp)
      (let-values (((ra mvra link proc)
                    (vm-frame fp %gdb-memory-backend)))
        (format port "#~a ~s~%" number (scm->object proc %gdb-memory-backend))
        (display-objects fp sp)
        (loop (+ 1 number) (- fp (* 5 %word-size)) link)))))

;; See libguile/frames.h.
(define* (vm-frame fp #:optional (backend %gdb-memory-backend))
  "Return the components of the stack frame at FP."
  (let ((caller (dereference-word backend (- fp %word-size)))
        (ra     (dereference-word backend (- fp (* 2 %word-size))))
        (mvra   (dereference-word backend (- fp (* 3 %word-size))))
        (link   (dereference-word backend (- fp (* 4 %word-size)))))
    (values ra mvra link caller)))

;;; libguile-2.0-gdb.scm ends here
