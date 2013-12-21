;;; eval.scm --- The R7RS eval library

;;      Copyright (C) 2013 Free Software Foundation, Inc.
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


(define-library (scheme eval)
  (export eval environment)
  (import (scheme base)
          (only (guile)
                eval
                make-module
                resolve-r6rs-interface
                module-use-interfaces!))
  (begin
    (define (environment . import-sets)
      (let ((m (make-module)))
        (module-use-interfaces! m (map resolve-r6rs-interface import-sets))
        m))))
