;;; inexact.scm --- The R7RS inexact library

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


(define-library (scheme inexact)
  (export acos asin atan cos sin tan
          log exp sqrt finite? infinite? nan?)
  (import (rename (rnrs base)
                  (finite? r6rs-finite?)
                  (infinite? r6rs-infinite?)
                  (nan? r6rs-nan?)))
  (begin
    (define (finite? z)
      (and (r6rs-finite? (real-part z))
           (r6rs-finite? (imag-part z))))
    (define (infinite? z)
      (or (r6rs-infinite? (real-part z))
          (r6rs-infinite? (imag-part z))))
    (define (nan? z)
      (or (r6rs-nan? (real-part z))
          (r6rs-nan? (imag-part z))))))
