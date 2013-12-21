;;; write.scm --- The R7RS write library

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


(define-library (scheme write)
  (export display
          write
          write-shared
          write-simple)
  (import (scheme base)
          (rename (only (guile)
                        display
                        write)
                  (write guile-write)))
  (begin
    (define write-simple guile-write)

    ;; XXX FIXME outputs cyclic data in non-standard format.
    (define write guile-write)

    ;; XXX FIXME doesn't show non-cyclic sharing, and outputs cyclic
    ;; data in non-standard format.
    (define write-shared guile-write)))
