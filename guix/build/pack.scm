;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build pack)
  #:use-module (guix build utils)
  #:export (tar-base-options))

(define* (tar-base-options #:key tar compressor)
  "Return the base GNU tar options required to produce deterministic archives
deterministically.  When TAR, a GNU tar command file name, is provided, the
`--sort' option is used only if supported.  When COMPRESSOR, a command such as
'(\"gzip\" \"-9n\"), is provided, the compressor is explicitly specified via
the `-I' option."
  (define (tar-supports-sort? tar)
    (zero? (system* tar "cf" "/dev/null" "--files-from=/dev/null"
                    "--sort=name")))

  `(,@(if compressor
          (list "-I" (string-join compressor))
          '())
    ;; The --sort option was added to GNU tar in version 1.28, released
    ;; 2014-07-28.  For testing, we use the bootstrap tar, which is older
    ;; and doesn't support it.
    ,@(if (and=> tar tar-supports-sort?)
          '("--sort=name")
          '())
    ;; Use GNU format so there's no file name length limitation.
    "--format=gnu"
    "--mtime=@1"
    "--owner=root:0"
    "--group=root:0"
    ;; The 'nlink' of the store item files leads tar to store hard links
    ;; instead of actual copies.  However, the 'nlink' count depends on
    ;; deduplication in the store; it's an "implicit input" to the build
    ;; process.  Use '--hard-dereference' to eliminate it.
    "--hard-dereference"
    "--check-links"))
