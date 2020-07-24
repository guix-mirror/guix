;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Robin Templeton <robin@igalia.com>
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

(define-module (gnu packages phabricator)
  #:use-module (gnu packages php)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public libphutil
  (let ((commit "b29d76e1709ef018cc5edc7c03033fd9fdebc578")
        (revision "1"))
    (package
      (name "libphutil")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/phacility/libphutil")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06j84721r9r8624fmil62b5crs2qs0v6rr3cvv2zvkvwhxwrwv1l"))))
      (build-system gnu-build-system)
      ;; TODO: Unbundle jsonlint and porter-stemmer.
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda _
               (let ((lib (string-append %output "/lib/libphutil")))
                 (mkdir-p lib)
                 (copy-recursively "." lib))
               #t)))))
      (inputs
       `(("php" ,php)))
      (home-page "https://github.com/phacility/libphutil")
      (synopsis "PHP utility library")
      (description
       "@code{libphutil} is a collection of utility classes and functions for
PHP.")
      ;; Bundled libraries are expat-licensed.
      (license (list license:asl2.0 license:expat)))))

(define-public arcanist
  (let ((commit "45a8d22c74a62624e69f5cd6ce901c9ab2658904")
        (revision "1"))
    (package
      (name "arcanist")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/phacility/arcanist")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "13vswhqy9sap6841y93j4mj71dl27vhcivcn3rzyi0cchkhg2ac9"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda _
               (let ((bin (string-append %output "/bin"))
                     (lib (string-append %output "/lib/arcanist")))
                 (mkdir-p lib)
                 (copy-recursively "." lib)
                 (mkdir-p bin)
                 (symlink (string-append lib "/bin/arc")
                          (string-append bin "/arc"))
                 (wrap-program (string-append bin "/arc")
                   `("ARC_PHUTIL_PATH" =
                     (,(string-append (assoc-ref %build-inputs "libphutil")
                                      "/lib/libphutil")))
                   `("PATH" ":" prefix
                     (,@(map (lambda (i)
                               (string-append (assoc-ref %build-inputs i) "/bin"))
                             '("git" "mercurial" "subversion"))))))
               #t))
           (add-before 'reset-gzip-timestamps 'make-compressed-files-writable
             (lambda _
               (for-each make-file-writable
                         (find-files %output ".*\\.t?gz$"))
               #t)))))
      (inputs
       `(("php" ,php)
         ("libphutil" ,libphutil)
         ("git" ,git)
         ("mercurial" ,mercurial)
         ("subversion" ,subversion)))
      (home-page "https://github.com/phacility/arcanist")
      (synopsis "Command-line interface for Phabricator")
      (description
       "Arcanist is the command-line tool for the Phabricator software
development service.  It allows you to interact with Phabricator installs to
send code for review, download patches, transfer files, view status, make API
calls, and various other things.")
      (license license:asl2.0))))
