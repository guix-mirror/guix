;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Robin Templeton <robin@igalia.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-public arcanist
  (let ((commit "ceb082ef6b2919d76a90d4a53ca84f5b1e0c2c06")
        (revision "2"))
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
                  "16590nywh3cpm2yq4igw3nfa8g84kwza215mrnqr2k6b2cqzjak3"))))
      (build-system gnu-build-system)
      ;; TODO: Unbundle jsonlint
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (lib (string-append out "/lib/arcanist")))
                 (mkdir-p lib)
                 (copy-recursively "." lib)
                 (mkdir-p bin)
                 (symlink (string-append lib "/bin/arc")
                          (string-append bin "/arc"))
                 (wrap-program (string-append bin "/arc")
                   `("PATH" ":" prefix
                     (,@(map (lambda (i)
                               (string-append (assoc-ref %build-inputs i) "/bin"))
                             '("php" "git" "mercurial" "subversion")))))))))))
      (inputs
       (list php git mercurial subversion))
      (home-page "https://github.com/phacility/arcanist")
      (synopsis "Command-line interface for Phabricator")
      (description
       "Arcanist is the command-line tool for the Phabricator software
development service.  It allows you to interact with Phabricator installs to
send code for review, download patches, transfer files, view status, make API
calls, and various other things.")
      ;; Bundled libraries are expat-licensed.
      (license (list license:asl2.0 license:expat)))))
