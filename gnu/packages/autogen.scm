;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages autogen)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile))

(define-public autogen
  (package
    (name "autogen")
    (version "5.18.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/autogen/rel" version
                           "/autogen-" version ".tar.xz"))
       (sha256
        (base32 "16mlbdys8q4ckxlvxyhwkdnh1ay9f6g0cyp1kylkpalgnik398gq"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("which" ,which)))
    (inputs `(("guile" ,guile-2.2)
              ("perl" ,perl)))          ; for doc generator mdoc
    (arguments
     '(#:configure-flags
       ;; XXX Needed to build 5.18.16.  ./configure fails without it:
       ;; “Something went wrong bootstrapping makefile fragments for
       ;;  automatic dependency tracking.  Try re-running configure with […]”
       (list "--disable-dependency-tracking")
       #:phases
       (modify-phases %standard-phases
         (add-before 'patch-source-shebangs 'patch-test-scripts
           (lambda _
             (let ((sh (which "sh")))
               (substitute*
                   (append (find-files "agen5/test" "\\.test$")
                           (find-files "autoopts/test" "\\.(test|in)$"))
                 (("/bin/sh") sh))
               #t))))))
    (home-page "https://www.gnu.org/software/autogen/")
    (synopsis "Automated program generator")
    (description
     "AutoGen is a program to ease the maintenance of programs that contain
large amounts of repetitive text.  It automates the construction of these
sections of the code, simplifying the task of keeping the text in sync.  It
also includes an add-on package called AutoOpts, which is specialized for the
maintenance and documentation of program options.")
    (license gpl3+)))
