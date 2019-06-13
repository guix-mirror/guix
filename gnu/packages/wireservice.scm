;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (gnu packages wireservice)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx))

;; Base package definition for packages from https://github.com/wireservice.
;; This is done so we can share how to run tests and build documentation.
(define base-package
  (package
    (name #f)
    (version #f)
    (source #f)
    (home-page #f)
    (synopsis #f)
    (description #f)
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-sphinx" ,python-sphinx)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "nosetests" "tests")))
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/"
                                        ,(package-name this-package)
                                        "-"
                                        ,(package-version this-package))))
               (with-directory-excursion "docs"
                 (for-each
                  (lambda (target)
                    (invoke "make" target)
                    (copy-recursively (string-append "_build/" target)
                                      (string-append doc "/" target)))
                  '("html" "dirhtml" "singlehtml" "text")))
               #t))))))
    (license license:expat)))

(define-public python-leather
  (package
    (inherit base-package)
    (name "python-leather")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wireservice/leather.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ck3dplni99sv4s117cbm07ydwwjsrxkhdy19rnk0iglia1d4s5i"))))
    (native-inputs
     `(("python-cssselect" ,python-cssselect)
       ("python-lxml" ,python-lxml)
       ,@(package-native-inputs base-package)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://leather.rtfd.org")
    (synopsis "Python charting for 80% of humans")
    (description "Leather is a Python charting library for those who need
charts now and don't care if they're perfect.")))
