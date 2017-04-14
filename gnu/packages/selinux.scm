;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages selinux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex))

;; Update the SELinux packages together!

(define-public libsepol
  (package
    (name "libsepol")
    (version "2.6")
    (source (let ((release "20161014"))
              (origin
                (method url-fetch)
                (uri (string-append "https://github.com/SELinuxProject/selinux/"
                                    "archive/" release ".tar.gz"))
                (file-name (string-append "selinux-" release ".tar.gz"))
                (sha256
                 (base32
                  "1dpwynfb6n31928343blac4159g4jbrwxdp61q5yffmxpy3c3czi")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; tests require checkpolicy, which requires libsepol
       #:test-target "test"
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "DESTDIR=" out)
               (string-append "MAN3DIR=" out "/share/man/man3")
               (string-append "MAN5DIR=" out "/share/man/man5")
               (string-append "MAN8DIR=" out "/share/man/man8")
               (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")
               "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir ,name) #t)))))
    (native-inputs
     `(("flex" ,flex)))
    (home-page "https://selinuxproject.org/")
    (synopsis "Library for manipulating SELinux policies")
    (description
     "The libsepol library provides an API for the manipulation of SELinux
binary policies.  It is used by @code{checkpolicy} (the policy compiler) and
similar tools, and programs such as @code{load_policy}, which must perform
specific transformations on binary policies (for example, customizing policy
boolean settings).")
    (license license:lgpl2.1+)))

(define-public checkpolicy
  (package (inherit libsepol)
    (name "checkpolicy")
    (arguments
     `(#:tests? #f ; there is no check target
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "LDLIBS="
                              (assoc-ref %build-inputs "libsepol")
                              "/lib/libsepol.a "
                              (assoc-ref %build-inputs "flex")
                              "/lib/libfl.a")
               "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir ,name) #t)))))
    (inputs
     `(("libsepol" ,libsepol)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (synopsis "Check SELinux security policy configurations and modules")
    (description
     "This package provides the tools \"checkpolicy\" and \"checkmodule\".
Checkpolicy is a program that checks and compiles a SELinux security policy
configuration into a binary representation that can be loaded into the kernel.
Checkmodule is a program that checks and compiles a SELinux security policy
module into a binary representation.")
    ;; GPLv2 only
    (license license:gpl2)))
