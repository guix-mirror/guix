;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu packages icu4c)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu))

(define-public icu4c
  (package
   (name "icu4c")
   (replacement icu4c/fixed)
   (version "66.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/unicode-org/icu/releases/download/release-"
                  (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                  "/icu4c-"
                  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                  "-src.tgz"))
            (sha256
             (base32 "0bharwzc9nzkbrcf405z2nb3h7q0711z450arz0mjmdrk8hg58sj"))))
   (build-system gnu-build-system)
   ;; When cross-compiling, this package needs a source directory of a
   ;; native-build of itself.
   (native-inputs
    `(("python" ,python-minimal)
      ,@(if (%current-target-system)
            `(("icu4c-build-root" ,icu4c-build-root))
            '())))
   (inputs
    `(("perl" ,perl)))
   (arguments
    `(#:configure-flags
      (list
       "--enable-rpath"
        ,@(if (%current-target-system)
              '((string-append "--with-cross-build="
                                (assoc-ref %build-inputs "icu4c-build-root")))
              '()))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'chdir-to-source
          (lambda _ (chdir "source") #t))
        (add-after 'chdir-to-source 'update-LDFLAGS
          (lambda _
            ;; Do not create a "data-only" libicudata.so because it causes
            ;; problems on some architectures (notably armhf and MIPS).
            (substitute* "config/mh-linux"
              (("LDFLAGSICUDT=-nodefaultlibs -nostdlib")
               "LDFLAGSICUDT="))
            #t))
        (add-after 'install 'avoid-coreutils-reference
          ;; Don't keep a reference to the build tools.
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* (find-files (string-append out "/lib/icu")
                                       "\\.inc$")
                (("INSTALL_CMD=.*/bin/install") "INSTALL_CMD=install"))
              #t))))))
   (synopsis "International Components for Unicode")
   (description
    "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications.  This package contains the
C/C++ part.")
   (license x11)
   (home-page "http://site.icu-project.org/")))

(define-public icu4c-build-root
  (package
    (inherit icu4c)
    (name "icu4c-build-root")
    (arguments
     (substitute-keyword-arguments (package-arguments icu4c)
       ((#:tests? _ '())
        #f)
       ((#:out-of-source? _ '())
        #t)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "../build" out)
                 #t)))))))
    (native-inputs '())))

(define icu4c/fixed
  (package
    (inherit icu4c)
    (source (origin
              (inherit (package-source icu4c))
              (patch-flags '("-p2"))
              (patches (append
                         (origin-patches (package-source icu4c))
                         (search-patches
                           "icu4c-CVE-2020-10531.patch")))))))

(define-public java-icu4j
  (package
    (name "java-icu4j")
    (version "59.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.icu-project.org/files/icu4j/"
                                  version "/icu4j-"
                                  (string-map (lambda (x)
                                                (if (char=? x #\.) #\_ x))
                                              version)
                                  "-src.jar"))
              (sha256
               (base32
                "0bgxsvgi0qcwj60pvcxrf7a3fbk7aksyxnfwpbzavyfrfzixqh0c"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:jar-name "icu4j.jar"))
    (home-page "http://site.icu-project.org/")
    (synopsis "International Components for Unicode")
    (description
     "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications.  This package contains the
Java part.")
    (license x11)))
