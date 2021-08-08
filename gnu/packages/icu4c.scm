;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu))

(define-public icu4c
  (package
   (name "icu4c")
   (version "69.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/unicode-org/icu/releases/download/release-"
                  (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                  "/icu4c-"
                  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                  "-src.tgz"))
            (sha256
             (base32 "0icps0avkwy5df3wwc5kybxcg63hcgk4phdh9g244g0xrmx7pfjc"))))
   (build-system gnu-build-system)
   ;; When cross-compiling, this package needs a source directory of a
   ;; native-build of itself.
   (native-inputs
    `(("python" ,python-minimal)
      ,@(if (%current-target-system)
            `(("icu4c-build-root" ,icu4c-build-root))
            '())))
   (inputs
    (list perl))
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
        ,@(if (target-riscv64?)
            `((add-after 'unpack 'disable-failing-test
                ;; It is unknown why this test is failing.
                (lambda _
                  (substitute* "source/test/intltest/numbertest_api.cpp"
                    (("(TESTCASE_AUTO\\(unitUsage\\));" all)
                     (string-append "//" all))))))
            '())
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

(define-public icu4c-70
  (package
    (inherit icu4c)
    (version "70.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32
                "1m9zgkaf5lyh65nyc6n0n5bs2f5k53nnj1ih6nskpwbvq4l5884d"))))))

(define-public icu4c-67
  (package
    (inherit icu4c)
    (version "67.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32
                "1p6mhvxl0xr2n0g6xdps3mwzwlv6mjsz3xlpm793p9aiybb0ra4l"))))))

(define-public icu4c-68
  (package
    (inherit icu4c)
    (version "68.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4c-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    "-src.tgz"))
              (sha256
               (base32
                "09fng7a80xj8d5r1cgbgq8r47dsw5jsr6si9p2cj2ylhwgg974f7"))))))

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

(define-public java-icu4j
  (package
    (name "java-icu4j")
    (version "70.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/unicode-org/icu/releases/download/release-"
                    (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                    "/icu4j-"
                    (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                    ".tgz"))
              (sha256
               (base32 "0qrs75iyzn19kf54q55jn8wf6xjlpkrihdwqpxm39jdh2hz4cgvj"))))
    (build-system ant-build-system)
    (arguments
     `(#:make-flags
       ,#~(list
           (string-append "-Djunit.core.jar="
                          (car (find-files
                                #$(this-package-native-input "java-junit")
                                ".*.jar$")))
           (string-append "-Djunit.junitparams.jar="
                          (car (find-files
                                #$(this-package-native-input "java-junitparams")
                                ".*.jar$")))
           (string-append "-Djunit.hamcrest.jar="
                          (car (find-files
                                #$(this-package-native-input "java-hamcrest-core")
                                ".*.jar$"))))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "..")))
         (add-before 'build 'remove-ivy
           (lambda _
             ;; This target wants to download ivy and use it to download
             ;; junit.
             (substitute* "build.xml"
               (("depends=\"test-init-junit-dependency\"") ""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/java/")))
               (mkdir-p share)
               (install-file "icu4j.jar" share)))))))
    (native-inputs
     (list java-junit java-junitparams java-hamcrest-core))
    (home-page "http://site.icu-project.org/")
    (synopsis "International Components for Unicode")
    (description
     "ICU is a set of C/C++ and Java libraries providing Unicode and
globalisation support for software applications.  This package contains the
Java part.")
    (license x11)))
