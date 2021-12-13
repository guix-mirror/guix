;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
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

(define-module (gnu packages lego)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages qt))

(define-public nqc
  (package
    (name "nqc")
    (version "3.1.r6")
    (source (origin
              ;; XXX Using url-fetch/tarbomb results in failure:
              ;; Error: could not create compiler/rcx1_nqh.h
              (method url-fetch)
              (uri (string-append "http://bricxcc.sourceforge.net/nqc/release/"
                                  "nqc-" version ".tgz"))
              (sha256
               (base32
                "0rp7pzr8xrdxpv75c2mi8zszzz2ypli4vvzxiic7mbrryrafdmdz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("add-usb-tcp-support.patch"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://sourceforge.net/p/bricxcc/patches/"
                               "_discuss/thread/00b427dc/b84b/attachment/"
                               "nqc-01-Linux_usb_and_tcp.diff"))
           (sha256
            (base32 "0z5gx55ra1kamhhqxz08lvvwslfl36pbmwdd566rhmbgmyhlykbr"))))
       ("debian-writable-swap-inst-len.patch"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://sources.debian.org/data/main/n/nqc/"
                               "3.1.r6-7/debian/patches/"
                               "writable-swap-inst-len.patch"))
           (sha256
            (base32 "1kr7j057aa5i0kxmlfpbfcsif5yq2lrmjw4sljn400ijaq4mys3v"))))))
    (arguments
     '(#:tests? #f                      ;no tests
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'rm-generated
           ;; Regenerating compiler/lexer.cpp avoids an 'undefined
           ;; reference to `isatty(int)'' error.
           (lambda _
             (for-each delete-file
                       '("compiler/lexer.cpp"
                         "compiler/parse.cpp"))
             #t))
         (add-after 'unpack 'deal-with-tarbomb
           (lambda _
             (chdir "..")               ;tarbomb
             #t))
         (add-after 'deal-with-tarbomb 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (patch)
                         (invoke "patch" "-Np1" "-i"
                                 (assoc-ref inputs patch)))
                       (list "add-usb-tcp-support.patch"
                             "debian-writable-swap-inst-len.patch")))))))
    (home-page "http://bricxcc.sourceforge.net/nqc/")
    (synopsis "C-like language for Lego's MINDSTORMS")
    (description
     "Not Quite C (NQC) is a simple language for programming several Lego
MINDSTORMS products.  The preprocessor and control structures of NQC are very
similar to C.  NQC is not a general purpose language -- there are many
restrictions that stem from limitations of the standard RCX firmware.")
    (license license:mpl1.0)))

(define-public leocad
  (package
    (name "leocad")
    (version "21.06")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/leozide/leocad")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ifbxngkbmg6d8vv08amxbnfvlyjdwzykrjp98lbwvgb0b843ygq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list qttools))           ; for lrelease
    (inputs
     (list mesa qtbase-5 zlib))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "INSTALL_PREFIX=" out)
                       ;; Otherwise looks for lrelease-qt4.
                       "QMAKE_LRELEASE=lrelease"
                       ;; Don't pester users about updates.
                       "DISABLE_UPDATE_CHECK=1")
               #t)))
         (add-after 'configure 'reset-resource-timestamps
           (lambda _
             ;; The contents of build/release/.qrc/qrc_leocad.cpp generated by
             ;; qt's rcc tool depends on the timestamps in resources/*, in
             ;; particular the leocad_*.qm files that are created by qmake
             ;; above.  So reset those timestamps for a reproducible build.
             (with-directory-excursion "resources"
               (for-each (lambda (file)
                           (let* ((base (basename file ".qm"))
                                  (src (string-append base ".ts"))
                                  (st (stat src)))
                             (set-file-time file st)))
                         (find-files "." "leocad_.*\\.qm")))
             #t)))))
    (home-page "https://www.leocad.org")
    (synopsis "Create virtual Lego models")
    (description
     "LeoCAD is a program for creating virtual LEGO models.  It has an
intuitive interface, designed to allow new users to start creating new models
without having to spend too much time learning the application.  LeoCAD is
fully compatible with the LDraw Standard and related tools.")
    (license license:gpl2+)))
