;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages perl)
  #:use-module (distro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public perl
  ;; Yeah, Perl...  It is required early in the bootstrap process by Linux.
  (package
    (name "perl")
    (version "5.16.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.cpan.org/src/5.0/perl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "15qxzba3a50c9nik5ydgyfp62x7h9vxxn12yd1jgl93hb1wj96km"))))
    (build-system gnu-build-system)
    (arguments
     (lambda (system)
       `(#:tests? #f
         #:patches (list (assoc-ref %build-inputs "patch/no-sys-dirs"))
         #:phases
         (alist-replace
          'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out  (assoc-ref outputs "out"))
                  (libc (assoc-ref inputs "libc"))
                  (pwd  (search-path (search-path-as-string->list
                                      (getenv "PATH"))
                                     "pwd")))
              ;; Use the right path for `pwd'.
              (substitute* "dist/Cwd/Cwd.pm"
                (("/bin/pwd") pwd))

              (zero?
               (system* "/bin/sh" "./Configure"
                        (string-append "-Dprefix=" out)
                        (string-append "-Dman1dir=" out "/share/man/man1")
                        (string-append "-Dman3dir=" out "/share/man/man3")
                        "-de" "-Dcc=gcc"
                        "-Uinstallusrbinperl"
                        "-Dinstallstyle=lib/perl5"
                        "-Duseshrplib"
                        (string-append "-Dlocincpth=" libc "/include")
                        (string-append "-Dloclibpth=" libc "/lib")))))
          %standard-phases))))
    (inputs `(("patch/no-sys-dirs" ,(search-patch "perl-no-sys-dirs.patch"))))
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl 5 is a highly capable, feature-rich programming language with over
24 years of development.")
    (home-page "http://www.perl.org/")
    (license "GPLv1+")))                          ; or "Artistic"
