;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages nvi)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages ncurses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix utils))

(define-public nvi
  (package
    (name "nvi")
    (version "1.81.6")
    (source
      (origin
        (method url-fetch)
        (uri          ;; sites.google.coma/bostic.com/keithbostic/vi is stale.
          (string-append "http://harrier.slackbuilds.org/misc/nvi-" version
                         ".tar.bz2"))
        (sha256
          (base32 "0nbbs1inyrqds0ywn3ln5slv54v5zraq7lszkg8nsavv4kivhh9l"))
        (patches (search-patches "nvi-assume-preserve-path.patch"
                                 "nvi-dbpagesize-binpower.patch"
                                 "nvi-db4.patch"))
        (modules '((guix build utils)))
        (snippet
          ;; Create a wrapper for the configure script, make it executable.
          '(let ((conf-wrap (open-output-file "configure")))
             (display "#!/bin/sh" conf-wrap)
             (newline conf-wrap)
             (display
               "../nvi-1.81.6/dist/configure --srcdir=../nvi-1.81.6/dist $@"
               conf-wrap)
             (newline conf-wrap)
             (close-output-port conf-wrap)
             (chmod "configure" #o0755)

             ;; Glibc 2.30 removed the deprecated <sys/stropts.h>, so fall back
             ;; to the internal PTY allocation logic.
             (substitute* "ex/ex_script.c"
               (("#ifdef HAVE_SYS5_PTY")
                "#if defined(HAVE_SYS5_PTY) && !defined(__GLIBC__)"))
             #t))))

    (build-system gnu-build-system)
    (arguments
      `(#:out-of-source? #t
        #:configure-flags
        '("--enable-widechar"
          ,@(if (%current-target-system)
                '("vi_cv_sprintf_count=yes")
                '()))
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'fix-configure
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              ;; Replace outdated config.sub and config.guess:
              (with-directory-excursion "dist"
                (for-each (lambda (file)
                            (chmod file #o755)
                            (install-file
                             (string-append
                              (assoc-ref
                               (or native-inputs inputs) "automake")
                              "/share/automake-"
                              ,(version-major+minor
                                (package-version automake))
                              "/" file) "."))
                          '("config.sub")))
              #t)))))
    (inputs
      (list bdb ncurses))
    (native-inputs
     (list automake)) ;Up to date 'config.guess' and 'config.sub'.
    (synopsis "The Berkeley Vi Editor")
    (description
      "Vi is the original screen based text editor for Unix systems.  It is
considered the standard text editor, and is available on almost all Unix
systems.  Nvi is intended as a \"bug-for-bug compatible\" clone of the
original BSD vi editor.  As such, it doesn't have a lot of snazzy features as
do some of the other vi clones such as elvis and vim.  However, if all you
want is vi, this is the one to get.")
    (home-page "https://sites.google.com/a/bostic.com/keithbostic/vi")
    (license bsd-3)))
