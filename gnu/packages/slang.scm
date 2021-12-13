;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages slang)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python))

(define-public slang
  (package
    (name "slang")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.jedsoft.org/releases/slang/slang-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "06p379fqn6w38rdpqi98irxi2bf4llb0rja3dlgkqz7nqh7kp7pw"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/Makefile.in"
                    (("/bin/ln") "ln"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f
       #:parallel-build? #f  ; there's at least one race
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'reduce-array-test-size
           ;; Reduce the size of the array, otherwise the array.sl/array.slc
           ;; tests fails with "Unable to create a multi-dimensional array of
           ;; the desired size" on 32 bit systems.
           (lambda _
             (substitute* "src/test/array.sl"
               (("10000,10000,10000,10000,10000,10000")
                "10,10,10,10,10,10"))))
         (add-before 'configure 'substitute-before-config
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ncurses (assoc-ref inputs "ncurses")))
               (substitute* "configure"
                 (("MISC_TERMINFO_DIRS=\"\"")
                  (string-append "MISC_TERMINFO_DIRS="
                                 "\"" ncurses "/share/terminfo" "\"")))))))))
    (inputs
     (list readline zlib libpng pcre ncurses))
    (home-page "https://www.jedsoft.org/slang/")
    (synopsis "Library for interactive applications and extensibility")
    (description
     "S-Lang is a multi-platform programmer's library designed to allow a
developer to create robust multi-platform software.  It provides facilities
required by interactive applications such as display/screen management,
keyboard input, keymaps, and so on.  The most exciting feature of the library
is the slang interpreter that may be easily embedded into a program to make it
extensible.  While the emphasis has always been on the embedded nature of the
interpreter, it may also be used in a stand-alone fashion through the use of
slsh, which is part of the S-Lang distribution.")
    (license license:gpl2+)))

(define-public newt
  (package
    (name "newt")
    (version "0.52.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pagure.io/releases/newt/"
                                  "newt-" version ".tar.gz"))
              (sha256
               (base32
                "0cdvbancr7y4nrj8257y5n45hmhizr8isynagy4fpsnpammv8pi6"))))
    (build-system gnu-build-system)
    (outputs '("out" "python"))
    (inputs
     (list slang popt python fribidi))
    (arguments
     `(#:tests? #f    ; no test suite
       #:configure-flags
       ;; Set the correct RUNPATH in binaries.
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:make-flags
       ;; configure uses a hard-coded search of /usr/include/python* to set
       ;; this variable, and does not allow us to override it from the
       ;; command line.  Fortunately, the Makefile does, so provide it here.
       (list (string-append "PYTHONVERS=python"
                            ,(version-major+minor (package-version python))))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-/usr/bin/install
          (lambda _
            (substitute* "po/Makefile"
              (("/usr/bin/install") "install"))
            #t))
         (add-after
          'install 'move-python
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out  (assoc-ref outputs "out"))
                  (py   (assoc-ref outputs "python"))
                  (ver ,(version-major+minor (package-version python))))
              (mkdir-p (string-append py "/lib"))
              (rename-file (string-append out "/lib/python" ver)
                           (string-append py  "/lib/python" ver))
              #t))))))
    (home-page "https://pagure.io/newt")
    (synopsis "Not Erik's Windowing Toolkit - text mode windowing with slang")
    (description
     "Newt is a windowing toolkit for text mode built from the slang library.
It allows color text mode applications to easily use stackable windows, push
buttons, check boxes, radio buttons, lists, entry fields, labels, and
displayable text.  Scrollbars are supported, and forms may be nested to
provide extra functionality.")
    (license license:lgpl2.0)))
