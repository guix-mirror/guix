;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
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

(define-module (gnu packages rednotebook)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages python-xyz))

(define-public rednotebook
  (package
    (name "rednotebook")
    (version "2.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jendrikseipp/rednotebook")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11n970ad0j57vlll5j30ngkrfyil23v1b29ickbnblcldvjbgwa5"))))
    (build-system python-build-system)
    (arguments
     ;; Tests fail to find the "_" function.
     ;; It should be defined in rednotebook/info.py if '_' is not a member of
     ;; 'builtins'. It is either not defined or not exported during the check
     ;; phase. The program does not have this problem after it is installed.
     ;; TODO: Fix tests.
     `(#:tests? #f
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((ice-9 match)
                  (guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         ;; Make sure rednotebook can find the typelibs and webkitgtk shared
         ;; libraries.
         (add-before 'wrap 'wrap-with-library-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                   (webkitgtk-path (string-append
                                    (assoc-ref inputs "webkitgtk")
                                    "/lib")))
               (wrap-program (string-append out "/bin/rednotebook")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("LD_LIBRARY_PATH" ":" prefix (,webkitgtk-path)))
               #t))))))
    (inputs
     (list gtk+ gtksourceview-3 python-pyyaml python-pygobject webkitgtk))
    ;; TODO: package the following for python3 (if possible), add them as
    ;; dependencies, and remove them from rednotebook source:
    ;; pygtkspellcheck, elib.intl, msgfmt, txt2tags
    ;; TODO: package and add pyenchant for python3 and add it as a dependency.
    (home-page "https://www.rednotebook.app")
    (synopsis "Daily journal with calendar, templates and keyword searching")
    (description
     "RedNotebook is a modern desktop journal.  It lets you format, tag and
search your entries.  You can also add pictures, links and customizable
templates, spell check your notes, and export to plain text, HTML, Latex or
PDF.")
    (license (list license:gpl2+     ; rednotebook, txt2tags
                   license:lgpl3+    ; elib.intl
                   license:gpl3+)))) ; pygtkspellcheck
