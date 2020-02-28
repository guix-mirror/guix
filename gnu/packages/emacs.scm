;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jesse John Gildersleve <jessejohngildersleve@zohomail.eu>
;;; Copyright © 2019 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2019 Leo Prikler <leo.prikler@student.tugraz.at>
;;; Copyright © 2019 Amin Bandali <mab@gnu.org>
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

(define-module (gnu packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)     ; for librsvg
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)     ; alsa-lib
  #:use-module (gnu packages mail)      ; for mailutils
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)       ; for jansson
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public emacs
  (package
    (name "emacs")
    (version "26.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/emacs/emacs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "119ldpk7sgn9jlpyngv5y4z3i7bb8q3xp4p0qqi7i5nq39syd42d"))
             (patches (search-patches "emacs-exec-path.patch"
                                      "emacs-fix-scheme-indent-function.patch"
                                      "emacs-source-date-epoch.patch"))
             (modules '((guix build utils)))
             (snippet
              '(with-directory-excursion "lisp"
                 ;; Delete the bundled byte-compiled elisp files and generated
                 ;; autoloads.
                 (for-each delete-file
                           (append (find-files "." "\\.elc$")
                                   (find-files "." "loaddefs\\.el$")
                                   ;; This is the only "autoloads" file that
                                   ;; does not have "*loaddefs.el" name.
                                   ;; TODO: Next time changing this package,
                                   ;; replace the following with a call to
                                   ;; `find-files', so that `delete-file'
                                   ;; wouldn't error out when the file is
                                   ;; missing, making the entire snippet field
                                   ;; reusable as-is for `emacs-next' below.
                                   '("eshell/esh-groups.el")))

                 ;; Make sure Tramp looks for binaries in the right places on
                 ;; remote Guix System machines, where 'getconf PATH' returns
                 ;; something bogus.
                 (substitute* "net/tramp-sh.el"
                   ;; Patch the line after "(defcustom tramp-remote-path".
                   (("\\(tramp-default-remote-path")
                    (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                            "~/.guix-profile/bin" "~/.guix-profile/sbin"
                            "/run/current-system/profile/bin"
                            "/run/current-system/profile/sbin")))

                 ;; Make sure Man looks for C header files in the right
                 ;; places.
                 (substitute* "man.el"
                   (("\"/usr/local/include\"" line)
                    (string-join
                     (list line
                           "\"~/.guix-profile/include\""
                           "\"/var/guix/profiles/system/profile/include\"")
                     " ")))
                 #t))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags (list "--with-modules"
                               "--disable-build-details")
       #:modules ((guix build emacs-utils)
                  (guix build glib-or-gtk-build-system)
                  (guix build utils))
       #:imported-modules ((guix build emacs-utils)
                           ,@%glib-or-gtk-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/pwd
           (lambda _
             ;; Use `pwd', not `/bin/pwd'.
             (substitute* (find-files "." "^Makefile\\.in$")
               (("/bin/pwd")
                "pwd"))
             #t))
         (add-after 'install 'install-site-start
           ;; Use 'guix-emacs' in "site-start.el", which is used autoload the
           ;; Elisp packages found in EMACSLOADPATH.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (lisp-dir (string-append out "/share/emacs/site-lisp"))
                    (emacs (string-append out "/bin/emacs")))
               (copy-file (assoc-ref inputs "guix-emacs.el")
                          (string-append lisp-dir "/guix-emacs.el"))
               (with-output-to-file (string-append lisp-dir "/site-start.el")
                 (lambda ()
                   (display
                    (string-append "(when (require 'guix-emacs nil t)\n"
                                   "  (guix-emacs-autoload-packages))\n"))))
               ;; Remove the extraneous subdirs.el file, as it causes Emacs to
               ;; add recursively all the the sub-directories of a profile's
               ;; share/emacs/site-lisp union when added to EMACSLOADPATH,
               ;; which leads to conflicts.
               (delete-file (string-append lisp-dir "/subdirs.el"))
               ;; Byte compile the site-start files.
               (parameterize ((%emacs emacs))
                 (emacs-byte-compile-directory lisp-dir))
               #t))))))
    (inputs
     `(("gnutls" ,gnutls)
       ("ncurses" ,ncurses)

       ;; Avoid Emacs's limited movemail substitute that retrieves POP3 email
       ;; only via insecure channels.  This is not needed for (modern) IMAP.
       ("mailutils" ,mailutils)

       ;; TODO: Add the optional dependencies.
       ("libx11" ,libx11)
       ("gtk+" ,gtk+)
       ("libxft" ,libxft)
       ("libtiff" ,libtiff)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg)
       ("imagemagick" ,imagemagick)
       ("acl" ,acl)

       ;; When looking for libpng `configure' links with `-lpng -lz', so we
       ;; must also provide zlib as an input.
       ("libpng" ,libpng)
       ("zlib" ,zlib)

       ("librsvg" ,librsvg)
       ("libxpm" ,libxpm)
       ("libxml2" ,libxml2)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)

       ;; multilingualization support
       ("libotf" ,libotf)
       ("m17n-lib" ,m17n-lib)))
    (native-inputs
     `(("guix-emacs.el" ,(search-auxiliary-file "emacs/guix-emacs.el"))
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))

    (native-search-paths
     (list (search-path-specification
            (variable "EMACSLOADPATH")
            ;; The versioned entry is for the Emacs' builtin libraries.
            (files (list "share/emacs/site-lisp"
                         (string-append "share/emacs/" version "/lisp"))))
           (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))))

    (home-page "https://www.gnu.org/software/emacs/")
    (synopsis "The extensible, customizable, self-documenting text editor")
    (description
     "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
    (license license:gpl3+)))

(define-public emacs-next
  (let ((commit "36abf6864604b3061c2e070f8997491fa2bce44c")
        (revision "0")
        (emacs-version "27.0.50"))
    (package
      (inherit emacs)
      (name "emacs-next")
      (version (git-version emacs-version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git")
               (commit commit)))
         (sha256
          (base32 "1ckn607p0clz0dhhlizvv7l03p4nminy48h53xrpz55w4rcrcm2l"))
         (file-name (git-file-name name version))
         (patches (search-patches "emacs27-exec-path.patch"
                                  "emacs-fix-scheme-indent-function.patch"
                                  "emacs-source-date-epoch.patch"))
         (modules (origin-modules (package-source emacs)))
         ;; TODO: once the snippet for `emacs' is changed to not fail when
         ;; eshell/esh-groups.el does not exist, replace this snippet with
         ;; (snippet (origin-snippet (package-source emacs))))).
         (snippet
          '(with-directory-excursion "lisp"
             ;; Make sure Tramp looks for binaries in the right places on
             ;; remote Guix System machines, where 'getconf PATH' returns
             ;; something bogus.
             (substitute* "net/tramp-sh.el"
               ;; Patch the line after "(defcustom tramp-remote-path".
               (("\\(tramp-default-remote-path")
                (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
                        "~/.guix-profile/bin" "~/.guix-profile/sbin"
                        "/run/current-system/profile/bin"
                        "/run/current-system/profile/sbin")))

             ;; Make sure Man looks for C header files in the right
             ;; places.
             (substitute* "man.el"
               (("\"/usr/local/include\"" line)
                (string-join
                 (list line
                       "\"~/.guix-profile/include\""
                       "\"/var/guix/profiles/system/profile/include\"")
                 " ")))
             #t))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; The 'reset-gzip-timestamps phase will throw a
             ;; permission error if gzip files aren't writable then
             (add-before
                 'reset-gzip-timestamps
                 'make-compressed-files-writable
               (lambda _
                 (for-each make-file-writable
                           (find-files %output ".*\\.t?gz$"))
                 #t))
             ;; restore the dump file that Emacs installs somewhere in
             ;; libexec/ to its original state
             (add-after 'glib-or-gtk-wrap 'restore-emacs-pdmp
               (lambda* (#:key outputs target #:allow-other-keys)
                 (let* ((libexec (string-append (assoc-ref outputs "out")
                                                "/libexec"))
                        ;; each of these find-files should return one file
                        (pdmp (find-files libexec "^emacs\\.pdmp$"))
                        (pdmp-real (find-files libexec
                                               "^\\.emacs\\.pdmp-real$")))
                   (for-each (lambda (wrapper real)
                               (delete-file wrapper)
                               (rename-file real wrapper))
                             pdmp pdmp-real)
                   #t)))))))
      (inputs
       `(("jansson" ,jansson)
         ,@(package-inputs emacs)))
      (native-inputs
       `(("autoconf" ,autoconf)      ; needed when building from trunk
         ,@(package-native-inputs emacs)))

      ;; TODO: consider changing `emacs' to use a more robust way of
      ;; specifying version for "EMACSLOADPATH", so as to avoid having to
      ;; duplicate native-search-paths here.
      (native-search-paths
       (list (search-path-specification
              (variable "EMACSLOADPATH")
              ;; The versioned entry is for the Emacs' builtin libraries.
              (files
               (list "share/emacs/site-lisp"
                     (string-append "share/emacs/" emacs-version "/lisp"))))
             (search-path-specification
              (variable "INFOPATH")
              (files '("share/info"))))))))

(define-public emacs-minimal
  ;; This is the version that you should use as an input to packages that just
  ;; need to byte-compile .el files.
  (package (inherit emacs)
    (name "emacs-minimal")
    (synopsis "The extensible text editor (used only for byte-compilation)")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:imported-modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'install-site-start)))
       ((#:configure-flags flags ''())
        `(list "--with-gnutls=no" "--disable-build-details"))))
    (inputs
     `(("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))))

(define-public emacs-xwidgets
  (package
    (inherit emacs)
    (name "emacs-xwidgets")
    (synopsis "The extensible, customizable, self-documenting text
editor (with xwidgets support)")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:imported-modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:configure-flags flags ''())
        `(cons "--with-xwidgets" ,flags))))
    (inputs
     `(("webkitgtk" ,webkitgtk)
       ("libxcomposite" ,libxcomposite)
       ,@(package-inputs emacs)))))

(define-public emacs-no-x
  (package (inherit emacs)
    (name "emacs-no-x")
    (synopsis "The extensible, customizable, self-documenting text
editor (console only)")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:imported-modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))))
    (inputs (fold alist-delete
                  (package-inputs emacs)
                  '("libx11" "gtk+" "libxft" "libtiff" "giflib" "libjpeg"
                    "imagemagick" "libpng" "librsvg" "libxpm" "libice"
                    "libsm"

                    ;; These depend on libx11, so remove them as well.
                    "libotf" "m17n-lib" "dbus")))))

(define-public emacs-no-x-toolkit
  (package (inherit emacs)
    (name "emacs-no-x-toolkit")
    (synopsis "The extensible, customizable, self-documenting text
editor (without an X toolkit)" )
    (build-system gnu-build-system)
    (inputs (append `(("inotify-tools" ,inotify-tools))
                    (alist-delete "gtk+" (package-inputs emacs))))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs)
       ((#:modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:imported-modules _)
        `((guix build emacs-utils)
          ,@%gnu-build-system-modules))
       ((#:configure-flags cf)
        `(cons "--with-x-toolkit=no" ,cf))))))

(define-public guile-emacs
  (let ((commit "41120e0f595b16387eebfbf731fff70481de1b4b")
        (revision "0"))
    (package (inherit emacs)
      (name "guile-emacs")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.hcoop.net/git/bpt/emacs.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (patches (search-patches "guile-emacs-fix-configure.patch"))
                (sha256
                 (base32
                  "0lvcvsz0f4mawj04db35p1dvkffdqkz8pkhc0jzh9j9x2i63kcz6"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("guile" ,guile-for-guile-emacs)
         ,@(package-native-inputs emacs)))
      (arguments
       (substitute-keyword-arguments `(;; Build fails if we allow parallel build.
                                       #:parallel-build? #f
                                       ;; Tests aren't passing for now.
                                       #:tests? #f
                                       ,@(package-arguments emacs))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'autogen
               (lambda _
                 (invoke "sh" "autogen.sh")))
             ;; Build sometimes fails: deps/dispnew.d: No such file or directory
             (add-before 'build 'make-deps-dir
               (lambda _
                 (invoke "mkdir" "-p" "src/deps"))))))))))

(define-public m17n-db
  (package
    (name "m17n-db")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-db-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0vfw7z9i2s9np6nmx1d4dlsywm044rkaqarn7akffmb6bf1j6zv5"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gettext-minimal)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-charmaps="
                            (assoc-ref %build-inputs "libc")
                            "/share/i18n/charmaps"))))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (database)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library database.")
    (license license:lgpl2.1+)))

(define-public m17n-lib
  (package
    (name "m17n-lib")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/m17n/m17n-lib-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0jp61y09xqj10mclpip48qlfhniw8gwy8b28cbzxy8hq8pkwmfkq"))))
    (build-system gnu-build-system)
    (inputs
     `(("fribidi" ,fribidi)
       ("gd" ,gd)
       ("libotf" ,libotf)
       ("libxft" ,libxft)
       ("libxml2" ,libxml2)
       ("m17n-db" ,m17n-db)))
    (arguments
     `(#:parallel-build? #f))
    ;; With `guix lint' the home-page URI returns a small page saying
    ;; that your browser does not handle frames. This triggers the "URI
    ;; returns suspiciously small file" warning.
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Multilingual text processing library (runtime)")
    (description "The m17n library realizes multilingualization of
many aspects of applications.  The m17n library represents
multilingual text as an object named M-text.  M-text is a string with
attributes called text properties, and designed to substitute for
string in C.  Text properties carry any information required to input,
display and edit the text.

This package contains the library runtime.")
    (license license:lgpl2.1+)))
