;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu packages patchutils)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages less)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml))

(define-public patchutils
  (package
    (name "patchutils")
    (version "0.3.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://cyberelk.net/tim/data/patchutils/stable/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0xp8mcfyi5nmb5a2zi5ibmyshxkb1zv1dgmnyn413m7ahgdx8mfg"))
      (patches (search-patches "patchutils-test-perms.patch"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-test-scripts
           (lambda _
             (substitute* (find-files "tests" "^run-test$")
               (("/bin/echo") (which "echo")))
             #t))
         (add-after 'install 'wrap-program
           ;; Point installed scripts to the utilities they need.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (diffutils (assoc-ref inputs "diffutils"))
                    (sed       (assoc-ref inputs "sed"))
                    (gawk      (assoc-ref inputs "gawk")))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/bin/" prog)
                    `("PATH" ":" prefix
                      ,(map (lambda (dir)
                              (string-append dir "/bin"))
                            (list diffutils sed gawk)))))
                '("dehtmldiff" "editdiff" "espdiff")))
             #t)))))
    (home-page "http://cyberelk.net/tim/software/patchutils")
    (synopsis "Collection of tools for manipulating patch files")
    (description
     "Patchutils is a collection of programs that can manipulate patch files
in useful ways such as interpolating between two pre-patches, combining two
incremental patches, fixing line numbers in hand-edited patches, and simply
listing the files modified by a patch.")
    (license gpl2+)))

(define-public quilt
  (package
    (name "quilt")
    (version "0.66")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/quilt/"
                           "quilt-" version ".tar.gz"))
       (sha256
        (base32 "01vfvk4pqigahx82fhaaffg921ivd3k7rylz1yfvy4zbdyd32jri"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gnu-gettext)))
    (inputs `(("perl" ,perl)
              ("less" ,less)
              ("file" ,file)
              ("ed" ,ed)
              ("diffstat" ,diffstat)))
    (arguments
     '(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-tests
           (lambda _
             (substitute*
                 '("test/run"
                   "test/edit.test")
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'install 'wrap-program
           ;; quilt's configure checks for the absolute path to the utilities it
           ;; needs, but uses only the name when invoking them, so we need to
           ;; make sure the quilt script can find those utilities when run.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (diffutils (assoc-ref inputs "diffutils"))
                    (findutils (assoc-ref inputs "findutils"))
                    (diffstat  (assoc-ref inputs "diffstat"))
                    (less      (assoc-ref inputs "less"))
                    (file      (assoc-ref inputs "file"))
                    (ed        (assoc-ref inputs "ed"))
                    (sed       (assoc-ref inputs "sed"))
                    (bash      (assoc-ref inputs "bash"))
                    (grep      (assoc-ref inputs "grep")))
               (wrap-program (string-append out "/bin/quilt")
                 `("PATH" ":" prefix
                   ,(map (lambda (dir)
                           (string-append dir "/bin"))
                         (list coreutils diffutils findutils
                               less file ed sed bash grep
                               diffstat)))))
             #t)))))
    (home-page "https://savannah.nongnu.org/projects/quilt/")
    (synopsis "Script for managing patches to software")
    (description
     "Quilt allows you to easily manage large numbers of patches by keeping
track of the changes each patch makes.  Patches can be applied, un-applied,
refreshed, and more.")
    (license gpl2)))

(define-public colordiff
  (package
    (name "colordiff")
    (version "1.0.18")
    (source
      (origin
        (method url-fetch)
        (uri (list (string-append "https://www.colordiff.org/colordiff-"
                                  version ".tar.gz")
                   (string-append "http://www.colordiff.org/archive/colordiff-"
                                  version ".tar.gz")))
      (sha256
       (base32
        "1q6n60n4b9fnzccxyxv04mxjsql4ddq17vl2c74ijvjdhpcfrkr9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "INSTALL_DIR=/bin" "MAN_DIR=/share/man/man1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'build))))             ; nothing to build
    (inputs
     `(("perl" ,perl)
       ("xmlto" ,xmlto)))
    (home-page "https://www.colordiff.org")
    (synopsis "Display diff output with colors")
    (description
     "Colordiff is Perl script wrapper on top of diff command which provides
'syntax highlighting' for various patch formats.")
    (license gpl2+)))

(define-public patches
  (let ((commit "ef1b8a7d954b82ed4af3a08fd63d2085d19090ef"))
    (package
      (name "patches")
      (home-page "https://github.com/stefanha/patches")
      (version (string-append "0.0-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "11rdmhv0l1s8nqb20ywmw2zqizczch2p62qf9apyx5wqgxlnjshk"))
                (file-name (string-append name "-"version "-checkout"))))
      (build-system python-build-system)
      (inputs `(("python-notmuch" ,python2-notmuch)))
      (arguments
       `(#:tests? #f                             ;no "test" target
         #:python ,python-2))                    ;not compatible with Python 3
      (synopsis "Patch tracking tool")
      (description
       "@code{Patches} is a patch-tracking tool initially written for the QEMU
project.  It provides commands that build a database of patches from a mailing
list, and commands that can search that database.  It allows users to track
the status of a patch, apply patches, and search for patches---all that from
the command-line or from Emacs via its Notmuch integration.")
      (license gpl2+))))

(define-public vbindiff
  (package
    (name "vbindiff")
    (version "3.0_beta5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cjmweb.net/vbindiff/vbindiff-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1f1kj4jki08bnrwpzi663mjfkrx4wnfpzdfwd2qgijlkx5ysjkgh"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://www.cjmweb.net/vbindiff/")
    (synopsis "Console-based tool for comparing binary data")
    (description "Visual Binary Diff (@command{vbindiff}) displays files in
hexadecimal and ASCII (or EBCDIC).  It can also display two files at once, and
highlight the differences between them.  It works well with large files (up to 4
GiB).")
    (license gpl2+)))

(define-public meld
  (package
    (name "meld")
    (version "3.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/meld/"
                           (version-major+minor version)
                           "/meld-" version ".tar.xz"))
       (sha256
        (base32 "0jdj7kd6vj1mdc16gvrj1kar88b2j5875ajq18fx7cbc9ny46j55"))))
    (build-system python-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("xmllint" ,libxml2)
       ("glib-compile-schemas" ,glib "bin")
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("python-cairo" ,python-pycairo)
       ("python-gobject" ,python-pygobject)
       ("gtksourceview" ,gtksourceview)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py"
                     ;; This setup.py runs gtk-update-icon-cache  which we don't want.
                     "--no-update-icon-cache"
                     ;; "--no-compile-schemas"
                     "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/")))
         ;; The tests need to be run after installation.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Tests look for installed package
             (add-installed-pythonpath inputs outputs)
             ;; The tests fail when HOME=/homeless-shelter.
             (setenv "HOME" "/tmp")
             (invoke "py.test" "-v" "-k"
                     ;; TODO: Those tests fail, why?
                     "not test_classify_change_actions"))))))
    (home-page "https://meldmerge.org/")
    (synopsis "Compare files, directories and working copies")
    (description "Meld is a visual diff and merge tool targeted at
developers.  Meld helps you compare files, directories, and version controlled
projects.  It provides two- and three-way comparison of both files and
directories, and has support for many popular version control systems.

Meld helps you review code changes and understand patches.  It might even help
you to figure out what is going on in that merge you keep avoiding.")
    (license gpl2)))

(define-public patchwork
  (package
    (name "patchwork")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/getpatchwork/patchwork.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06ng5pv6744w98zkyfm0ldkmpdgnsql3gbbbh6awq61sr2ndr3qw"))))
    (build-system python-build-system)
    (arguments
     `(;; TODO: Tests require a running database
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'replace-wsgi.py
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (delete-file "patchwork/wsgi.py")
             (call-with-output-file "patchwork/wsgi.py"
               (lambda (port)
                 ;; Embed the PYTHONPATH containing the dependencies, as well
                 ;; as the python modules in this package in the wsgi.py file,
                 ;; as this will ensure they are available at runtime.
                 (define pythonpath
                   (string-append (getenv "PYTHONPATH")
                                  ":"
                                  (site-packages inputs outputs)))
                 (display
                  (string-append "
import os, sys

sys.path.extend('" pythonpath "'.split(':'))

from django.core.wsgi import get_wsgi_application

# By default, assume that patchwork is running as a Guix service, which
# provides the settings as the 'guix.patchwork.settings' Python module.
#
# When using httpd, it's hard to set environment variables, so rely on the
# default set here.
os.environ['DJANGO_SETTINGS_MODULE'] = os.getenv(
  'DJANGO_SETTINGS_MODULE',
  'guix.patchwork.settings' # default
)

application = get_wsgi_application()\n") port)))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "DJANGO_SETTINGS_MODULE" "patchwork.settings.dev")
               (invoke "python" "-Wonce" "./manage.py" "test" "--noinput"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (out-site-packages (site-packages inputs outputs)))
               (for-each (lambda (directory)
                           (copy-recursively
                            directory
                            (string-append out-site-packages directory)))
                         '(;; Contains the python code
                           "patchwork"
                           ;; Contains the templates for the generated HTML
                           "templates"))
               (delete-file-recursively
                (string-append out-site-packages "patchwork/tests"))

               ;; Install patchwork related tools
               (for-each (lambda (file)
                           (install-file file (string-append out "/bin")))
                         (list
                          (string-append out-site-packages
                                         "patchwork/bin/pwclient")
                          (string-append out-site-packages
                                         "patchwork/bin/parsemail.sh")
                          (string-append out-site-packages
                                         "patchwork/bin/parsemail-batch.sh")))

               ;; Delete the symlink to pwclient, and replace it with the
               ;; actual file, as this can cause issues when serving the file
               ;; from a webserver.
               (let ((template-pwclient (string-append
                                         out-site-packages
                                         "patchwork/templates/patchwork/pwclient")))
                 (delete-file template-pwclient)
                 (copy-file (string-append out-site-packages
                                           "patchwork/bin/pwclient")
                            template-pwclient))

               ;; Collect the static assets, this includes JavaScript, CSS and
               ;; fonts. This is a standard Django process when running a
               ;; Django application for regular use, and includes assets for
               ;; dependencies like the admin site from Django.
               ;;
               ;; The intent here is that you can serve files from this
               ;; directory through a webserver, which is recommended when
               ;; running Django applications.
               (let ((static-root
                      (string-append out "/share/patchwork/htdocs")))
                 (mkdir-p static-root)
                 (copy-file "patchwork/settings/production.example.py"
                            "patchwork/settings/assets.py")
                 (setenv "DJANGO_SECRET_KEY" "dummyvalue")
                 (setenv "DJANGO_SETTINGS_MODULE" "patchwork.settings.assets")
                 (setenv "STATIC_ROOT" static-root)
                 (invoke "./manage.py" "collectstatic" "--no-input"))

               ;; The lib directory includes example configuration files that
               ;; may be useful when deploying patchwork.
               (copy-recursively "lib"
                                 (string-append
                                  out "/share/doc/" ,name "-" ,version)))
             #t))
         ;; The hasher script is used from the post-receive.hook
         (add-after 'install 'install-hasher
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-site-packages (site-packages inputs outputs))
                    (out-hasher.py (string-append out-site-packages
                                                  "/patchwork/hasher.py")))
               (chmod out-hasher.py #o555)
               (symlink out-hasher.py (string-append out "/bin/hasher")))
             #t))
         ;; Create a patchwork specific version of Django's command line admin
         ;; utility.
         (add-after 'install 'install-patchwork-admin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (call-with-output-file (string-append out "/bin/patchwork-admin")
                 (lambda (port)
                   (simple-format port "#!~A
import os, sys

if __name__ == \"__main__\":
    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)" (which "python"))))
               (chmod (string-append out "/bin/patchwork-admin") #o555))
             #t)))))
    (inputs
     `(("python-wrapper" ,python-wrapper)))
    (propagated-inputs
     `(("python-django" ,python-django)
       ;; TODO: Make this configurable
       ("python-psycopg2" ,python-psycopg2)
       ("python-mysqlclient" ,python-mysqlclient)
       ("python-django-filter" ,python-django-filter)
       ("python-djangorestframework" ,python-djangorestframework)
       ("python-django-debug-toolbar" ,python-django-debug-toolbar)))
    (synopsis "Web based patch tracking system")
    (description
     "Patchwork is a patch tracking system.  It takes in emails containing
patches, and displays the patches along with comments and state information.
Users can login allowing them to change the state of patches.")
    (home-page "http://jk.ozlabs.org/projects/patchwork/")
    (license gpl2+)))
