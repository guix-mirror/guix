;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages bash)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix build-system gnu)
  #:autoload   (guix gnupg) (gnupg-verify*)
  #:autoload   (gcrypt hash) (port-sha256)
  #:autoload   (guix base32) (bytevector->nix-base32-string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format))

(define (patch-url seqno)
  "Return the URL of Bash patch number SEQNO."
  (format #f "mirror://gnu/bash/bash-5.0-patches/bash50-~3,'0d" seqno))

(define (bash-patch seqno sha256)
  "Return the origin of Bash patch SEQNO, with expected hash SHA256"
  (origin
    (method url-fetch)
    (uri (patch-url seqno))
    (sha256 sha256)))

(define-syntax-rule (patch-series (seqno hash) ...)
  (list (bash-patch seqno (base32 hash))
        ...))

(define %patch-series-5.0
  ;; This is the current patches series for 5.0, generated using
  ;; 'download-patches' below.
  (patch-series
    (1 "12bjfdy6bg8nhyw27bdgxn7h4paylx8d927skfmi9pxd1wgrxzpj")
    (2 "01w7yrzmz10mw06ys0546vhl7isv2v402ziyvfd7k67588spvs47")))

(define (download-patches store count)
  "Download COUNT Bash patches into store.  Return a list of
number/base32-hash tuples, directly usable in the 'patch-series' form."
  (unfold (cut > <> count)
          (lambda (number)
            (let* ((patch  (download-to-store store (patch-url number)))
                   (sig    (download-to-store store
                                              (string-append (patch-url number)
                                                             ".sig"))))
              (unless (gnupg-verify* sig patch)
                (error "failed to verify signature" patch))

              (list number
                    (bytevector->nix-base32-string
                     (call-with-input-file patch port-sha256)))))
          1+
          1))

(define-public bash
  (let* ((cppflags (string-join '("-DDEFAULT_PATH_VALUE='\"/no-such-path\"'"
                                  "-DSTANDARD_UTILS_PATH='\"/no-such-path\"'"
                                  "-DNON_INTERACTIVE_LOGIN_SHELLS"
                                  "-DSSH_SOURCE_BASHRC")
                                " "))
         (configure-flags
          ``("--with-installed-readline"
             ,,(string-append "CPPFLAGS=" cppflags)
             ,(string-append
               "LDFLAGS=-Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "readline")
               "/lib"
               " -Wl,-rpath -Wl,"
               (assoc-ref %build-inputs "ncurses")
               "/lib")))
         (version "5.0"))
    (package
     (name "bash")
     (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/bash/bash-" version ".tar.gz"))
              (sha256
               (base32
                "0kgvfwqdcd90waczf4gx39xnrxzijhjrzyzv7s8v4w31qqm0za5l"))
              (patch-flags '("-p0"))
              (patches %patch-series-5.0)))
     (version (string-append version "." (number->string (length %patch-series-5.0))))
     (build-system gnu-build-system)

     (outputs '("out"
                "doc"                         ;1.7 MiB of HTML and extra files
                "include"))                   ;headers used by extensions
     (inputs `(("readline" ,readline)
               ("ncurses" ,ncurses)))             ;TODO: add texinfo
     (arguments
      `(;; When cross-compiling, `configure' incorrectly guesses that job
        ;; control is missing.
        #:configure-flags ,(if (%current-target-system)
                               `(cons* "bash_cv_job_control_missing=no"
                                       ,configure-flags)
                               configure-flags)

        ;; Bash is reportedly not parallel-safe.  See, for instance,
        ;; <http://patches.openembedded.org/patch/32745/> and
        ;; <http://git.buildroot.net/buildroot/commit/?h=79e2d802a>.
        #:parallel-build? #f
        #:parallel-tests? #f

        ;; XXX: The tests have a lot of hard-coded paths, so disable them
        ;; for now.
        #:tests? #f

        #:modules ((srfi srfi-26)
                   (guix build utils)
                   (guix build gnu-build-system))

        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'install-sh-symlink
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Add a `sh' -> `bash' link.
              (let ((out (assoc-ref outputs "out")))
                (with-directory-excursion (string-append out "/bin")
                  (symlink "bash" "sh")
                  #t))))

          (add-after 'install 'move-development-files
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Move 'Makefile.inc' and 'bash.pc' to "include" to avoid
              ;; circular references among the outputs.
              (let ((out     (assoc-ref outputs "out"))
                    (include (assoc-ref outputs "include"))
                    (lib     (cut string-append <> "/lib/bash")))
                (mkdir-p (lib include))
                (rename-file (string-append (lib out)
                                            "/Makefile.inc")
                             (string-append (lib include)
                                            "/Makefile.inc"))
                (rename-file (string-append out "/lib/pkgconfig")
                             (string-append include
                                            "/lib/pkgconfig"))

                ;; Don't capture the absolute file name of 'install' to avoid
                ;; retaining a dependency on Coreutils.
                (substitute* (string-append (lib include)
                                            "/Makefile.inc")
                  (("^INSTALL =.*")
                   "INSTALL = install -c\n"))
                #t))))))

     (native-search-paths
      (list (search-path-specification            ;new in 4.4
             (variable "BASH_LOADABLES_PATH")
             (files '("lib/bash")))))

     (synopsis "The GNU Bourne-Again SHell")
     (description
      "Bash is the shell, or command-line interpreter, of the GNU system.  It
is compatible with the Bourne Shell, but it also integrates useful features
from the Korn Shell and the C Shell and new improvements of its own.  It
allows command-line editing, unlimited command history, shell functions and
aliases, and job control while still allowing most sh scripts to be run
without modification.")
     (license gpl3+)
     (home-page "https://www.gnu.org/software/bash/"))))

(define-public bash-minimal
  ;; A stripped-down Bash for non-interactive use.
  (package (inherit bash)
    (name "bash-minimal")
    (inputs '())                                ; no readline, no curses

    ;; No "include" output because there's no support for loadable modules.
    (outputs (delete "include" (package-outputs bash)))

    (arguments
     (substitute-keyword-arguments (package-arguments bash)
       ((#:modules _ '())
        '((guix build gnu-build-system)
          (guix build utils)
          (srfi srfi-1)
          (srfi srfi-26)))
       ((#:configure-flags flags '())
        `(list "--without-bash-malloc"
               "--disable-readline"
               "--disable-history"
               "--disable-help-builtin"
               "--disable-progcomp"
               "--disable-net-redirections"
               "--disable-nls"

               ;; Pretend 'dlopen' is missing so we don't build loadable
               ;; modules and related code.
               "ac_cv_func_dlopen=no"

               ,@(if (%current-target-system)
                     '("bash_cv_job_control_missing=no"
                       "bash_cv_getcwd_malloc=yes")
                     '())))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; No loadable modules.
           (delete 'move-development-files)))))))

(define-public static-bash
  ;; Statically-linked Bash that contains nothing but the 'bash' binary and
  ;; 'sh' symlink, without any reference.
  (let ((bash (static-package bash-minimal)))
    (package
      (inherit bash)
      (name "bash-static")
      (arguments
       (substitute-keyword-arguments
           `(#:allowed-references ("out") ,@(package-arguments bash))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'strip 'remove-everything-but-the-binary
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (bin (string-append out "/bin")))
                   (remove-store-references (string-append bin "/bash"))
                   (delete-file (string-append bin "/bashbug"))
                   (delete-file-recursively (string-append out "/share"))
                   #t))))))))))

(define-public bash-completion
  (package
    (name "bash-completion")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/scop/" name "/releases/download/"
                    version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0kgmflrr1ga9wfk770vmakna3nj46ylb5ky9ipd0v2k9ymq5a7y0"))
              (patches
               (search-patches "bash-completion-directories.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("util-linux" ,util-linux)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'install 'remove-redundant-completions
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Util-linux comes with a bunch of completion files for
                     ;; its own commands which are more sophisticated and
                     ;; up-to-date than those of bash-completion.  Remove those
                     ;; from bash-completion.
                     (let* ((out         (assoc-ref outputs "out"))
                            (util-linux  (assoc-ref inputs "util-linux"))
                            (completions (string-append out
                                                        "/share/bash-completion"
                                                        "/completions"))
                            (already     (find-files
                                          (string-append
                                           util-linux
                                           "/etc/bash_completion.d"))))
                       (with-directory-excursion completions
                         (for-each (lambda (file)
                                     (when (file-exists? file)
                                       (delete-file file)))
                                   (map basename already)))
                       #t))))))
    (synopsis "Bash completions for common commands")
    (description
     "This package provides extensions that allow Bash to provide adapted
completion for many common commands.")
    (home-page "https://github.com/scop/bash-completion")
    (license gpl2+)))

(define-public bash-tap
  (package
    (name "bash-tap")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/illusori/bash-tap/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qs1qi38bl3ns4mpagcawv618dsk2q1lgrbddgvs0wl3ia12cyz5"))))
    ;; There is no compilation process to use this package, however, the bash
    ;; scripts installed by this package start with "#!/bin/bash".  To fix
    ;; these lines, we use the patch-shebangs of the GNU build system.  The
    ;; project does not use a Makefile.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no test suite.
       #:phases
       (modify-phases %standard-phases
         ;; Because there are no configure scripts or Makefile, we can
         ;; remove these phases.
         (delete 'configure)
         (delete 'build)
         ;; The installation involves manually copying the files to a location.
         ;; To make them easily accessible by setting PATH, we add the scripts
         ;; to the "bin" folder.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "bash-tap" bin)
               (install-file "bash-tap-bootstrap" bin)
               (install-file "bash-tap-mock" bin)))))))
    (home-page "http://www.illusori.co.uk/projects/bash-tap/")
    (synopsis "Bash port of a Test::More/Test::Builder-style TAP-compliant
test library")
    (description "Bash TAP is a TAP-compliant Test::More-style testing library
for Bash shell scripts and functions.  Along with the Test::More-style testing
helpers it provides helper functions for mocking commands and in-process output
capturing.")
    (license expat)))
