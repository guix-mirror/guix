;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
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

(define-module (gnu packages task-runners)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages mail)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go))

(define-public run
  (package
    (name "run")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TekWizely/run")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17n11lqhywq4z62w2rakdq80v7mxf83rgln19vj4v4nxpwd2hjjw"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-tekwizely-go-parsing))
    (arguments
     `(#:import-path "github.com/tekwizely/run"))
    (synopsis "Easily manage and invoke small scripts and wrappers")
    (description
     "Run is a tool to easily manage and invoke small scripts and wrappers by
using a Runfile.")
    (home-page "https://github.com/TekWizely/run")
    (license license:expat)))

(define-public task-spooler
  (package
    (name "task-spooler")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://vicerveza.homeunix.net/~viric/soft/ts/ts-" version ".tar.gz"))
        (sha256 (base32 "0y32sm2i2jxs88c307h76449fynk75p9qfw1k11l5ixrn03z67pl"))))
    (build-system gnu-build-system)
    (arguments
      `(#:make-flags
        (let ((c-flags "-g -O2"))
          (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                ,(string-append "CC=" (cc-for-target))
                (string-append "CFLAGS=" c-flags)))
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ;; no configuration script
          (add-after 'unpack 'rename-and-patch-paths
            (lambda _
              ;; Rename "ts" to "tsp" to not interfere with "ts" command
              ;; from moreutils package.
              (rename-file "ts.1" "tsp.1");
              (substitute* '("Makefile" "testbench.sh")
                (("\\bts\\b") "tsp"))
              ;; Patch gzip/sendmail/shell paths.
              (substitute* "execute.c"
                (("execlp\\(\"gzip\"")
                 (format #f "execlp(\"~a/bin/gzip\""
                         (assoc-ref %build-inputs "gzip"))))
              (substitute* "list.c"
                (("/bin/sh\\b") (which "sh")))
              (substitute* "env.c"
                (("execlp\\(\"/bin/sh\"")
                 (format #f "execlp(\"~a/bin/sh\""
                         (assoc-ref %build-inputs "bash"))))
              (substitute* "mail.c"
                (("execl\\(\"/usr/sbin/sendmail\"")
                 (format #f "execl(\"~a/sbin/sendmail\""
                         (assoc-ref %build-inputs "sendmail"))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "PATH" (string-join (list (getenv "PATH") (getcwd)) ":"))
                (invoke "./testbench.sh")))))))
    (inputs
      `(("bash" ,bash-minimal)
        ("gzip" ,gzip)
        ("sendmail" ,sendmail)))
    (synopsis "UNIX task queue system")
    (description "Task spooler lets users run shell commands asynchronously
one after the other in a separate process.")
    (home-page "https://vicerveza.homeunix.net/~viric/soft/ts/")
    (license license:gpl2+)))
