;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
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

(define-module (gnu packages vlang)
  #:use-module (gnu packages glib)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public v
  (package
   (name "v")
   (version "0.1.27")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/vlang/v")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1d9qhacllvkqif42jaayixhjyhx7pzslh8p1yr5p19447q763fq1"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f ; tests are broken in v 0.1.27
      #:make-flags
      `("CC=gcc"
        "GITCLEANPULL=true"
        "GITFASTCLONE=mkdir -p"
        "TCCREPO="
        "TMPTCC=tcc"
        ,(string-append "TMPVC=" (assoc-ref %build-inputs "vc"))
        "VCREPO="
        "VERBOSE=1"
        "V_ALWAYS_CLEAN_TMP=false")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("rm -rf") "true")
              (("v self") "v -cc gcc cmd/v"))
            #t))
        ;; A few tests are broken in v 0.1.27. This function should be
        ;; enabled to run tests in the next release.
        ;; (replace 'check
        ;;   (lambda _
        ;;     (let* ((tmpbin "tmp/bin")
        ;;            (gcc (which "gcc")))
        ;;       (mkdir-p tmpbin)
        ;;       (symlink gcc (string-append tmpbin "/cc"))
        ;;       (setenv "PATH" (string-append tmpbin ":" (getenv "PATH")))
        ;;       (invoke "./v" "test-fixed"))
        ;;     #t))
        (replace 'install
          (lambda _
            (let* ((bin (string-append (assoc-ref %outputs "out") "/bin"))
                   (tools (string-append bin "/cmd/tools"))
                   (thirdparty (string-append bin "/thirdparty"))
                   (vlib (string-append bin "/vlib"))
                   (vmod (string-append bin "/v.mod")))
              (mkdir-p bin)
              (copy-file "./v" (string-append bin "/v"))
              ;; v requires as of 0.1.27 that these other components are in the
              ;; same directory. In a future release we may be able to move
              ;; these into other output folders.
              (copy-recursively "cmd/tools" tools)
              (copy-recursively "thirdparty" thirdparty)
              (copy-recursively "vlib" vlib)
              (copy-file "v.mod" vmod))
            #t)))))
   (inputs
    `(("glib" ,glib)))
   (native-inputs
    `(("vc"
       ,(let ((vc-version "0884d7092f4c2a4f8ca16da6f1792efa235247be"))
          ;; v bootstraps from generated c source code from a dedicated
          ;; repository. It's readable, as generated source goes, and not at all
          ;; obfuscated, and it's about 15kb. The original source written in
          ;; golang is lost to the forces of entropy; modifying the generated c
          ;; source by hand has been a commonly used technique for iterating on
          ;; the codebase.
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/vlang/vc")
                  (commit vc-version)))
            (file-name (git-file-name "vc" vc-version))
            (sha256
             (base32 "17bs09iwxfd0si70j48n9nd16gfgcj8imd0azypk3xzzbz4wybnz")))))))
   (home-page "https://vlang.io/")
   (synopsis "Compiler for the V programming language")
   (description
    "V is a systems programming language.  It provides memory safety and thread
safety guarantees with minimal abstraction.")
   (license license:expat)))
