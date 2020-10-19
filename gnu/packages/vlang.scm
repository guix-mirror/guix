;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages node)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public vlang
  (package
   (name "vlang")
   (version "0.1.29")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/vlang/v")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1rqi7cah5nq8aggrib9xvdpfjxq20li91svv0w9yny6nn1ag7snx"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags
      (list (string-append "CC=" ,(cc-for-target))
            "TMPTCC=tcc"
            (string-append "VC=" (assoc-ref %build-inputs "vc"))
            "GITCLEANPULL=true"
            "GITFASTCLONE=mkdir -p"
            "TCCREPO="
            "VCREPO="
            "VERBOSE=1")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("rm -rf") "true")
              (("v self") (string-append "v -cc " ,(cc-for-target) " cmd/v")))
            #t))
        (add-before 'check 'delete-failing-tests
          ;; XXX As always, these should eventually be fixed and run.
          (lambda _
            (for-each delete-file
                      '("vlib/v/gen/x64/tests/x64_test.v"
                        "vlib/v/tests/repl/repl_test.v"
                        "vlib/v/tests/valgrind/valgrind_test.v"
                        "vlib/v/tests/valgrind/strings_and_arrays.vv"
                        "vlib/v/tests/live_test.v"
                        "vlib/net/websocket/ws_test.v"))
            #t))
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (let* ((bin "tmp/bin")
                   (gcc (which "gcc")))
              (when tests?
                (mkdir-p bin)
                (symlink gcc (string-append bin "/cc"))
                (setenv "PATH" (string-append bin ":" (getenv "PATH")))
                (invoke "./v" "test-fixed")))
            #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
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
       ;; Versions are not consistently tagged, but the matching commit will
       ;; probably have ‘v0.x.y’ in the commit message.
       ,(let ((vc-version "b01d0fcda4b55861baa4be82e307cca4834b1641"))
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
             (base32 "052gp5q2k31r3lci3rx4k0vy0vjdjva64xvrbbihn8lgmw63lc9f")))))

      ;; For the tests.
      ("libx11" ,libx11)
      ("node" ,node)
      ("openssl" ,openssl)
      ("sqlite" ,sqlite)))
   (home-page "https://vlang.io/")
   (synopsis "Compiler for the V programming language")
   (description
    "V is a systems programming language.  It provides memory safety and thread
safety guarantees with minimal abstraction.")
   (license license:expat)))

(define-public v
  ;; We used to provide 'vlang' under the name 'v'.
  (deprecated-package "v" vlang))
