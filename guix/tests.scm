;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix tests)
  #:use-module ((guix config) #:select (%storedir %localstatedir))
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (guix monads)
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((guix build utils) #:select (mkdir-p compressor))
  #:use-module ((gcrypt hash) #:hide (sha256))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (web uri)
  #:export (open-connection-for-tests
            with-external-store
            %seed
            random-text
            random-bytevector
            file=?
            canonical-file?
            network-reachable?
            shebang-too-long?
            with-environment-variable

            search-bootstrap-binary

            mock
            %test-substitute-urls
            test-assertm
            test-equalm
            %substitute-directory
            with-derivation-narinfo
            with-derivation-substitute
            dummy-package
            dummy-origin

            gnu-make-for-tests

            test-file))

;;; Commentary:
;;;
;;; This module provide shared infrastructure for the test suite.  For
;;; internal use only.
;;;
;;; Code:

(define %test-substitute-urls
  ;; URLs where to look for substitutes during tests.
  (make-parameter
   (or (and=> (getenv "GUIX_BINARY_SUBSTITUTE_URL") list)
       '())))

(define* (open-connection-for-tests #:optional (uri (%daemon-socket-uri)))
  "Open a connection to the build daemon for tests purposes and return it."
  (guard (c ((store-error? c)
             (format (current-error-port)
                     "warning: build daemon error: ~s~%" c)
             #f))
    (let ((store (open-connection uri)))
      ;; Make sure we build everything by ourselves.
      (set-build-options store
                         #:use-substitutes? #f
                         #:substitute-urls (%test-substitute-urls))

      ;; Use the bootstrap Guile when running tests, so we don't end up
      ;; building everything in the temporary test store.
      (%guile-for-build (package-derivation store %bootstrap-guile))

      store)))

(define (bootstrap-binary-file program system)
  "Return the absolute file name where bootstrap binary PROGRAM for SYSTEM is
stored."
  (string-append (dirname (search-path %load-path
                                       "gnu/packages/bootstrap.scm"))
                 "/bootstrap/" system "/" program))

(define (search-bootstrap-binary file-name system)
  "Search the bootstrap binary FILE-NAME for SYSTEM.  Raise an error if not
found."
  ;; Note: Keep bootstrap binaries on the local file system so that the 'guix'
  ;; package can provide them as inputs and copy them to the right place.
  (let* ((system (match system
                   ("x86_64-linux" "i686-linux")
                   (_ system)))
         (file   (bootstrap-binary-file file-name system)))
    (if (file-exists? file)
        file
        (with-store store
          (run-with-store store
            (mlet %store-monad ((drv (origin->derivation
                                      (bootstrap-executable file-name system))))
              (mbegin %store-monad
                (built-derivations (list drv))
                (begin
                  (mkdir-p (dirname file))
                  (copy-file (derivation->output-path drv) file)
                  (return file)))))))))

(define (call-with-external-store proc)
  "Call PROC with an open connection to the external store or #f it there is
no external store to talk to."
  (parameterize ((%daemon-socket-uri
                  (string-append %localstatedir
                                 "/guix/daemon-socket/socket"))
                 (%store-prefix %storedir))
    (define store
      (catch #t
        (lambda ()
          (open-connection))
        (const #f)))

    (let ((store-variable (getenv "NIX_STORE_DIR")))
      (dynamic-wind
        (lambda ()
          ;; This environment variable is set by 'pre-inst-env' but it
          ;; influences '%store-directory' in (guix build utils), which is
          ;; itself used in (guix packages).  Thus, unset it before going any
          ;; further.
          (unsetenv "NIX_STORE_DIR"))
        (lambda ()
          (proc store))
        (lambda ()
          (when store-variable
            (setenv "NIX_STORE_DIR" store-variable))
          (when store
            (close-connection store)))))))

(define-syntax-rule (with-external-store store exp ...)
  "Evaluate EXP with STORE bound to the external store rather than the
temporary test store, or #f if there is no external store to talk to.

This is meant to be used for tests that need to build packages that would be
too expensive to build entirely in the test store."
  (call-with-external-store (lambda (store) exp ...)))

(define (random-seed)
  (or (and=> (getenv "GUIX_TESTS_RANDOM_SEED")
             number->string)
      (logxor (getpid) (car (gettimeofday)))))

(define (%seed)
  (let ((seed (random-seed)))
    (format (current-error-port) "random seed for tests: ~a~%"
            seed)
    (let ((result (seed->random-state seed)))
      (set! %seed (lambda () result))
      result)))

(define (random-text)
  "Return the hexadecimal representation of a random number."
  (number->string (random (expt 2 256) (%seed)) 16))

(define (random-bytevector n)
  "Return a random bytevector of N bytes."
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256 (%seed)))
            (loop (1+ i)))
          bv))))

(define* (file=? a b #:optional (stat lstat))
  "Return true if files A and B have the same type and same content.  Call
STAT to obtain file metadata."
  (let ((sta (stat a)) (stb (stat b)))
    (and (eq? (stat:type sta) (stat:type stb))
         (case (stat:type sta)
           ((regular)
            (or (and (= (stat:ino sta) (stat:ino stb))
                     (= (stat:dev sta) (stat:dev stb)))
                (equal?
                 (call-with-input-file a get-bytevector-all)
                 (call-with-input-file b get-bytevector-all))))
           ((symlink)
            (string=? (readlink a) (readlink b)))
           (else
            (error "what?" (stat a)))))))

(define (canonical-file? file)
  "Return #t if FILE is in the store, is read-only, and its mtime is 1."
  (let ((st (lstat file)))
    (or (not (string-prefix? (%store-prefix) file))
        (eq? 'symlink (stat:type st))
        (and (= 1 (stat:mtime st))
             (zero? (logand #o222 (stat:mode st)))))))

(define (network-reachable?)
  "Return true if we can reach the Internet."
  (false-if-exception (getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)))

(define-syntax-rule (mock (module proc replacement) body ...)
  "Within BODY, replace the definition of PROC from MODULE with the definition
given by REPLACEMENT."
  (let* ((m (resolve-module 'module))
         (original (module-ref m 'proc)))
    (dynamic-wind
      (lambda () (module-set! m 'proc replacement))
      (lambda () body ...)
      (lambda () (module-set! m 'proc original)))))

(define-syntax-rule (test-assertm name exp)
  "Like 'test-assert', but EXP is a monadic value.  A new connection to the
store is opened."
  (test-assert name
    (let ((store (open-connection-for-tests)))
      (dynamic-wind
        (const #t)
        (lambda ()
          (run-with-store store exp
                          #:guile-for-build (%guile-for-build)))
        (lambda ()
          (close-connection store))))))

(define-syntax-rule (test-equalm name value exp)
  "Like 'test-equal', but EXP is a monadic value.  A new connection to the
store is opened."
  (test-equal name
    value
    (with-store store
      (run-with-store store exp
                      #:guile-for-build (%guile-for-build)))))

(define-syntax-rule (with-environment-variable variable value body ...)
  "Run BODY with VARIABLE set to VALUE."
  (let ((orig (getenv variable)))
    (dynamic-wind
      (lambda ()
        (setenv variable value))
      (lambda ()
        body ...)
      (lambda ()
        (if orig
            (setenv variable orig)
            (unsetenv variable))))))


;;;
;;; Narinfo files, as used by the substituter.
;;;

(define* (derivation-narinfo drv #:key (nar "example.nar")
                             (sha256 (make-bytevector 32 0))
                             (references '()))
  "Return the contents of the narinfo corresponding to DRV, with the specified
REFERENCES (a list of store items); NAR should be the file name of the archive
containing the substitute for DRV, and SHA256 is the expected hash."
  (format #f "StorePath: ~a
URL: ~a
Compression: none
NarSize: 1234
NarHash: sha256:~a
References: ~a
System: ~a
Deriver: ~a~%"
          (derivation->output-path drv)       ; StorePath
          nar                                 ; URL
          (bytevector->nix-base32-string sha256)  ; NarHash
          (string-join (map basename references)) ; References
          (derivation-system drv)             ; System
          (basename
           (derivation-file-name drv))))      ; Deriver

(define %substitute-directory
  (make-parameter
   (and=> (getenv "GUIX_BINARY_SUBSTITUTE_URL")
          (compose uri-path string->uri))))

(define* (call-with-derivation-narinfo drv thunk
                                       #:key
                                       (sha256 (make-bytevector 32 0))
                                       (references '()))
  "Call THUNK in a context where fake substituter data, as read by 'guix
substitute', has been installed for DRV.  SHA256 is the hash of the
expected output of DRV."
  (let* ((output  (derivation->output-path drv))
         (dir     (%substitute-directory))
         (info    (string-append dir "/nix-cache-info"))
         (narinfo (string-append dir "/" (store-path-hash-part output)
                                 ".narinfo")))
    (dynamic-wind
      (lambda ()
        (call-with-output-file info
          (lambda (p)
            (format p "StoreDir: ~a\nWantMassQuery: 0\n"
                    (%store-prefix))))
        (call-with-output-file narinfo
          (lambda (p)
            (display (derivation-narinfo drv #:sha256 sha256
                                         #:references references)
                     p))))
      thunk
      (lambda ()
        (delete-file narinfo)
        (delete-file info)))))

(define-syntax with-derivation-narinfo
  (syntax-rules (sha256 references =>)
    "Evaluate BODY in a context where DRV looks substitutable from the
substituter's viewpoint."
    ((_ drv (sha256 => hash) (references => refs) body ...)
     (call-with-derivation-narinfo drv
       (lambda () body ...)
       #:sha256 hash
       #:references refs))
    ((_ drv (sha256 => hash) body ...)
     (with-derivation-narinfo drv
       (sha256 => hash) (references => '())
       body ...))
    ((_ drv body ...)
     (call-with-derivation-narinfo drv
       (lambda ()
         body ...)))))

(define* (call-with-derivation-substitute drv contents thunk
                                          #:key
                                          sha256
                                          (references '()))
  "Call THUNK in a context where a substitute for DRV has been installed,
using CONTENTS, a string, as its contents.  If SHA256 is true, use it as the
expected hash of the substitute; otherwise use the hash of the nar containing
CONTENTS."
  (define dir (%substitute-directory))
  (dynamic-wind
    (lambda ()
      (call-with-output-file (string-append dir "/example.out")
        (lambda (port)
          (display contents port)))
      (call-with-output-file (string-append dir "/example.nar")
        (lambda (p)
          (write-file (string-append dir "/example.out") p))))
    (lambda ()
      (let ((hash (call-with-input-file (string-append dir "/example.nar")
                    port-sha256)))
        ;; Create fake substituter data, to be read by 'guix substitute'.
        (call-with-derivation-narinfo drv
          thunk
          #:sha256 (or sha256 hash)
          #:references references)))
    (lambda ()
      (delete-file (string-append dir "/example.out"))
      (delete-file (string-append dir "/example.nar")))))

(define (shebang-too-long?)
  "Return true if the typical shebang in the current store would exceed
Linux's static limit---the BINPRM_BUF_SIZE constant, normally 128 characters
all included."
  (define shebang
    (string-append "#!" (%store-prefix) "/"
                   (make-string 32 #\a)
                   "-bootstrap-binaries-0/bin/bash\0"))

  (> (string-length shebang) 128))

(define-syntax with-derivation-substitute
  (syntax-rules (sha256 references =>)
    "Evaluate BODY in a context where DRV is substitutable with the given
CONTENTS."
    ((_ drv contents (sha256 => hash) (references => refs) body ...)
     (call-with-derivation-substitute drv contents
       (lambda () body ...)
       #:sha256 hash
       #:references refs))
    ((_ drv contents (sha256 => hash) body ...)
     (with-derivation-substitute drv contents
       (sha256 => hash) (references => '())
       body ...))
    ((_ drv contents body ...)
     (call-with-derivation-substitute drv contents
       (lambda ()
         body ...)))))

(define-syntax-rule (dummy-package name* extra-fields ...)
  "Return a \"dummy\" package called NAME*, with all its compulsory fields
initialized with default values, and with EXTRA-FIELDS set as specified."
  (let ((p (package
             (name name*) (version "0") (source #f)
             (build-system gnu-build-system)
             (synopsis #f) (description #f)
             (home-page #f) (license #f))))
    (package (inherit p) extra-fields ...)))

(define-syntax-rule (dummy-origin extra-fields ...)
  "Return a \"dummy\" origin, with all its compulsory fields initialized with
default values, and with EXTRA-FIELDS set as specified."
  (let ((o (origin (method #f) (uri "http://www.example.com")
                   (sha256 (base32 (make-string 52 #\x))))))
    (origin (inherit o) extra-fields ...)))

(define gnu-make-for-tests
  ;; This is a variant of 'gnu-make-boot0' that can be built with minimal
  ;; resources.
  (package-with-bootstrap-guile
   (package
     (inherit gnu-make)
     (name "make-test-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:tests? #f                               ;cannot run "make check"
        ,@(substitute-keyword-arguments (package-arguments gnu-make)
            ((#:configure-flags flags ''())
             ;; As in 'gnu-make-boot0', work around a 'config.status' defect.
             `(cons "--disable-dependency-tracking" ,flags))
            ((#:phases phases)
             `(modify-phases ,phases
                (replace 'build
                  (lambda _
                    (invoke "./build.sh")
                    #t))
                (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (install-file "make" bin)
                      #t))))))))
     (native-inputs '())                          ;no need for 'pkg-config'
     (inputs %bootstrap-inputs-for-tests))))


;;;
;;; Test utility procedures.

(define (test-file store name content)
  "Create a simple file in STORE with CONTENT (a string), compressed according
to its file name extension.  Return both its file name and its hash."
  (let* ((ext (string-index-right name #\.))
         (name-sans-ext (if ext
                            (string-take name (string-index-right name #\.))
                            name))
         (comp (compressor name))
         (command #~(if #+comp
                        (string-append #+%bootstrap-coreutils&co
                                       "/bin/" #+comp)
                        #f))
         (f (with-imported-modules '((guix build utils))
              (computed-file name
                             #~(begin
                                 (use-modules (guix build utils)
                                              (rnrs io simple))
                                 (with-output-to-file #+name-sans-ext
                                   (lambda _
                                     (format #t #+content)))
                                 (when #+command
                                   (invoke #+command #+name-sans-ext))
                                 (copy-file #+name #$output)))))
         (file-drv (run-with-store store (lower-object f)))
         (file (derivation->output-path file-drv))
         (file-drv-outputs (derivation-outputs file-drv))
         (_ (build-derivations store (list file-drv)))
         (file-hash (derivation-output-hash
                     (assoc-ref file-drv-outputs "out"))))
    (values file file-hash)))

;;;
;; Local Variables:
;; eval: (put 'call-with-derivation-narinfo 'scheme-indent-function 1)
;; eval: (put 'call-with-derivation-substitute 'scheme-indent-function 2)
;; End:

;;; tests.scm ends here
