;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix inferior)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module ((guix utils)
                #:select (%current-system
                          source-properties->location
                          call-with-temporary-directory
                          version>? version-prefix?))
  #:use-module ((guix store)
                #:select (nix-server-socket
                          nix-server-major-version
                          nix-server-minor-version
                          store-lift))
  #:use-module ((guix derivations)
                #:select (read-derivation-from-file))
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 binary-ports)
  #:export (inferior?
            open-inferior
            close-inferior
            inferior-eval
            inferior-object?

            inferior-package?
            inferior-package-name
            inferior-package-version

            inferior-packages
            lookup-inferior-packages
            inferior-package-synopsis
            inferior-package-description
            inferior-package-home-page
            inferior-package-location
            inferior-package-derivation))

;;; Commentary:
;;;
;;; This module provides a way to spawn Guix "inferior" processes and to talk
;;; to them.  It allows us, from one instance of Guix, to interact with
;;; another instance of Guix coming from a different commit.
;;;
;;; Code:

;; Inferior Guix process.
(define-record-type <inferior>
  (inferior pid socket version packages table)
  inferior?
  (pid      inferior-pid)
  (socket   inferior-socket)
  (version  inferior-version)                    ;REPL protocol version
  (packages inferior-package-promise)            ;promise of inferior packages
  (table    inferior-package-table))             ;promise of vhash

(define (inferior-pipe directory command)
  "Return an input/output pipe on the Guix instance in DIRECTORY.  This runs
'DIRECTORY/COMMAND repl' if it exists, or falls back to some other method if
it's an old Guix."
  (let ((pipe (with-error-to-port (%make-void-port "w")
                (lambda ()
                  (open-pipe* OPEN_BOTH
                              (string-append directory "/" command)
                              "repl" "-t" "machine")))))
    (if (eof-object? (peek-char pipe))
        (begin
          (close-pipe pipe)

          ;; Older versions of Guix didn't have a 'guix repl' command, so
          ;; emulate it.
          (open-pipe* OPEN_BOTH "guile"
                      "-L" (string-append directory "/share/guile/site/"
                                          (effective-version))
                      "-C" (string-append directory "/share/guile/site/"
                                          (effective-version))
                      "-C" (string-append directory "/lib/guile/"
                                          (effective-version) "/site-ccache")
                      "-c"
                      (object->string
                       `(begin
                          (primitive-load ,(search-path %load-path
                                                        "guix/scripts/repl.scm"))
                          ((@ (guix scripts repl) machine-repl))))))
        pipe)))

(define* (open-inferior directory #:key (command "bin/guix"))
  "Open the inferior Guix in DIRECTORY, running 'DIRECTORY/COMMAND repl' or
equivalent.  Return #f if the inferior could not be launched."
  (define pipe
    (inferior-pipe directory command))

  (cond-expand
    ((and guile-2 (not guile-2.2)) #t)
    (else (setvbuf pipe 'line)))

  (match (read pipe)
    (('repl-version 0 rest ...)
     (letrec ((result (inferior 'pipe pipe (cons 0 rest)
                                (delay (%inferior-packages result))
                                (delay (%inferior-package-table result)))))
       (inferior-eval '(use-modules (guix)) result)
       (inferior-eval '(use-modules (gnu)) result)
       (inferior-eval '(define %package-table (make-hash-table))
                      result)
       result))
    (_
     #f)))

(define (close-inferior inferior)
  "Close INFERIOR."
  (close-pipe (inferior-socket inferior)))

;; Non-self-quoting object of the inferior.
(define-record-type <inferior-object>
  (inferior-object address appearance)
  inferior-object?
  (address     inferior-object-address)
  (appearance  inferior-object-appearance))

(define (write-inferior-object object port)
  (match object
    (($ <inferior-object> _ appearance)
     (format port "#<inferior-object ~a>" appearance))))

(set-record-type-printer! <inferior-object> write-inferior-object)

(define (read-inferior-response inferior)
  (define sexp->object
    (match-lambda
      (('value value)
       value)
      (('non-self-quoting address string)
       (inferior-object address string))))

  (match (read (inferior-socket inferior))
    (('values objects ...)
     (apply values (map sexp->object objects)))
    (('exception key objects ...)
     (apply throw key (map sexp->object objects)))))

(define (send-inferior-request exp inferior)
  (write exp (inferior-socket inferior))
  (newline (inferior-socket inferior)))

(define (inferior-eval exp inferior)
  "Evaluate EXP in INFERIOR."
  (send-inferior-request exp inferior)
  (read-inferior-response inferior))


;;;
;;; Inferior packages.
;;;

(define-record-type <inferior-package>
  (inferior-package inferior name version id)
  inferior-package?
  (inferior   inferior-package-inferior)
  (name       inferior-package-name)
  (version    inferior-package-version)
  (id         inferior-package-id))

(define (write-inferior-package package port)
  (match package
    (($ <inferior-package> _ name version)
     (format port "#<inferior-package ~a@~a ~a>"
             name version
             (number->string (object-address package) 16)))))

(set-record-type-printer! <inferior-package> write-inferior-package)

(define (%inferior-packages inferior)
  "Compute the list of inferior packages from INFERIOR."
  (let ((result (inferior-eval
                 '(fold-packages (lambda (package result)
                                   (let ((id (object-address package)))
                                     (hashv-set! %package-table id package)
                                     (cons (list (package-name package)
                                                 (package-version package)
                                                 id)
                                           result)))
                                 '())
                 inferior)))
    (map (match-lambda
           ((name version id)
            (inferior-package inferior name version id)))
         result)))

(define (inferior-packages inferior)
  "Return the list of packages known to INFERIOR."
  (force (inferior-package-promise inferior)))

(define (%inferior-package-table inferior)
  "Compute a package lookup table for INFERIOR."
  (fold (lambda (package table)
          (vhash-cons (inferior-package-name package) package
                      table))
        vlist-null
        (inferior-packages inferior)))

(define* (lookup-inferior-packages inferior name #:optional version)
  "Return the sorted list of inferior packages matching NAME in INFERIOR, with
highest version numbers first.  If VERSION is true, return only packages with
a version number prefixed by VERSION."
  ;; This is the counterpart of 'find-packages-by-name'.
  (sort (filter (lambda (package)
                  (or (not version)
                      (version-prefix? version
                                       (inferior-package-version package))))
                (vhash-fold* cons '() name
                             (force (inferior-package-table inferior))))
        (lambda (p1 p2)
          (version>? (inferior-package-version p1)
                     (inferior-package-version p2)))))

(define (inferior-package-field package getter)
  "Return the field of PACKAGE, an inferior package, accessed with GETTER."
  (let ((inferior (inferior-package-inferior package))
        (id       (inferior-package-id package)))
    (inferior-eval `(,getter (hashv-ref %package-table ,id))
                   inferior)))

(define* (inferior-package-synopsis package #:key (translate? #t))
  "Return the Texinfo synopsis of PACKAGE, an inferior package.  When
TRANSLATE? is true, translate it to the current locale's language."
  (inferior-package-field package
                          (if translate?
                              '(compose (@ (guix ui) P_) package-synopsis)
                              'package-synopsis)))

(define* (inferior-package-description package #:key (translate? #t))
  "Return the Texinfo description of PACKAGE, an inferior package.  When
TRANSLATE? is true, translate it to the current locale's language."
  (inferior-package-field package
                          (if translate?
                              '(compose (@ (guix ui) P_) package-description)
                              'package-description)))

(define (inferior-package-home-page package)
  "Return the home page of PACKAGE."
  (inferior-package-field package 'package-home-page))

(define (inferior-package-location package)
  "Return the source code location of PACKAGE, either #f or a <location>
record."
  (source-properties->location
   (inferior-package-field package
                           '(compose (lambda (loc)
                                       (and loc
                                            (location->source-properties
                                             loc)))
                                     package-location))))

(define (proxy client backend)                    ;adapted from (guix ssh)
  "Proxy communication between CLIENT and BACKEND until CLIENT closes the
connection, at which point CLIENT is closed (both CLIENT and BACKEND must be
input/output ports.)"
  (define (select* read write except)
    ;; This is a workaround for <https://bugs.gnu.org/30365> in Guile < 2.2.4:
    ;; since 'select' sometimes returns non-empty sets for no good reason,
    ;; call 'select' a second time with a zero timeout to filter out incorrect
    ;; replies.
    (match (select read write except)
      ((read write except)
       (select read write except 0))))

  ;; Use buffered ports so that 'get-bytevector-some' returns up to the
  ;; whole buffer like read(2) would--see <https://bugs.gnu.org/30066>.
  (setvbuf client _IOFBF 65536)
  (setvbuf backend _IOFBF 65536)

  (let loop ()
    (match (select* (list client backend) '() '())
      ((reads () ())
       (when (memq client reads)
         (match (get-bytevector-some client)
           ((? eof-object?)
            (close-port client))
           (bv
            (put-bytevector backend bv)
            (force-output backend))))
       (when (memq backend reads)
         (match (get-bytevector-some backend)
           (bv
            (put-bytevector client bv)
            (force-output client))))
       (unless (port-closed? client)
         (loop))))))

(define* (inferior-package-derivation store package
                                      #:optional
                                      (system (%current-system))
                                      #:key target)
  "Return the derivation for PACKAGE, an inferior package, built for SYSTEM
and cross-built for TARGET if TARGET is true.  The inferior corresponding to
PACKAGE must be live."
  ;; Create a named socket in /tmp and let the inferior of PACKAGE connect to
  ;; it and use it as its store.  This ensures the inferior uses the same
  ;; store, with the same options, the same per-session GC roots, etc.
  (call-with-temporary-directory
   (lambda (directory)
     (chmod directory #o700)
     (let* ((name     (string-append directory "/inferior"))
            (socket   (socket AF_UNIX SOCK_STREAM 0))
            (inferior (inferior-package-inferior package))
            (major    (nix-server-major-version store))
            (minor    (nix-server-minor-version store))
            (proto    (logior major minor)))
       (bind socket AF_UNIX name)
       (listen socket 1024)
       (send-inferior-request
        `(let ((socket (socket AF_UNIX SOCK_STREAM 0)))
           (connect socket AF_UNIX ,name)

           ;; 'port->connection' appeared in June 2018 and we can hardly
           ;; emulate it on older versions.  Thus fall back to
           ;; 'open-connection', at the risk of talking to the wrong daemon or
           ;; having our build result reclaimed (XXX).
           (let* ((store   (if (defined? 'port->connection)
                               (port->connection socket #:version ,proto)
                               (open-connection)))
                  (package (hashv-ref %package-table
                                      ,(inferior-package-id package)))
                  (drv     ,(if target
                                `(package-cross-derivation store package
                                                           ,target
                                                           ,system)
                                `(package-derivation store package
                                                     ,system))))
             (close-connection store)
             (close-port socket)
             (derivation-file-name drv)))
        inferior)
       (match (accept socket)
         ((client . address)
          (proxy client (nix-server-socket store))))
       (close-port socket)
       (read-derivation-from-file (read-inferior-response inferior))))))

(define inferior-package->derivation
  (store-lift inferior-package-derivation))

(define-gexp-compiler (package-compiler (package <inferior-package>) system
                                        target)
  ;; Compile PACKAGE for SYSTEM, optionally cross-building for TARGET.
  (inferior-package->derivation package system #:target target))
