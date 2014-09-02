;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
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

(define-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (manifest make-manifest
            manifest?
            manifest-entries

            <manifest-entry>              ; FIXME: eventually make it internal
            manifest-entry
            manifest-entry?
            manifest-entry-name
            manifest-entry-version
            manifest-entry-output
            manifest-entry-item
            manifest-entry-dependencies

            manifest-pattern
            manifest-pattern?

            manifest-remove
            manifest-add
            manifest-lookup
            manifest-installed?
            manifest-matching-entries

            manifest-transaction
            manifest-transaction?
            manifest-transaction-install
            manifest-transaction-remove
            manifest-perform-transaction
            manifest-transaction-effects
            manifest-show-transaction

            profile-manifest
            package->manifest-entry
            profile-derivation
            generation-number
            generation-numbers
            previous-generation-number
            generation-time
            generation-file-name))

;;; Commentary:
;;;
;;; Tools to create and manipulate profiles---i.e., the representation of a
;;; set of installed packages.
;;;
;;; Code:


;;;
;;; Manifests.
;;;

(define-record-type <manifest>
  (manifest entries)
  manifest?
  (entries manifest-entries))                     ; list of <manifest-entry>

;; Convenient alias, to avoid name clashes.
(define make-manifest manifest)

(define-record-type* <manifest-entry> manifest-entry
  make-manifest-entry
  manifest-entry?
  (name         manifest-entry-name)              ; string
  (version      manifest-entry-version)           ; string
  (output       manifest-entry-output             ; string
                (default "out"))
  (item         manifest-entry-item)              ; package | store path
  (dependencies manifest-entry-dependencies       ; (store path | package)*
                (default '())))

(define-record-type* <manifest-pattern> manifest-pattern
  make-manifest-pattern
  manifest-pattern?
  (name         manifest-pattern-name)            ; string
  (version      manifest-pattern-version          ; string | #f
                (default #f))
  (output       manifest-pattern-output           ; string | #f
                (default "out")))

(define (profile-manifest profile)
  "Return the PROFILE's manifest."
  (let ((file (string-append profile "/manifest")))
    (if (file-exists? file)
        (call-with-input-file file read-manifest)
        (manifest '()))))

(define* (package->manifest-entry package #:optional output)
  "Return a manifest entry for the OUTPUT of package PACKAGE.  When OUTPUT is
omitted or #f, use the first output of PACKAGE."
  (let ((deps (map (match-lambda
                    ((label package)
                     `(,package "out"))
                    ((label package output)
                     `(,package ,output)))
                   (package-transitive-propagated-inputs package))))
    (manifest-entry
     (name (package-name package))
     (version (package-version package))
     (output (or output (car (package-outputs package))))
     (item package)
     (dependencies (delete-duplicates deps)))))

(define (manifest->gexp manifest)
  "Return a representation of MANIFEST as a gexp."
  (define (entry->gexp entry)
    (match entry
      (($ <manifest-entry> name version output (? string? path) (deps ...))
       #~(#$name #$version #$output #$path #$deps))
      (($ <manifest-entry> name version output (? package? package) (deps ...))
       #~(#$name #$version #$output
                 (ungexp package (or output "out")) #$deps))))

  (match manifest
    (($ <manifest> (entries ...))
     #~(manifest (version 1)
                 (packages #$(map entry->gexp entries))))))

(define (sexp->manifest sexp)
  "Parse SEXP as a manifest."
  (match sexp
    (('manifest ('version 0)
                ('packages ((name version output path) ...)))
     (manifest
      (map (lambda (name version output path)
             (manifest-entry
              (name name)
              (version version)
              (output output)
              (item path)))
           name version output path)))

    ;; Version 1 adds a list of propagated inputs to the
    ;; name/version/output/path tuples.
    (('manifest ('version 1)
                ('packages ((name version output path deps) ...)))
     (manifest
      (map (lambda (name version output path deps)
             ;; Up to Guix 0.7 included, dependencies were listed as ("gmp"
             ;; "/gnu/store/...-gmp") for instance.  Discard the 'label' in
             ;; such lists.
             (let ((deps (match deps
                           (((labels directories) ...)
                            directories)
                           ((directories ...)
                            directories))))
               (manifest-entry
                 (name name)
                 (version version)
                 (output output)
                 (item path)
                 (dependencies deps))))
           name version output path deps)))

    (_
     (error "unsupported manifest format" manifest))))

(define (read-manifest port)
  "Return the packages listed in MANIFEST."
  (sexp->manifest (read port)))

(define (entry-predicate pattern)
  "Return a procedure that returns #t when passed a manifest entry that
matches NAME/OUTPUT/VERSION.  OUTPUT and VERSION may be #f, in which case they
are ignored."
  (match pattern
    (($ <manifest-pattern> name version output)
     (match-lambda
      (($ <manifest-entry> entry-name entry-version entry-output)
       (and (string=? entry-name name)
            (or (not entry-output) (not output)
                (string=? entry-output output))
            (or (not version)
                (string=? entry-version version))))))))

(define (manifest-remove manifest patterns)
  "Remove entries for each of PATTERNS from MANIFEST.  Each item in PATTERNS
must be a manifest-pattern."
  (define (remove-entry pattern lst)
    (remove (entry-predicate pattern) lst))

  (make-manifest (fold remove-entry
                       (manifest-entries manifest)
                       patterns)))

(define (manifest-add manifest entries)
  "Add a list of manifest ENTRIES to MANIFEST and return new manifest.
Remove MANIFEST entries that have the same name and output as ENTRIES."
  (define (same-entry? entry name output)
    (match entry
      (($ <manifest-entry> entry-name _ entry-output _ ...)
       (and (equal? name entry-name)
            (equal? output entry-output)))))

  (make-manifest
   (append entries
           (fold (lambda (entry result)
                   (match entry
                     (($ <manifest-entry> name _ out _ ...)
                      (filter (negate (cut same-entry? <> name out))
                              result))))
                 (manifest-entries manifest)
                 entries))))

(define (manifest-lookup manifest pattern)
  "Return the first item of MANIFEST that matches PATTERN, or #f if there is
no match.."
  (find (entry-predicate pattern)
        (manifest-entries manifest)))

(define (manifest-installed? manifest pattern)
  "Return #t if MANIFEST has an entry matching PATTERN (a manifest-pattern),
#f otherwise."
  (->bool (manifest-lookup manifest pattern)))

(define (manifest-matching-entries manifest patterns)
  "Return all the entries of MANIFEST that match one of the PATTERNS."
  (define predicates
    (map entry-predicate patterns))

  (define (matches? entry)
    (any (lambda (pred)
           (pred entry))
         predicates))

  (filter matches? (manifest-entries manifest)))


;;;
;;; Manifest transactions.
;;;

(define-record-type* <manifest-transaction> manifest-transaction
  make-manifest-transaction
  manifest-transaction?
  (install manifest-transaction-install ; list of <manifest-entry>
           (default '()))
  (remove  manifest-transaction-remove  ; list of <manifest-pattern>
           (default '())))

(define (manifest-transaction-effects manifest transaction)
  "Compute the effect of applying TRANSACTION to MANIFEST.  Return 3 values:
the list of packages that would be removed, installed, or upgraded when
applying TRANSACTION to MANIFEST.  Upgrades are represented as pairs where the
head is the entry being upgraded and the tail is the entry that will replace
it."
  (define (manifest-entry->pattern entry)
    (manifest-pattern
      (name   (manifest-entry-name entry))
      (output (manifest-entry-output entry))))

  (let loop ((input    (manifest-transaction-install transaction))
             (install '())
             (upgrade '()))
    (match input
      (()
       (let ((remove (manifest-transaction-remove transaction)))
         (values (manifest-matching-entries manifest remove)
                 (reverse install) (reverse upgrade))))
      ((entry rest ...)
       ;; Check whether installing ENTRY corresponds to the installation of a
       ;; new package or to an upgrade.

       ;; XXX: When the exact same output directory is installed, we're not
       ;; really upgrading anything.  Add a check for that case.
       (let* ((pattern  (manifest-entry->pattern entry))
              (previous (manifest-lookup manifest pattern)))
         (loop rest
               (if previous install (cons entry install))
               (if previous
                   (alist-cons previous entry upgrade)
                   upgrade)))))))

(define (manifest-perform-transaction manifest transaction)
  "Perform TRANSACTION on MANIFEST and return new manifest."
  (let ((install (manifest-transaction-install transaction))
        (remove  (manifest-transaction-remove transaction)))
    (manifest-add (manifest-remove manifest remove)
                  install)))

(define (right-arrow port)
  "Return either a string containing the 'RIGHT ARROW' character, or an ASCII
replacement if PORT is not Unicode-capable."
  (with-fluids ((%default-port-encoding (port-encoding port)))
    (let ((arrow "→"))
      (catch 'encoding-error
        (lambda ()
          (with-fluids ((%default-port-conversion-strategy 'error))
            (with-output-to-string
              (lambda ()
                (display arrow)))))
        (lambda (key . args)
          ">")))))

(define* (manifest-show-transaction store manifest transaction
                                    #:key dry-run?)
  "Display what will/would be installed/removed from MANIFEST by TRANSACTION."
  (define (package-strings name version output item)
    (map (lambda (name version output item)
           (format #f "   ~a-~a\t~a\t~a" name version output
                   (if (package? item)
                       (package-output store item output)
                       item)))
         name version output item))

  (define →                        ;an arrow that can be represented on stderr
    (right-arrow (current-error-port)))

  (define (upgrade-string name old-version new-version output item)
    (format #f "   ~a\t~a ~a ~a\t~a\t~a" name
            old-version → new-version
            output
            (if (package? item)
                (package-output store item output)
                item)))

  (let-values (((remove install upgrade)
                (manifest-transaction-effects manifest transaction)))
    (match remove
      ((($ <manifest-entry> name version output item _) ..1)
       (let ((len    (length name))
             (remove (package-strings name version output item)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be removed:~%~{~a~%~}~%"
                         "The following packages would be removed:~%~{~a~%~}~%"
                         len)
                     remove)
             (format (current-error-port)
                     (N_ "The following package will be removed:~%~{~a~%~}~%"
                         "The following packages will be removed:~%~{~a~%~}~%"
                         len)
                     remove))))
      (_ #f))
    (match upgrade
      (((($ <manifest-entry> name old-version)
         . ($ <manifest-entry> _ new-version output item)) ..1)
       (let ((len     (length name))
             (upgrade (map upgrade-string
                           name old-version new-version output item)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be upgraded:~%~{~a~%~}~%"
                         "The following packages would be upgraded:~%~{~a~%~}~%"
                         len)
                     upgrade)
             (format (current-error-port)
                     (N_ "The following package will be upgraded:~%~{~a~%~}~%"
                         "The following packages will be upgraded:~%~{~a~%~}~%"
                         len)
                     upgrade))))
      (_ #f))
    (match install
      ((($ <manifest-entry> name version output item _) ..1)
       (let ((len     (length name))
             (install (package-strings name version output item)))
         (if dry-run?
             (format (current-error-port)
                     (N_ "The following package would be installed:~%~{~a~%~}~%"
                         "The following packages would be installed:~%~{~a~%~}~%"
                         len)
                     install)
             (format (current-error-port)
                     (N_ "The following package will be installed:~%~{~a~%~}~%"
                         "The following packages will be installed:~%~{~a~%~}~%"
                         len)
                     install))))
      (_ #f))))


;;;
;;; Profiles.
;;;

(define (manifest-inputs manifest)
  "Return the list of inputs for MANIFEST.  Each input has one of the
following forms:

  (PACKAGE OUTPUT-NAME)

or

  STORE-PATH
"
  (append-map (match-lambda
               (($ <manifest-entry> name version
                                    output (? package? package) deps)
                `((,package ,output) ,@deps))
               (($ <manifest-entry> name version output path deps)
                ;; Assume PATH and DEPS are already valid.
                `(,path ,@deps)))
              (manifest-entries manifest)))

(define (info-dir-file manifest)
  "Return a derivation that builds the 'dir' file for all the entries of
MANIFEST."
  (define texinfo                                 ;lazy reference
    (module-ref (resolve-interface '(gnu packages texinfo)) 'texinfo))
  (define gzip                                    ;lazy reference
    (module-ref (resolve-interface '(gnu packages compression)) 'gzip))

  (define build
    #~(begin
        (use-modules (guix build utils)
                     (srfi srfi-1) (srfi srfi-26)
                     (ice-9 ftw))

        (define (info-file? file)
          (or (string-suffix? ".info" file)
              (string-suffix? ".info.gz" file)))

        (define (info-files top)
          (let ((infodir (string-append top "/share/info")))
            (map (cut string-append infodir "/" <>)
                 (or (scandir infodir info-file?) '()))))

        (define (install-info info)
          (setenv "PATH" (string-append #+gzip "/bin")) ;for info.gz files
          (zero?
           (system* (string-append #+texinfo "/bin/install-info")
                    info (string-append #$output "/share/info/dir"))))

        (mkdir-p (string-append #$output "/share/info"))
        (every install-info
               (append-map info-files
                           '#$(manifest-inputs manifest)))))

  ;; Don't depend on Texinfo when there's nothing to do.
  (if (null? (manifest-entries manifest))
      (gexp->derivation "info-dir" #~(mkdir #$output))
      (gexp->derivation "info-dir" build
                        #:modules '((guix build utils)))))

(define* (profile-derivation manifest #:key (info-dir? #t))
  "Return a derivation that builds a profile (aka. 'user environment') with
the given MANIFEST.  The profile includes a top-level Info 'dir' file, unless
INFO-DIR? is #f."
  (mlet %store-monad ((info-dir (if info-dir?
                                    (info-dir-file manifest)
                                    (return #f))))
    (define inputs
      (if info-dir
          (cons info-dir (manifest-inputs manifest))
          (manifest-inputs manifest)))

    (define builder
      #~(begin
          (use-modules (ice-9 pretty-print)
                       (guix build union))

          (setvbuf (current-output-port) _IOLBF)
          (setvbuf (current-error-port) _IOLBF)

          (union-build #$output '#$inputs
                       #:log-port (%make-void-port "w"))
          (call-with-output-file (string-append #$output "/manifest")
            (lambda (p)
              (pretty-print '#$(manifest->gexp manifest) p)))))

    (gexp->derivation "profile" builder
                      #:modules '((guix build union))
                      #:local-build? #t)))

(define (profile-regexp profile)
  "Return a regular expression that matches PROFILE's name and number."
  (make-regexp (string-append "^" (regexp-quote (basename profile))
                              "-([0-9]+)")))

(define (generation-number profile)
  "Return PROFILE's number or 0.  An absolute file name must be used."
  (or (and=> (false-if-exception (regexp-exec (profile-regexp profile)
                                              (basename (readlink profile))))
             (compose string->number (cut match:substring <> 1)))
      0))

(define (generation-numbers profile)
  "Return the sorted list of generation numbers of PROFILE, or '(0) if no
former profiles were found."
  (define* (scandir name #:optional (select? (const #t))
                    (entry<? (@ (ice-9 i18n) string-locale<?)))
    ;; XXX: Bug-fix version introduced in Guile v2.0.6-62-g139ce19.
    (define (enter? dir stat result)
      (and stat (string=? dir name)))

    (define (visit basename result)
      (if (select? basename)
          (cons basename result)
          result))

    (define (leaf name stat result)
      (and result
           (visit (basename name) result)))

    (define (down name stat result)
      (visit "." '()))

    (define (up name stat result)
      (visit ".." result))

    (define (skip name stat result)
      ;; All the sub-directories are skipped.
      (visit (basename name) result))

    (define (error name* stat errno result)
      (if (string=? name name*)             ; top-level NAME is unreadable
          result
          (visit (basename name*) result)))

    (and=> (file-system-fold enter? leaf down up skip error #f name lstat)
           (lambda (files)
             (sort files entry<?))))

  (match (scandir (dirname profile)
                  (cute regexp-exec (profile-regexp profile) <>))
    (#f                                         ; no profile directory
     '(0))
    (()                                         ; no profiles
     '(0))
    ((profiles ...)                             ; former profiles around
     (sort (map (compose string->number
                         (cut match:substring <> 1)
                         (cute regexp-exec (profile-regexp profile) <>))
                profiles)
           <))))

(define (previous-generation-number profile number)
  "Return the number of the generation before generation NUMBER of
PROFILE, or 0 if none exists.  It could be NUMBER - 1, but it's not the
case when generations have been deleted (there are \"holes\")."
  (fold (lambda (candidate highest)
          (if (and (< candidate number) (> candidate highest))
              candidate
              highest))
        0
        (generation-numbers profile)))

(define (generation-file-name profile generation)
  "Return the file name for PROFILE's GENERATION."
  (format #f "~a-~a-link" profile generation))

(define (generation-time profile number)
  "Return the creation time of a generation in the UTC format."
  (make-time time-utc 0
             (stat:ctime (stat (generation-file-name profile number)))))

;;; profiles.scm ends here
