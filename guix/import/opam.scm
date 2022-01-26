;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021, 2022 Alice Brenon <alice.brenon@ens-lyon.fr>
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

(define-module (guix import opam)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module ((ice-9 popen) #:select (open-pipe*))
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module ((srfi srfi-26) #:select (cut))
  #:use-module ((web uri) #:select (string->uri uri->string))
  #:use-module ((guix build utils) #:select (dump-port find-files mkdir-p))
  #:use-module (guix build-system)
  #:use-module (guix build-system ocaml)
  #:use-module (guix http-client)
  #:use-module (guix ui)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix utils) #:select (cache-directory
                                       version>?
                                       call-with-temporary-output-file))
  #:use-module ((guix import utils) #:select (beautify-description
                                              guix-hash-url
                                              recursive-import
                                              spdx-string->license
                                              url-fetch))
  #:use-module ((guix licenses) #:prefix license:)
  #:export (opam->guix-package
            opam-recursive-import
            %opam-updater

            ;; The following patterns are exported for testing purposes.
            string-pat
            multiline-string
            list-pat
            dict
            condition))

;; Define a PEG parser for the opam format
(define-peg-pattern comment none (and "#" (* COMMCHR) "\n"))
(define-peg-pattern SP none (or " " "\n" comment))
(define-peg-pattern SP2 body (or " " "\n"))
(define-peg-pattern QUOTE none "\"")
(define-peg-pattern QUOTE2 body "\"")
(define-peg-pattern COLON none ":")
;; A string character is any character that is not a quote, or a quote preceded by a backslash.
(define-peg-pattern COMMCHR none
                    (or " " "!" "\\" "\"" (range #\# #\頋)))
(define-peg-pattern STRCHR body
                    (or " " "!" "\n" (and (ignore "\\") "\"")
                        (ignore "\\\n") (and (ignore "\\") "\\")
                        (range #\# #\頋)))
(define-peg-pattern operator all (or "=" "!" "<" ">"))

(define-peg-pattern records body (and (* SP) (* (and (or record weird-record) (* SP)))))
(define-peg-pattern record all (and key COLON (* SP) value))
(define-peg-pattern weird-record all (and key (* SP) dict))
(define-peg-pattern key body (+ (or (range #\a #\z) "-")))
(define-peg-pattern value body (and (or conditional-value ground-value operator) (* SP)))
(define-peg-pattern choice-pat all (and (ignore "(") (* SP) choice (* SP)  (ignore ")")))
(define-peg-pattern choice body
  (or (and (or conditional-value ground-value) (* SP) (ignore "|") (* SP) choice)
      group-pat
      conditional-value
      ground-value))
(define-peg-pattern group-pat all
                    (and (or conditional-value ground-value) (* SP) (ignore "&") (* SP)
                         (or group-pat conditional-value ground-value)))
(define-peg-pattern ground-value body (and (or multiline-string string-pat choice-pat list-pat var) (* SP)))
(define-peg-pattern conditional-value all (and ground-value (* SP) condition))
(define-peg-pattern string-pat all (and QUOTE (* STRCHR) QUOTE))
(define-peg-pattern list-pat all (and (ignore "[") (* SP) (* (and value (* SP))) (ignore "]")))
(define-peg-pattern var all (+ (or (range #\a #\z) "-")))
(define-peg-pattern multiline-string all
                    (and QUOTE QUOTE QUOTE (* SP)
                         (* (or SP2 STRCHR (and QUOTE2 (not-followed-by QUOTE))
                                (and QUOTE2 QUOTE2 (not-followed-by QUOTE))))
                         QUOTE QUOTE QUOTE))
(define-peg-pattern dict all (and (ignore "{") (* SP) records (* SP) (ignore "}")))

(define-peg-pattern condition body (and (ignore "{") condition-form (ignore "}")))

(define-peg-pattern condition-form body
                    (and
                      (* SP)
                      (or condition-and condition-or condition-form2)
                      (* SP)))
(define-peg-pattern condition-form2 body
                    (and (* SP) (or condition-greater-or-equal condition-greater
                                    condition-lower-or-equal condition-lower
                                    condition-neq condition-eq condition-not
                                    condition-content) (* SP)))

;(define-peg-pattern condition-operator all (and (ignore operator) (* SP) condition-string))
(define-peg-pattern condition-greater-or-equal all (and (ignore (and ">" "=")) (* SP) condition-string))
(define-peg-pattern condition-greater all (and (ignore ">") (* SP) condition-string))
(define-peg-pattern condition-lower-or-equal all (and (ignore (and "<" "=")) (* SP) condition-string))
(define-peg-pattern condition-lower all (and (ignore "<") (* SP) condition-string))
(define-peg-pattern condition-and all (and condition-form2 (* SP) (? (ignore "&")) (* SP) condition-form))
(define-peg-pattern condition-or all (and condition-form2 (* SP) (ignore "|") (* SP) condition-form))
(define-peg-pattern condition-eq all (and (? condition-content) (* SP) (ignore "=") (* SP) condition-content))
(define-peg-pattern condition-neq all (and (? condition-content) (* SP) (ignore (and "!" "=")) (* SP) condition-content))
(define-peg-pattern condition-not all (and (ignore (and "!")) (* SP) condition-content))
(define-peg-pattern condition-content body (or condition-paren condition-string condition-var))
(define-peg-pattern condition-content2 body (and condition-content (* SP) (not-followed-by (or "&" "=" "!"))))
(define-peg-pattern condition-paren body (and "(" condition-form ")"))
(define-peg-pattern condition-string all (and QUOTE (* STRCHR) QUOTE))
(define-peg-pattern condition-var all (+ (or (range #\a #\z) "-" ":")))

(define (opam-cache-directory path)
  (string-append (cache-directory) "/opam/" path))

(define known-repositories
  '((opam . "https://opam.ocaml.org")
    (coq . "https://coq.inria.fr/opam/released")
    (coq-released . "https://coq.inria.fr/opam/released")
    (coq-core-dev . "https://coq.inria.fr/opam/core-dev")
    (coq-extra-dev . "https://coq.inria.fr/opam/extra-dev")
    (grew . "http://opam.grew.fr")))

(define (get-uri repo-root)
  (let ((archive-file (string-append repo-root "/index.tar.gz")))
    (or (string->uri archive-file)
        (begin
          (warning (G_ "'~a' is not a valid URI~%") archive-file)
          'bad-repo))))

(define (repo-type repo)
  (match (assoc-ref known-repositories (string->symbol repo))
    (#f (if (file-exists? repo)
            `(local ,repo)
            `(remote ,(get-uri repo))))
    (url `(remote ,(get-uri url)))))

(define (update-repository input)
  "Make sure the cache for opam repository INPUT is up-to-date"
  (let* ((output (opam-cache-directory (basename (port-filename input))))
         (cached-date (if (file-exists? output)
                          (stat:mtime (stat output))
                          (begin (mkdir-p output) 0))))
    (when (> (stat:mtime (stat input)) cached-date)
      (call-with-port
       (open-pipe* OPEN_WRITE "tar" "xz" "-C" output "-f" "-")
       (cut dump-port input <>)))
    output))

(define* (get-opam-repository #:optional (repo "opam"))
  "Update or fetch the latest version of the opam repository and return the
path to the repository."
  (match (repo-type repo)
    (('local p) p)
    (('remote 'bad-repo) #f) ; to weed it out during filter-map in opam-fetch
    (('remote r) (call-with-port (http-fetch/cached r) update-repository))))

;; Prevent Guile 3 from inlining this procedure so we can mock it in tests.
(set! get-opam-repository get-opam-repository)

(define (get-version-and-file path)
  "Analyse a candidate path and return an list containing information for proper
  version comparison as well as the source path for metadata."
  (and-let* ((metadata-file (string-append path "/opam"))
             (filename (basename path))
             (version (string-join (cdr (string-split filename #\.)) ".")))
    (and (file-exists? metadata-file)
         (eq? 'regular (stat:type (stat metadata-file)))
         (if (string-prefix? "v" version)
             `(V ,(substring version 1) ,metadata-file)
             `(digits ,version ,metadata-file)))))

(define (keep-max-version a b)
  "Version comparison on the lists returned by the previous function taking the
  janestreet re-versioning into account (v-prefixed come first)."
  (match (cons a b)
    ((('V va _) . ('V vb _)) (if (version>? va vb) a b))
    ((('V _ _) . _) a)
    ((_ . ('V _ _)) b)
    ((('digits va _) . ('digits vb _)) (if (version>? va vb) a b))))

(define (find-latest-version package repository)
  "Get the latest version of a package as described in the given repository."
  (let ((packages (string-append repository "/packages"))
        (filter (make-regexp (string-append "^" package "\\."))))
    (reduce keep-max-version #f
            (filter-map
             get-version-and-file
             (find-files packages filter #:directories? #t)))))

(define (get-metadata opam-file)
  (with-input-from-file opam-file
    (lambda _
      (peg:tree (match-pattern records (get-string-all (current-input-port)))))))

(define (substitute-char str what with)
  (string-join (string-split str what) with))

(define (ocaml-name->guix-name name)
  (substitute-char
    (cond
      ((equal? name "ocamlfind") "ocaml-findlib")
      ((equal? name "coq") name)
      ((string-prefix? "ocaml" name) name)
      ((string-prefix? "conf-" name) (substring name 5))
      (else (string-append "ocaml-" name)))
    #\_ "-"))

(define (metadata-ref file lookup)
  (fold (lambda (record acc)
          (match record
            ((record key val)
             (if (equal? key lookup)
               (match val
                 (('list-pat . stuff) stuff)
                 (('string-pat stuff) stuff)
                 (('multiline-string stuff) stuff)
                 (('dict records ...) records)
                 (_ #f))
               acc))))
        #f file))

(define (native? condition)
  (match condition
    (('condition-var var)
     (match var
       ("with-test" #t)
       ("test" #t)
       ("build" #t)
       (_ #f)))
    ((or ('condition-or cond-left cond-right) ('condition-and cond-left cond-right))
     (or (native? cond-left)
         (native? cond-right)))
    (_ #f)))

(define (dependency->input dependency)
  (match dependency
    (('string-pat str) str)
    ;; Arbitrary select the first dependency
    (('choice-pat choice ...) (dependency->input (car choice)))
    (('group-pat val ...) (map dependency->input val))
    (('conditional-value val condition)
     (if (native? condition) "" (dependency->input val)))))

(define (dependency->native-input dependency)
  (match dependency
    (('string-pat str) "")
    ;; Arbitrary select the first dependency
    (('choice-pat choice ...) (dependency->native-input (car choice)))
    (('group-pat val ...) (map dependency->native-input val))
    (('conditional-value val condition)
     (if (native? condition) (dependency->input val) ""))))

(define (dependency->name dependency)
  (match dependency
    (('string-pat str) str)
    ;; Arbitrary select the first dependency
    (('choice-pat choice ...) (dependency->name (car choice)))
    (('group-pat val ...) (map dependency->name val))
    (('conditional-value val condition)
     (dependency->name val))))

(define (dependency-list->names lst)
  (filter
    (lambda (name)
      (not (or
             (string-prefix? "conf-" name)
             (equal? name "ocaml")
             (equal? name "findlib"))))
    (map dependency->name lst)))

(define (ocaml-names->guix-names names)
  (map ocaml-name->guix-name
       (remove (lambda (name)
                 (or (equal? "" name))
                     (equal? "ocaml" name))
               names)))

(define (filter-dependencies depends)
  "Remove implicit dependencies from the list of dependencies in @var{depends}."
  (filter (lambda (name)
            (and (not (member name '("" "ocaml" "ocamlfind" "dune" "jbuilder")))
                 (not (string-prefix? "base-" name))))
          depends))

(define (depends->inputs depends)
  (filter-dependencies (map dependency->input depends)))

(define (depends->native-inputs depends)
  (filter (lambda (name) (not (equal? "" name)))
          (map dependency->native-input depends)))

(define (dependency-list->inputs lst)
  (map string->symbol
       (ocaml-names->guix-names lst)))

(define* (opam-fetch name #:optional (repositories-specs '("opam")))
  (or (fold (lambda (repository others)
              (match (find-latest-version name repository)
                ((_ version file) `(("metadata" ,@(get-metadata file))
                                    ("version" . ,version)))
                (_ others)))
            #f
            (filter-map get-opam-repository repositories-specs))
      (warning (G_ "opam: package '~a' not found~%") name)))

(define* (opam->guix-package name #:key (repo 'opam) version)
  "Import OPAM package NAME from REPOSITORY (a directory name) or, if
REPOSITORY is #f, from the official OPAM repository.  Return a 'package' sexp
or #f on failure."
  (and-let* ((with-opam (if (member "opam" repo) repo (cons "opam" repo)))
             (opam-file (opam-fetch name with-opam))
             (version (assoc-ref opam-file "version"))
             (opam-content (assoc-ref opam-file "metadata"))
             (url-dict (metadata-ref opam-content "url"))
             (source-url (or (metadata-ref url-dict "src")
                             (metadata-ref url-dict "archive")))
             (requirements (metadata-ref opam-content "depends"))
             (names (dependency-list->names requirements))
             (dependencies (filter-dependencies names))
             (native-dependencies (depends->native-inputs requirements))
             (inputs (dependency-list->inputs (depends->inputs requirements)))
             (native-inputs (dependency-list->inputs
                              ;; Do not add dune nor jbuilder since they are
                              ;; implicit inputs of the dune-build-system.
                              (filter
                                (lambda (name)
                                  (not (member name '("dune" "jbuilder"))))
                                native-dependencies))))
        (let ((use-dune? (member "dune" names)))
          (call-with-temporary-output-file
            (lambda (temp port)
              (and (url-fetch source-url temp)
                   (values
                    `(package
                       (name ,(ocaml-name->guix-name name))
                       (version ,version)
                       (source
                         (origin
                           (method url-fetch)
                           (uri ,source-url)
                           (sha256 (base32 ,(guix-hash-url temp)))))
                       (build-system ,(if use-dune?
                                          'dune-build-system
                                          'ocaml-build-system))
                       ,@(if (null? inputs)
                           '()
                           `((propagated-inputs (list ,@inputs))))
                       ,@(if (null? native-inputs)
                           '()
                           `((native-inputs (list ,@native-inputs))))
                       ,@(if (equal? name (guix-name->opam-name (ocaml-name->guix-name name)))
                           '()
                           `((properties
                               ,(list 'quasiquote `((upstream-name . ,name))))))
                       (home-page ,(metadata-ref opam-content "homepage"))
                       (synopsis ,(metadata-ref opam-content "synopsis"))
                       (description ,(beautify-description
                                      (metadata-ref opam-content "description")))
                       (license ,(spdx-string->license
                                  (metadata-ref opam-content "license"))))
                    (filter
                      (lambda (name)
                        (not (member name '("dune" "jbuilder"))))
                      dependencies))))))))

(define* (opam-recursive-import package-name #:key repo)
  (recursive-import package-name
                    #:repo->guix-package opam->guix-package
                    #:guix-name ocaml-name->guix-name
                    #:repo repo))

(define (guix-name->opam-name name)
  (if (string-prefix? "ocaml-" name)
    (substring name 6)
    name))

(define (guix-package->opam-name package)
  "Given an OCaml PACKAGE built from OPAM, return the name of the
package in OPAM."
  (let ((upstream-name (assoc-ref
                         (package-properties package)
                         'upstream-name))
        (name (package-name package)))
    (if upstream-name
      upstream-name
      (guix-name->opam-name name))))

(define (opam-package? package)
  "Return true if PACKAGE is an OCaml package from OPAM"
  (and
    (member (build-system-name (package-build-system package)) '(dune ocaml))
    (not (string-prefix? "ocaml4" (package-name package)))))

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (and-let* ((opam-name (guix-package->opam-name package))
             (opam-file (opam-fetch opam-name))
             (version (assoc-ref opam-file "version"))
             (opam-content (assoc-ref opam-file "metadata"))
             (url-dict (metadata-ref opam-content "url"))
             (source-url (metadata-ref url-dict "src")))
    (upstream-source
      (package (package-name package))
      (version version)
      (urls (list source-url)))))

(define %opam-updater
  (upstream-updater
    (name 'opam)
    (description "Updater for OPAM packages")
    (pred opam-package?)
    (latest latest-release)))
