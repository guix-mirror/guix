;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (ice-9 receive)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (web uri)
  #:use-module (guix http-client)
  #:use-module (guix git)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (opam->guix-package
            opam-recursive-import))

;; Define a PEG parser for the opam format
(define-peg-pattern SP none (or " " "\n"))
(define-peg-pattern SP2 body (or " " "\n"))
(define-peg-pattern QUOTE none "\"")
(define-peg-pattern QUOTE2 body "\"")
(define-peg-pattern COLON none ":")
;; A string character is any character that is not a quote, or a quote preceded by a backslash.
(define-peg-pattern STRCHR body
                    (or " " "!" (and (ignore "\\") "\"")
                        (and (ignore "\\") "\\") (range #\# #\頋)))
(define-peg-pattern operator all (or "=" "!" "<" ">"))

(define-peg-pattern records body (* (and (or record weird-record) (* SP))))
(define-peg-pattern record all (and key COLON (* SP) value))
(define-peg-pattern weird-record all (and key (* SP) dict))
(define-peg-pattern key body (+ (or (range #\a #\z) "-")))
(define-peg-pattern value body (and (or conditional-value ground-value operator) (* SP)))
(define-peg-pattern ground-value body (and (or multiline-string string-pat list-pat var) (* SP)))
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
                                    condition-neq condition-eq condition-content) (* SP)))

;(define-peg-pattern condition-operator all (and (ignore operator) (* SP) condition-string))
(define-peg-pattern condition-greater-or-equal all (and (ignore (and ">" "=")) (* SP) condition-string))
(define-peg-pattern condition-greater all (and (ignore ">") (* SP) condition-string))
(define-peg-pattern condition-lower-or-equal all (and (ignore (and "<" "=")) (* SP) condition-string))
(define-peg-pattern condition-lower all (and (ignore "<") (* SP) condition-string))
(define-peg-pattern condition-and all (and condition-form2 (* SP) (? (ignore "&")) (* SP) condition-form))
(define-peg-pattern condition-or all (and condition-form2 (* SP) (ignore "|") (* SP) condition-form))
(define-peg-pattern condition-eq all (and condition-content (* SP) (ignore "=") (* SP) condition-content))
(define-peg-pattern condition-neq all (and condition-content (* SP) (ignore (and "!" "=")) (* SP) condition-content))
(define-peg-pattern condition-content body (or condition-string condition-var))
(define-peg-pattern condition-content2 body (and condition-content (* SP) (not-followed-by (or "&" "=" "!"))))
(define-peg-pattern condition-string all (and QUOTE (* STRCHR) QUOTE))
(define-peg-pattern condition-var all (+ (or (range #\a #\z) "-")))

(define (get-opam-repository)
  "Update or fetch the latest version of the opam repository and return the
path to the repository."
  (receive (location commit)
    (update-cached-checkout "https://github.com/ocaml/opam-repository")
    location))

(define (latest-version versions)
  "Find the most recent version from a list of versions."
  (fold (lambda (a b) (if (version>? a b) a b)) (car versions) versions))

(define (find-latest-version package repository)
  "Get the latest version of a package as described in the given repository."
  (let* ((dir (string-append repository "/packages/" package))
         (versions (scandir dir (lambda (name) (not (string-prefix? "." name))))))
    (if versions
      (let ((versions (map
                        (lambda (dir)
                          (string-join (cdr (string-split dir #\.)) "."))
                        versions)))
        (latest-version versions))
      (begin
        (format #t (G_ "Package not found in opam repository: ~a~%") package)
        #f))))

(define (get-metadata opam-file)
  (with-input-from-file opam-file
    (lambda _
      (peg:tree (match-pattern records (get-string-all (current-input-port)))))))

(define (ocaml-name->guix-name name)
  (cond
    ((equal? name "ocamlfind") "ocaml-findlib")
    ((string-prefix? "ocaml" name) name)
    ((string-prefix? "conf-" name) (substring name 5))
    (else (string-append "ocaml-" name))))

(define (metadata-ref file lookup)
  (fold (lambda (record acc)
          (match record
            ((record key val)
             (if (equal? key lookup)
               (match val
                 (('list-pat . stuff) stuff)
                 (('string-pat stuff) stuff)
                 (('multiline-string stuff) stuff)
                 (('dict records ...) records))
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
    (('conditional-value val condition)
     (if (native? condition) "" (dependency->input val)))))

(define (dependency->native-input dependency)
  (match dependency
    (('string-pat str) "")
    (('conditional-value val condition)
     (if (native? condition) (dependency->input val) ""))))

(define (dependency->name dependency)
  (match dependency
    (('string-pat str) str)
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

(define (depends->inputs depends)
  (filter (lambda (name)
            (and (not (equal? "" name))
                 (not (equal? "ocaml" name))
                 (not (equal? "ocamlfind" name))))
    (map dependency->input depends)))

(define (depends->native-inputs depends)
  (filter (lambda (name) (not (equal? "" name)))
    (map dependency->native-input depends)))

(define (dependency-list->inputs lst)
  (map
    (lambda (dependency)
      (list dependency (list 'unquote (string->symbol dependency))))
    (ocaml-names->guix-names lst)))

(define (opam->guix-package name)
  (and-let* ((repository (get-opam-repository))
             (version (find-latest-version name repository))
             (file (string-append repository "/packages/" name "/" name "." version "/opam"))
             (opam-content (get-metadata file))
             (url-dict (metadata-ref opam-content "url"))
             (source-url (metadata-ref url-dict "src"))
             (requirements (metadata-ref opam-content "depends"))
             (dependencies (dependency-list->names requirements))
             (inputs (dependency-list->inputs (depends->inputs requirements)))
             (native-inputs (dependency-list->inputs (depends->native-inputs requirements))))
        (call-with-temporary-output-file
          (lambda (temp port)
            (and (url-fetch source-url temp)
                 (values
                  `(package
                     (name ,(ocaml-name->guix-name name))
                     (version ,(metadata-ref opam-content "version"))
                     (source
                       (origin
                         (method url-fetch)
                         (uri ,source-url)
                         (sha256 (base32 ,(guix-hash-url temp)))))
                     (build-system ocaml-build-system)
                     ,@(if (null? inputs)
                         '()
                         `((inputs ,(list 'quasiquote inputs))))
                     ,@(if (null? native-inputs)
                         '()
                         `((native-inputs ,(list 'quasiquote native-inputs))))
                     (home-page ,(metadata-ref opam-content "homepage"))
                     (synopsis ,(metadata-ref opam-content "synopsis"))
                     (description ,(metadata-ref opam-content "description"))
                     (license #f))
                  dependencies))))))

(define (opam-recursive-import package-name)
  (recursive-import package-name #f
                    #:repo->guix-package (lambda (name repo)
                                           (opam->guix-package name))
                    #:guix-name ocaml-name->guix-name))
