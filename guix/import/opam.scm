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
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (guix http-client)
  #:use-module (guix utils)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (opam->guix-package))

(define (opam-urls)
  "Fetch the urls.txt file from the opam repository and returns the list of
URLs it contains."
  (let ((port (http-fetch/cached (string->uri "https://opam.ocaml.org/urls.txt"))))
    (let loop ((result '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin
            (close port)
            result)
          (loop (cons line result)))))))

(define (vhash-ref hashtable key default)
  (match (vhash-assoc key hashtable)
    (#f default)
    ((_ . x) x)))

(define (hashtable-update hashtable line)
  "Parse @var{line} to get the name and version of the package and adds them
to the hashtable."
  (let* ((line (string-split line #\ )))
    (match line
      ((url foo ...)
       (if (equal? url "repo")
         hashtable
         (match (string-split url #\/)
           ((type name1 versionstr foo ...)
            (if (equal? type "packages")
              (match (string-split versionstr #\.)
                ((name2 versions ...)
                 (let ((version (string-join versions ".")))
                   (if (equal? name1 name2)
                     (let ((curr (vhash-ref hashtable name1 '())))
                       (vhash-cons name1 (cons version curr) hashtable))
                     hashtable)))
                (_ hashtable))
              hashtable))
           (_ hashtable))))
      (_ hashtable))))

(define (urls->hashtable urls)
  "Transform urls.txt in a hashtable whose keys are package names and values
the list of available versions."
  (let ((hashtable vlist-null))
    (let loop ((urls urls) (hashtable hashtable))
      (match urls
        (() hashtable)
        ((url rest ...) (loop rest (hashtable-update hashtable url)))))))

(define (latest-version versions)
  "Find the most recent version from a list of versions."
  (match versions
    ((first rest ...)
     (let loop ((versions rest) (m first))
       (match versions
         (() m)
         ((first rest ...)
          (loop rest (if (version>? m first) m first))))))))

(define (fetch-package-url uri)
  "Fetch and parse the url file.  Return the URL the package can be downloaded
from."
  (let ((port (http-fetch uri)))
    (let loop ((result #f))
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin
            (close port)
            result)
          (let* ((line (string-split line #\ )))
            (match line
              ((key value rest ...)
               (if (member key '("archive:" "http:"))
                 (loop (string-trim-both value #\"))
                 (loop result))))))))))

(define (fetch-package-metadata uri)
  "Fetch and parse the opam file.  Return an association list containing the
homepage, the license and the list of inputs."
  (let ((port (http-fetch uri)))
    (let loop ((result '()) (dependencies? #f))
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin
            (close port)
            result)
          (let* ((line (string-split line #\ )))
            (match line
               ((key value ...)
                (let ((dependencies?
                        (if dependencies?
                          (not (equal? key "]"))
                          (equal? key "depends:")))
                      (val (string-trim-both (string-join value "") #\")))
                  (cond
                    ((equal? key "homepage:")
                     (loop (cons `("homepage" . ,val) result) dependencies?))
                    ((equal? key "license:")
                     (loop (cons `("license" . ,val) result) dependencies?))
                    ((and dependencies? (not (equal? val "[")))
                     (match (string-split val #\{)
                       ((val rest ...)
                        (let ((curr (assoc-ref result "inputs"))
                              (new (string-trim-both
                                     val (list->char-set '(#\] #\[ #\")))))
                          (loop (cons `("inputs" . ,(cons new (if curr curr '()))) result)
                                (if (string-contains val "]") #f dependencies?))))))
                    (else (loop result dependencies?))))))))))))

(define (string->license str)
  (cond
    ((equal? str "MIT") '(license:expat))
    ((equal? str "GPL2") '(license:gpl2))
    ((equal? str "LGPLv2") '(license:lgpl2))
    (else `())))

(define (ocaml-name->guix-name name)
  (cond
    ((equal? name "ocamlfind") "ocaml-findlib")
    ((string-prefix? "ocaml" name) name)
    ((string-prefix? "conf-" name) (substring name 5))
    (else (string-append "ocaml-" name))))

(define (dependencies->inputs dependencies)
  "Transform the list of dependencies in a list of inputs."
  (if (not dependencies)
    '()
    (map (lambda (input)
           (list input (list 'unquote (string->symbol input))))
         (map ocaml-name->guix-name dependencies))))

(define (opam->guix-package name)
  (let* ((hashtable (urls->hashtable (opam-urls)))
         (versions (vhash-ref hashtable name #f)))
    (unless (eq? versions #f)
      (let* ((version (latest-version versions))
             (package-url (string-append "https://opam.ocaml.org/packages/" name
                                         "/" name "." version "/"))
             (url-url (string-append package-url "url"))
             (opam-url (string-append package-url "opam"))
             (source-url (fetch-package-url url-url))
             (metadata (fetch-package-metadata opam-url))
             (dependencies (assoc-ref metadata "inputs"))
             (inputs (dependencies->inputs dependencies)))
        (call-with-temporary-output-file
          (lambda (temp port)
            (and (url-fetch source-url temp)
                 `(package
                    (name ,(ocaml-name->guix-name name))
                    (version ,version)
                    (source
                      (origin
                        (method url-fetch)
                        (uri ,source-url)
                        (sha256 (base32 ,(guix-hash-url temp)))))
                    (build-system ocaml-build-system)
                    ,@(if (null? inputs)
                        '()
                        `((inputs ,(list 'quasiquote inputs))))
                    (home-page ,(assoc-ref metadata "homepage"))
                    (synopsis "")
                    (description "")
                    (license ,@(string->license (assoc-ref metadata "license")))))))))))
