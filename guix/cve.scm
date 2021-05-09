;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix cve)
  #:use-module (guix utils)
  #:use-module (guix http-client)
  #:use-module (guix i18n)
  #:use-module ((guix diagnostics) #:select (formatted-message))
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:export (json->cve-items

            cve-item?
            cve-item-cve
            cve-item-configurations
            cve-item-published-date
            cve-item-last-modified-date

            cve?
            cve-id
            cve-data-type
            cve-data-format
            cve-references

            cve-reference?
            cve-reference-url
            cve-reference-tags

            vulnerability?
            vulnerability-id
            vulnerability-packages

            json->vulnerabilities
            current-vulnerabilities
            vulnerabilities->lookup-proc))

;;; Commentary:
;;;
;;; This modules provides the tools to fetch, parse, and digest part of the
;;; Common Vulnerabilities and Exposures (CVE) feeds provided by the US NIST
;;; at <https://nvd.nist.gov/vuln/data-feeds>.
;;;
;;; Code:

(define (string->date* str)
  (string->date str "~Y-~m-~dT~H:~M~z"))

(define-json-mapping <cve-item> cve-item cve-item?
  json->cve-item
  (cve            cve-item-cve "cve" json->cve)   ;<cve>
  (configurations cve-item-configurations         ;list of sexps
                  "configurations" configuration-data->cve-configurations)
  (published-date cve-item-published-date
                  "publishedDate" string->date*)
  (last-modified-date cve-item-last-modified-date
                      "lastModifiedDate" string->date*))

(define-json-mapping <cve> cve cve?
  json->cve
  (id             cve-id "CVE_data_meta"          ;string
                  (cut assoc-ref <> "ID"))
  (data-type      cve-data-type                   ;'CVE
                  "data_type" string->symbol)
  (data-format    cve-data-format                 ;'MITRE
                  "data_format" string->symbol)
  (references     cve-references                  ;list of <cve-reference>
                  "references" reference-data->cve-references))

(define-json-mapping <cve-reference> cve-reference cve-reference?
  json->cve-reference
  (url            cve-reference-url)              ;string
  (tags           cve-reference-tags              ;list of strings
                  "tags" vector->list))

(define (reference-data->cve-references alist)
  (map json->cve-reference
       ;; Normally "reference_data" is always present but rejected CVEs such
       ;; as CVE-2020-10020 can lack it.
       (vector->list (or (assoc-ref alist "reference_data") '#()))))

(define %cpe-package-rx
  ;; For applications: "cpe:2.3:a:VENDOR:PACKAGE:VERSION", or sometimes
  ;; "cpe:2.3:a:VENDOR:PACKAGE:VERSION:PATCH-LEVEL".
  (make-regexp "^cpe:2\\.3:a:([^:]+):([^:]+):([^:]+):([^:]+):"))

(define (cpe->package-name cpe)
  "Converts the Common Platform Enumeration (CPE) string CPE to a package
name, in a very naive way.  Return two values: the package name, and its
version string.  Return #f and #f if CPE does not look like an application CPE
string."
  (cond ((regexp-exec %cpe-package-rx cpe)
         =>
         (lambda (matches)
           (values (match:substring matches 2)
                   (match (match:substring matches 3)
                     ("*" '_)
                     (version
                      (string-append version
                                     (match (match:substring matches 4)
                                       ("" "")
                                       (patch-level
                                        ;; Drop the colon from things like
                                        ;; "cpe:2.3:a:openbsd:openssh:6.8:p1".
                                        (string-drop patch-level 1)))))))))
        (else
         (values #f #f))))

(define (cpe-match->cve-configuration alist)
  "Convert ALIST, a \"cpe_match\" alist, into an sexp representing the package
and versions matched.  Return #f if ALIST doesn't correspond to an application
package."
  (let ((cpe    (assoc-ref alist "cpe23Uri"))
        (starti (assoc-ref alist "versionStartIncluding"))
        (starte (assoc-ref alist "versionStartExcluding"))
        (endi   (assoc-ref alist "versionEndIncluding"))
        (ende   (assoc-ref alist "versionEndExcluding")))
    ;; Normally "cpe23Uri" is here in each "cpe_match" item, but CVE-2020-0534
    ;; has a configuration that lacks it.
    (and cpe
         (let-values (((package version) (cpe->package-name cpe)))
           (and package
                `(,package
                   ,(cond ((and (or starti starte) (or endi ende))
                           `(and ,(if starti `(>= ,starti) `(> ,starte))
                                 ,(if endi `(<= ,endi) `(< ,ende))))
                          (starti `(>= ,starti))
                          (starte `(> ,starte))
                          (endi   `(<= ,endi))
                          (ende   `(< ,ende))
                          (else   version))))))))

(define (configuration-data->cve-configurations alist)
  "Given ALIST, a JSON dictionary for the baroque \"configurations\"
element found in CVEs, return an sexp such as (\"binutils\" (<
\"2.31\")) that represents matching configurations."
  (define string->operator
    (match-lambda
      ("OR" 'or)
      ("AND" 'and)))

  (define (node->configuration node)
    (let ((operator (string->operator (assoc-ref node "operator"))))
      (cond
       ((assoc-ref node "cpe_match")
        =>
        (lambda (matches)
          (let ((matches (vector->list matches)))
            (match (filter-map cpe-match->cve-configuration
                               matches)
              (()    #f)
              ((one) one)
              (lst   (cons operator lst))))))
       ((assoc-ref node "children")               ;typically for 'and'
        =>
        (lambda (children)
          (match (filter-map node->configuration (vector->list children))
            (()    #f)
            ((one) one)
            (lst   (cons operator lst)))))
       (else
        #f))))

  (let ((nodes (vector->list (assoc-ref alist "nodes"))))
    (filter-map node->configuration nodes)))

(define (json->cve-items json)
  "Parse JSON, an input port or a string, and return a list of <cve-item>
records."
  (let* ((alist   (json->scm json))
         (type    (assoc-ref alist "CVE_data_type"))
         (format  (assoc-ref alist "CVE_data_format"))
         (version (assoc-ref alist "CVE_data_version")))
    (unless (equal? type "CVE")
      (raise (condition (&message
                         (message "invalid CVE feed")))))
    (unless (equal? format "MITRE")
      (raise (formatted-message (G_ "unsupported CVE format: '~a'")
                                format)))
    (unless (equal? version "4.0")
      (raise (formatted-message (G_ "unsupported CVE data version: '~a'")
                                version)))

    (map json->cve-item
         (vector->list (assoc-ref alist "CVE_Items")))))

(define (version-matches? version sexp)
  "Return true if VERSION, a string, matches SEXP."
  (match sexp
    ('_
     #t)
    ((? string? expected)
     (version-prefix? expected version))
    (('or sexps ...)
     (any (cut version-matches? version <>) sexps))
    (('and sexps ...)
     (every (cut version-matches? version <>) sexps))
    (('< max)
     (version>? max version))
    (('<= max)
     (version>=? max version))
    (('> min)
     (version>? version min))
    (('>= min)
     (version>=? version min))))


;;;
;;; High-level interface.
;;;

(define %now
  (current-date))
(define %current-year
  (date-year %now))
(define %past-year
  (- %current-year 1))

(define (yearly-feed-uri year)
  "Return the URI for the CVE feed for YEAR."
  (string->uri
   (string-append "https://nvd.nist.gov/feeds/json/cve/1.1/nvdcve-1.1-"
                  (number->string year) ".json.gz")))

(define %current-year-ttl
  ;; According to <https://nvd.nist.gov/download.cfm#CVE_FEED>, feeds are
  ;; updated "approximately every two hours."
  (* 60 30))

(define %past-year-ttl
  ;; Update the previous year's database more and more infrequently.
  (* 3600 24 (date-month %now)))

(define-record-type <vulnerability>
  (vulnerability id packages)
  vulnerability?
  (id         vulnerability-id)             ;string
  (packages   vulnerability-packages))      ;((p1 sexp1) (p2 sexp2) ...)

(define vulnerability->sexp
  (match-lambda
    (($ <vulnerability> id packages)
     `(v ,id ,packages))))

(define sexp->vulnerability
  (match-lambda
    (('v id (packages ...))
     (vulnerability id packages))))

(define (cve-configuration->package-list config)
  "Parse CONFIG, a config sexp, and return a list of the form (P SEXP)
where P is a package name and SEXP expresses constraints on the matching
versions."
  (let loop ((config config)
             (packages '()))
    (match config
      (('or configs ...)
       (fold loop packages configs))
      (('and config _ ...)                        ;XXX
       (loop config packages))
      (((? string? package) '_)                   ;any version
       (cons `(,package _)
             (alist-delete package packages)))
      (((? string? package) sexp)
       (let ((previous (assoc-ref packages package)))
         (if previous
             (cons `(,package (or ,sexp ,@previous))
                   (alist-delete package packages))
             (cons `(,package ,sexp) packages)))))))

(define (merge-package-lists lst)
  "Merge the list in LST, each of which has the form (p sexp), where P
is the name of a package and SEXP is an sexp that constrains matching
versions."
  (fold (lambda (plist result)                    ;XXX: quadratic
          (fold (match-lambda*
                  (((package version) result)
                   (match (assoc-ref result package)
                     (#f
                      (cons `(,package ,version) result))
                     ((previous)
                      (cons `(,package (or ,version ,previous))
                            (alist-delete package result))))))
                result
                plist))
        '()
        lst))

(define (cve-item->vulnerability item)
  "Return a <vulnerability> corresponding to ITEM, a <cve-item> record;
return #f if ITEM does not list any configuration or if it does not list
any \"a\" (application) configuration."
  (let ((id (cve-id (cve-item-cve item))))
    (match (cve-item-configurations item)
      (()                                         ;no configurations
       #f)
      ((configs ...)
       (vulnerability id
                      (merge-package-lists
                       (map cve-configuration->package-list configs)))))))

(define (json->vulnerabilities json)
  "Parse JSON, an input port or a string, and return the list of
vulnerabilities found therein."
  (filter-map cve-item->vulnerability (json->cve-items json)))

(define (write-cache input cache)
  "Read vulnerabilities as gzipped JSON from INPUT, and write it as a compact
sexp to CACHE."
  (call-with-decompressed-port 'gzip input
    (lambda (input)
      (define vulns
        (json->vulnerabilities input))

      (write `(vulnerabilities
               1                                  ;format version
               ,(map vulnerability->sexp vulns))
             cache))))

(define* (fetch-vulnerabilities year ttl #:key (timeout 10))
  "Return the list of <vulnerability> for YEAR, assuming the on-disk cache has
the given TTL (fetch from the NIST web site when TTL has expired)."
  (define (cache-miss uri)
    (format (current-error-port) "fetching CVE database for ~a...~%" year))

  (define (read* port)
    ;; Disable read options to avoid populating the source property weak
    ;; table, which speeds things up, saves memory, and works around
    ;; <https://lists.gnu.org/archive/html/guile-devel/2017-09/msg00031.html>.
    (let ((options (read-options)))
      (dynamic-wind
        (lambda ()
          (read-disable 'positions))
        (lambda ()
          (read port))
        (lambda ()
          (read-options options)))))

  ;; Note: We used to keep the original JSON files in cache but parsing it
  ;; would take typically ~15s for a year of data.  Thus, we instead store a
  ;; summarized version thereof as an sexp, which can be parsed in 1s or so.
  (let* ((port (http-fetch/cached (yearly-feed-uri year)
                                  #:ttl ttl
                                  #:write-cache write-cache
                                  #:cache-miss cache-miss
                                  #:timeout timeout))
         (sexp (read* port)))
    (close-port port)
    (match sexp
      (('vulnerabilities 1 vulns)
       (map sexp->vulnerability vulns)))))

(define* (current-vulnerabilities #:key (timeout 10))
  "Return the current list of Common Vulnerabilities and Exposures (CVE) as
published by the US NIST.  TIMEOUT specifies the timeout in seconds for
connection establishment."
  (let ((past-years (unfold (cut > <> 3)
                            (lambda (n)
                              (- %current-year n))
                            1+
                            1))
        (past-ttls  (unfold (cut > <> 3)
                            (lambda (n)
                              (* n %past-year-ttl))
                            1+
                            1)))
    (append-map (cut fetch-vulnerabilities <> <> #:timeout timeout)
                (cons %current-year past-years)
                (cons %current-year-ttl past-ttls))))

(define (vulnerabilities->lookup-proc vulnerabilities)
  "Return a lookup procedure built from VULNERABILITIES that takes a package
name and optionally a version number.  When the version is omitted, the lookup
procedure returns a list of vulnerabilities; otherwise, it returns a list of
vulnerabilities affecting the given package version."
  (define table
    ;; Map package names to lists of version/vulnerability pairs.
    (fold (lambda (vuln table)
            (match vuln
              (($ <vulnerability> id packages)
               (fold (lambda (package table)
                       (match package
                         ((name . versions)
                          (vhash-cons name (cons vuln versions)
                                      table))))
                     table
                     packages))))
          vlist-null
          vulnerabilities))

  (lambda* (package #:optional version)
    (vhash-fold* (if version
                     (lambda (pair result)
                       (match pair
                         ((vuln sexp)
                          (if (version-matches? version sexp)
                              (cons vuln result)
                              result))))
                     (lambda (pair result)
                       (match pair
                         ((vuln . _)
                          (cons vuln result)))))
                 '()
                 package table)))


;;; cve.scm ends here
