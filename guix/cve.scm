;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (sxml ssax)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:export (vulnerability?
            vulnerability-id
            vulnerability-packages

            xml->vulnerabilities
            current-vulnerabilities
            vulnerabilities->lookup-proc))

;;; Commentary:
;;;
;;; This modules provides the tools to fetch, parse, and digest part of the
;;; Common Vulnerabilities and Exposures (CVE) feeds provided by the US NIST
;;; at <https://nvd.nist.gov/download.cfm#CVE_FEED>.
;;;
;;; Code:

(define-record-type <vulnerability>
  (vulnerability id packages)
  vulnerability?
  (id         vulnerability-id)                   ;string
  (packages   vulnerability-packages))            ;((p1 v1 v2 v3) (p2 v1) ...)

(define %now
  (current-date))
(define %current-year
  (date-year %now))
(define %past-year
  (- %current-year 1))

(define (yearly-feed-uri year)
  "Return the URI for the CVE feed for YEAR."
  (string->uri
   (string-append "https://static.nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-"
                  (number->string year) ".xml.gz")))

(define %current-year-ttl
  ;; According to <https://nvd.nist.gov/download.cfm#CVE_FEED>, feeds are
  ;; updated "approximately every two hours."
  (* 60 30))

(define %past-year-ttl
  ;; Update the previous year's database more and more infrequently.
  (* 3600 24 (date-month %now)))

(define %cpe-package-rx
  ;; For applications: "cpe:/a:VENDOR:PACKAGE:VERSION", or sometimes
  ;; "cpe/a:VENDOR:PACKAGE:VERSION:PATCH-LEVEL".
  (make-regexp "^cpe:/a:([^:]+):([^:]+):([^:]+)((:.+)?)"))

(define (cpe->package-name cpe)
  "Converts the Common Platform Enumeration (CPE) string CPE to a package
name, in a very naive way.  Return two values: the package name, and its
version string.  Return #f and #f if CPE does not look like an application CPE
string."
  (cond ((regexp-exec %cpe-package-rx (string-trim-both cpe))
         =>
         (lambda (matches)
           (values (match:substring matches 2)
                   (string-append (match:substring matches 3)
                                  (match (match:substring matches 4)
                                    ("" "")
                                    (patch-level
                                     ;; Drop the colon from things like
                                     ;; "cpe:/a:openbsd:openssh:6.8:p1".
                                     (string-drop patch-level 1)))))))
        (else
         (values #f #f))))

(define (cpe->product-alist products)
  "Given PRODUCTS, a list of CPE names, return the subset limited to the
applications listed in PRODUCTS, with names converted to package names:

  (cpe->product-alist
    '(\"cpe:/a:gnu:libtasn1:4.7\" \"cpe:/a:gnu:libtasn1:4.6\" \"cpe:/a:gnu:cpio:2.11\"))
  => ((\"libtasn1\" \"4.7\" \"4.6\") (\"cpio\" \"2.11\"))
"
  (fold (lambda (product result)
          (let-values (((name version) (cpe->package-name product)))
            (if name
                (match result
                  (((previous . versions) . tail)
                   ;; Attempt to coalesce NAME and PREVIOUS.
                   (if (string=? name previous)
                       (alist-cons name (cons version versions) tail)
                       (alist-cons name (list version) result)))
                  (()
                   (alist-cons name (list version) result)))
                result)))
        '()
        (sort products string<?)))

(define %parse-vulnerability-feed
  ;; Parse the XML vulnerability feed from
  ;; <https://nvd.nist.gov/download.cfm#CVE_FEED> and return a list of
  ;; vulnerability objects.
  (ssax:make-parser NEW-LEVEL-SEED
                    (lambda (elem-gi attributes namespaces expected-content
                                     seed)
                      (match elem-gi
                        ((name-space . 'entry)
                         (cons (assoc-ref attributes 'id) seed))
                        ((name-space . 'vulnerable-software-list)
                         (cons '() seed))
                        ((name-space . 'product)
                         (cons 'product seed))
                        (x seed)))

                    FINISH-ELEMENT
                    (lambda (elem-gi attributes namespaces parent-seed
                                     seed)
                      (match elem-gi
                        ((name-space . 'entry)
                         (match seed
                           (((? string? id) . rest)
                            ;; Some entries have no vulnerable-software-list.
                            rest)
                           ((products id . rest)
                            (match (cpe->product-alist products)
                              (()
                               ;; No application among PRODUCTS.
                               rest)
                              (packages
                               (cons (vulnerability id packages)
                                     rest))))))
                        (x
                         seed)))

                    CHAR-DATA-HANDLER
                    (lambda (str _ seed)
                      (match seed
                        (('product software-list . rest)
                         ;; Add STR to the vulnerable software list this
                         ;; <product> tag is part of.
                         (cons (cons str software-list) rest))
                        (x x)))))

(define (xml->vulnerabilities port)
  "Read from PORT an XML feed of vulnerabilities and return a list of
vulnerability objects."
  (reverse (%parse-vulnerability-feed port '())))

(define vulnerability->sexp
  (match-lambda
    (($ <vulnerability> id packages)
     `(v ,id ,packages))))

(define sexp->vulnerability
  (match-lambda
    (('v id (packages ...))
     (vulnerability id packages))))

(define (write-cache input cache)
  "Read vulnerabilities as gzipped XML from INPUT, and write it as a compact
sexp to CACHE."
  (call-with-decompressed-port 'gzip input
    (lambda (input)
      ;; XXX: The SSAX "error port" is used to send pointless warnings such as
      ;; "warning: Skipping PI".  Turn that off.
      (define vulns
        (parameterize ((current-ssax-error-port (%make-void-port "w")))
          (xml->vulnerabilities input)))

      (write `(vulnerabilities
               1                                  ;format version
               ,(map vulnerability->sexp vulns))
             cache))))

(define (fetch-vulnerabilities year ttl)
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

  ;; Note: We used to keep the original XML files in cache but parsing it
  ;; would take typically ~15s for a year of data.  Thus, we instead store a
  ;; summarized version thereof as an sexp, which can be parsed in 1s or so.
  (let* ((port (http-fetch/cached (yearly-feed-uri year)
                                  #:ttl ttl
                                  #:write-cache write-cache
                                  #:cache-miss cache-miss))
         (sexp (read* port)))
    (close-port port)
    (match sexp
      (('vulnerabilities 1 vulns)
       (map sexp->vulnerability vulns)))))

(define (current-vulnerabilities)
  "Return the current list of Common Vulnerabilities and Exposures (CVE) as
published by the US NIST."
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
    (append-map fetch-vulnerabilities
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
                         ((vuln . versions)
                          (if (member version versions)
                              (cons vuln result)
                              result))))
                     (lambda (pair result)
                       (match pair
                         ((vuln . _)
                          (cons vuln result)))))
                 '()
                 package table)))


;;; cve.scm ends here
