;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-19)
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
  (id         vulnerability-id)
  (packages   vulnerability-packages))

(define %cve-feed-uri
  (string->uri
   "https://nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-Modified.xml.gz"))

(define %ttl
  ;; According to <https://nvd.nist.gov/download.cfm#CVE_FEED>, feeds are
  ;; updated "approximately every two hours."
  (* 3600 3))

(define (call-with-cve-port proc)
  "Pass PROC an input port from which to read the CVE stream."
  (let ((port (http-fetch/cached %cve-feed-uri #:ttl %ttl)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (call-with-decompressed-port 'gzip port
          proc))
      (lambda ()
        (close-port port)))))

(define %cpe-package-rx
  ;; For applications: "cpe:/a:VENDOR:PACKAGE:VERSION".
  (make-regexp "^cpe:/a:([^:]+):([^:]+):([^:]+)"))

(define (cpe->package-name cpe)
  "Converts the Common Platform Enumeration (CPE) string CPE to a package
name, in a very naive way.  Return #f if CPE does not look like an application
CPE string."
  (and=> (regexp-exec %cpe-package-rx (string-trim-both cpe))
         (lambda (matches)
           (cons (match:substring matches 2)
                 (match:substring matches 3)))))

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
                            (match (filter-map cpe->package-name products)
                              (()
                               ;; No application among PRODUCTS.
                               rest)
                              (packages
                               (cons (vulnerability id (reverse packages))
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

(define (current-vulnerabilities)
  "Return the current list of Common Vulnerabilities and Exposures (CVE) as
published by the US NIST."
  (call-with-cve-port
   (lambda (port)
     ;; XXX: The SSAX "error port" is used to send pointless warnings such as
     ;; "warning: Skipping PI".  Turn that off.
     (parameterize ((current-ssax-error-port (%make-void-port "w")))
       (xml->vulnerabilities port)))))

(define (vulnerabilities->lookup-proc vulnerabilities)
  "Return a lookup procedure built from VULNERABILITIES that takes a package
name and optionally a version number.  When the version is omitted, the lookup
procedure returns a list of version/vulnerability pairs; otherwise, it returns
a list of vulnerabilities affection the given package version."
  (define table
    ;; Map package names to lists of version/vulnerability pairs.
    (fold (lambda (vuln table)
            (match vuln
              (($ <vulnerability> id packages)
               (fold (lambda (package table)
                       (match package
                         ((name . version)
                          (vhash-cons name (cons version vuln)
                                      table))))
                     table
                     packages))))
          vlist-null
          vulnerabilities))

  (lambda* (package #:optional version)
    (vhash-fold* (if version
                     (lambda (pair result)
                       (match pair
                         ((v . vuln)
                          (if (string=? v version)
                              (cons vuln result)
                              result))))
                     cons)
                 '()
                 package table)))

;;; cve.scm ends here
