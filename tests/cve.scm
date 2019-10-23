;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-cve)
  #:use-module (guix cve)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64))

(define %sample
  (search-path %load-path "tests/cve-sample.json"))

(define (vulnerability id packages)
  (make-struct/no-tail (@@ (guix cve) <vulnerability>) id packages))

(define %expected-vulnerabilities
  ;; What we should get when reading %SAMPLE.
  (list
   (vulnerability "CVE-2019-0001"
                  ;; Only the "a" CPE configurations are kept; the "o"
                  ;; configurations are discarded.
                  '(("junos" (or "18.21-s4" (or "18.21-s3" "18.2")))))
   (vulnerability "CVE-2019-0005"
                  '(("junos" (or "18.11" "18.1"))))
   ;; CVE-2019-0005 has no "a" configurations.
   (vulnerability "CVE-2019-14811"
                  '(("ghostscript" (< "9.28"))))
   (vulnerability "CVE-2019-17365"
                  '(("nix" (<= "2.3"))))
   (vulnerability "CVE-2019-1010180"
                  '(("gdb" _)))                   ;any version
   (vulnerability "CVE-2019-1010204"
                  '(("binutils" (and (>= "2.21") (<= "2.31.1")))
                    ("binutils_gold" (and (>= "1.11") (<= "1.16")))))
   ;; CVE-2019-18192 has no associated configurations.
   ))


(test-begin "cve")

(test-equal "json->cve-items"
  '("CVE-2019-0001"
    "CVE-2019-0005"
    "CVE-2019-14811"
    "CVE-2019-17365"
    "CVE-2019-1010180"
    "CVE-2019-1010204"
    "CVE-2019-18192")
  (map (compose cve-id cve-item-cve)
       (call-with-input-file %sample json->cve-items)))

(test-equal "cve-item-published-date"
  '(2019)
  (delete-duplicates
   (map (compose date-year cve-item-published-date)
        (call-with-input-file %sample json->cve-items))))

(test-equal "json->vulnerabilities"
  %expected-vulnerabilities
  (call-with-input-file %sample json->vulnerabilities))

(test-equal "vulnerabilities->lookup-proc"
  (list (list (third %expected-vulnerabilities))  ;ghostscript
        (list (third %expected-vulnerabilities))
        '()

        (list (fifth %expected-vulnerabilities))  ;gdb
        (list (fifth %expected-vulnerabilities))

        (list (fourth %expected-vulnerabilities)) ;nix
        '()

        (list (sixth %expected-vulnerabilities))  ;binutils
        '()
        (list (sixth %expected-vulnerabilities))
        '())
  (let* ((vulns  (call-with-input-file %sample json->vulnerabilities))
         (lookup (vulnerabilities->lookup-proc vulns)))
    (list (lookup "ghostscript")
          (lookup "ghostscript" "9.27")
          (lookup "ghostscript" "9.28")
          (lookup "gdb")
          (lookup "gdb" "42.0")
          (lookup "nix")
          (lookup "nix" "2.4")
          (lookup "binutils" "2.31.1")
          (lookup "binutils" "2.10")
          (lookup "binutils_gold" "1.11")
          (lookup "binutils" "2.32"))))

(test-end "cve")
