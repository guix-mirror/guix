;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-64))

(define %sample
  (search-path %load-path "tests/cve-sample.xml"))

(define (vulnerability id packages)
  (make-struct/no-tail (@@ (guix cve) <vulnerability>) id packages))

(define %expected-vulnerabilities
  ;; What we should get when reading %SAMPLE.
  (list
   ;; CVE-2003-0001 has no "/a" in its product list so it is omitted.
   ;; CVE-2004-0230 lists "tcp" as an application, but lacks a version number.
   (vulnerability "CVE-2008-2335" '(("phpvid" "1.2" "1.1")))
   (vulnerability "CVE-2008-3522" '(("enterprise_virtualization" "3.5")
                                    ("jasper" "1.900.1")))
   (vulnerability "CVE-2009-3301" '(("openoffice.org" "2.3.0" "2.2.1" "2.1.0")))
   ;; CVE-2015-8330 has no software list.
   ))


(test-begin "cve")

(test-equal "xml->vulnerabilities"
  %expected-vulnerabilities
  (call-with-input-file %sample xml->vulnerabilities))

(test-equal "vulnerabilities->lookup-proc"
  (list (list (first %expected-vulnerabilities))
        '()
        '()
        (list (second %expected-vulnerabilities))
        (list (third %expected-vulnerabilities)))
  (let* ((vulns  (call-with-input-file %sample xml->vulnerabilities))
         (lookup (vulnerabilities->lookup-proc vulns)))
    (list (lookup "phpvid")
          (lookup "jasper" "2.0")
          (lookup "foobar")
          (lookup "jasper" "1.900.1")
          (lookup "openoffice.org" "2.3.0"))))

(test-end "cve")
