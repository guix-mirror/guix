;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages lsof)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl))

(define-public lsof
  (package
   (name "lsof")
   (version "4.89")
   (source
    (origin
      (method url-fetch)
      (uri
       (apply append
              (map
               (lambda (mirror-uri)
                 (let ((tarball (string-append name "_" version ".tar.bz2")))
                   (list
                    (string-append mirror-uri "/" tarball)
                    ;; Upon every new release, the previous one is moved here:
                    (string-append mirror-uri "/OLD/" tarball))))
               (list
                "ftp://lsof.itap.purdue.edu/pub/tools/unix/lsof/"

                ;; Add mirrors because the canonical FTP server at purdue.edu
                ;; bails out when it cannot do a reverse DNS lookup, as noted
                ;; at <http://people.freebsd.org/~abe/>.
                "ftp://ftp.fu-berlin.de/pub/unix/tools/lsof/"
                (string-append "http://www.mirrorservice.org/sites/"
                               "lsof.itap.purdue.edu/pub/tools/unix/lsof")
                (string-append "ftp://ftp.mirrorservice.org/sites/"
                               "lsof.itap.purdue.edu/pub/tools/unix/lsof")))))
      (sha256
       (base32 "061p18v0mhzq517791xkjs8a5dfynq1418a1mwxpji69zp2jzb41"))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))
   (arguments
    `(#:tests? #f ; no test target
      #:phases
      (modify-phases %standard-phases
        (replace 'unpack
          (lambda* (#:key source #:allow-other-keys)
            (let ((unpack (assoc-ref %standard-phases 'unpack)))
              (unpack #:source source)
              (unpack #:source (car (find-files "." "\\.tar$"))))))
        (replace 'configure
          (lambda _
            (setenv "LSOF_CC" "gcc")
            (setenv "LSOF_MAKE" "make")
            (invoke "./Configure" "linux")
            #t))
        (add-after 'configure 'patch-timestamps
          (lambda _
            (substitute* "Makefile"
              (("`date`") "`date --date=@1`"))))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (install-file "lsof" (string-append out "/bin"))
              (install-file "lsof.8" (string-append out "/share/man/man8")))
            #t)))))
   (synopsis "Display information about open files")
   (description
    "Lsof stands for LiSt Open Files, and it does just that.
It lists information about files that are open by the processes running
on the system.")
   (license (license:fsf-free
             "file://00FAQ"
             "License inspired by zlib, see point 1.9 of 00FAQ in the distribution."))
   (home-page "http://people.freebsd.org/~abe/")))
