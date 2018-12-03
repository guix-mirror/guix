;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

(define-module (guix build java-utils)
  #:use-module (guix build utils)
  #:export (ant-build-javadoc
            install-jars
            install-javadoc))

(define* (ant-build-javadoc #:key (target "javadoc") (make-flags '())
                            #:allow-other-keys)
  (apply invoke `("ant" ,target ,@make-flags)))

(define* (install-jars jar-directory)
  "Install jar files from JAR-DIRECTORY to the default target directory.  This
is used in case the build.xml does not include an install target."
  (lambda* (#:key outputs #:allow-other-keys)
    (let ((share (string-append (assoc-ref outputs "out")
                                "/share/java")))
      (for-each (lambda (f) (install-file f share))
                (find-files jar-directory "\\.jar$"))
      #t)))

(define* (install-javadoc apidoc-directory)
  "Install the APIDOC-DIRECTORY to the target directory.  This is used to
install javadocs when this is not done by the install target."
  (lambda* (#:key outputs #:allow-other-keys)
    (let* ((out (assoc-ref outputs "out"))
           (name-version (strip-store-file-name out))
           (docs (string-append (or (assoc-ref outputs "doc") out)
                                "/share/doc/" name-version "/")))
      (mkdir-p docs)
      (copy-recursively apidoc-directory docs)
      #t)))
