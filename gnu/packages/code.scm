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

(define-module (gnu packages code)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages perl))

;;; Tools to deal with source code: metrics, cross-references, etc.

(define-public sloccount
  (package
    (name "sloccount")
    (version "2.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.dwheeler.com/sloccount/sloccount-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ayiwfjdh1946asah861ah9269s5xkc8p5fv1wnxs9znyaxs4zzs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete configure)
                  (add-before build make-dotl-files-older
                              (lambda _
                                ;; Make the '.l' files as old as the '.c'
                                ;; files to avoid triggering the rule that
                                ;; requires Flex.
                                (define ref
                                  (stat "README"))

                                (for-each (lambda (file)
                                            (set-file-time file ref))
                                          (find-files "." "\\.[chl]$"))
                                #t))
                  (add-before install make-target-directories
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let ((out (assoc-ref outputs "out")))
                                  (mkdir-p (string-append out "/bin"))
                                  (mkdir-p (string-append out
                                                          "/share/man/man1"))
                                  (mkdir-p (string-append out
                                                          "/share/doc")))))
                  (replace check
                           (lambda _
                             (setenv "HOME" (getcwd))
                             (setenv "PATH"
                                     (string-append (getcwd) ":"
                                                    (getenv "PATH")))
                             (zero? (system* "make" "test")))))

        #:make-flags (list (string-append "PREFIX="
                                          (assoc-ref %outputs "out")))))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.dwheeler.com/sloccount/")
    (synopsis "Count physical source lines of code (SLOC)")
    (description
     "SLOCCount is a set of the programs for counting source lines of
code (SLOC) in large software systems.  It can automatically identify and
measure a wide range of programming languages.  It automatically estimates the
effort, time, and money it would take to develop the software, using the
COCOMO model or user-provided parameters.")
    (license gpl2+)))
