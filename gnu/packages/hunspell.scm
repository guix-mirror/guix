;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu packages hunspell)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (gnu packages libreoffice))

(define* (hunspell-dictionary dict-name full-name #:key synopsis home-page license)
  (package
    (name (string-append
           "hunspell-dict-"
           ;; Downcase and replace underscore in package names
           ;; to follow Guix naming conventions.
           (string-map (match-lambda
                         (#\_ #\-)
                         (chr chr))
                       (string-downcase dict-name))))
    (version (package-version libreoffice))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://anongit.freedesktop.org/git/"
                                 "libreoffice/dictionaries.git/"))
             (commit
              (string-append "libreoffice-" version))))
       (file-name (git-file-name "libreoffice-dictionaries" version))
       (sha256
        (base32 "0h1sz8haqwpis4af1vy7jvivl4rr9g53l4l680qa7yn0691gkiv3"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((dictionary
                           (string-append (assoc-ref %build-inputs "source")
                                          "/" ,dict-name
                                          "/" ,dict-name))
                          (hunspell (string-append %output "/share/hunspell/"))
                          (myspell (string-append %output "/share/myspell")))
                     (for-each
                      (lambda (ext)
                        (install-file (string-append dictionary ext)
                                      hunspell))
                      '(".aff" ".dic"))
                     (symlink hunspell myspell)
                     #t))))
    (synopsis synopsis)
    (description "This package provides a dictionary for the Hunspell
spell-checking library.")
    (license license)
    (home-page home-page)))

(define-public hunspell-dict-it-it
  (let ((synopsis identity))
    (hunspell-dictionary "it_IT" "Italian"
                         #:synopsis (synopsis "Hunspell dictionary for Italian")
                         #:home-page "https://www.libreitalia.org/"
                         #:license license:gpl3)))
