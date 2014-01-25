;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages aspell)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages which))

(define-public aspell
  (package
    (name "aspell")
    (version "0.60.6.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/aspell/aspell-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1qgn5psfyhbrnap275xjfrzppf5a83fb67gpql0kfqv37al869gm"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (home-page "http://aspell.net/")
    (synopsis "Spell checker")
    (description
     "Aspell is a spell-checker which can be used either as a library or as
a standalone program.  Notable features of Aspell include its full support of
documents written in the UTF-8 encoding and its ability to use multiple
dictionaries, including personal ones.")
    (license lgpl2.1+)))


;;;
;;; Dictionaries.
;;;
;;; Use 'export ASPELL_CONF="dict-dir $HOME/.guix-profile/lib/aspell"' to use them.
;;;

(define* (aspell-dictionary dict-name full-name
                            #:key version sha256 (prefix "aspell6-"))
  (package
    (name (string-append "aspell-dict-" dict-name))
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/aspell/dict/" dict-name
                                  "/" prefix dict-name "-"
                                  version ".tar.bz2"))
              (sha256 sha256)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"))))
                 %standard-phases)
                #:make-flags (let ((out (assoc-ref %outputs "out")))
                               (list (string-append "dictdir=" out "/lib/aspell")
                                     (string-append "datadir=" out "/lib/aspell")))
                #:tests? #f))
    (native-inputs `(("aspell" ,aspell)
                     ("which" ,which)))
    (synopsis (string-append full-name " dictionary for GNU Aspell")) ; XXX: i18n
    (description
     "This package provides a dictionary for the GNU Aspell spell checker.")
    (license gpl2+)
    (home-page "http://aspell.net/")))


(define-public aspell-dict-en
  (aspell-dictionary "en" "English"
                     #:version "7.1-0"
                     #:sha256
                     (base32
                      "02ldfiny4iakgfgy4sdrzjqdzi7l1rmb6y30lv31kfy5x31g77gz")))

(define-public aspell-dict-eo
  (aspell-dictionary "eo" "Esperanto"
                     #:version "2.1.20000225a-2"
                     #:sha256
                     (base32
                      "09vf0mbiicbmyb4bwb7v7lgpabnylg0wy7m3hlhl5rjdda6x3lj1")))

(define-public aspell-dict-es
  (aspell-dictionary "es" "Spanish"
                     #:version "1.11-2"
                     #:sha256
                     (base32
                      "1k5g328ac1hdpp6fsg57d8md6i0aqcwlszp3gbmp5706wyhpydmd")))

(define-public aspell-dict-fr
  (aspell-dictionary "fr" "French"
                     #:version "0.50-3"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "14ffy9mn5jqqpp437kannc3559bfdrpk7r36ljkzjalxa53i0hpr")))
