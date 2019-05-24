;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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

(define-module (gnu packages anthy)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public anthy
  (package
    (name "anthy")
    (version "9100h")
    (source (origin
              (method url-fetch)
              ;; The URI does not appear to be easily guessable.  For
              ;; example, you cannot download version "9100g" simply
              ;; by replacing "9100h" in the URI.
              (uri "http://dl.osdn.jp/anthy/37536/anthy-9100h.tar.gz")
              (sha256
               (base32
                "0ism4zibcsa5nl77wwi12vdsfjys3waxcphn1p5s7d0qy1sz0mnj"))))
    (build-system gnu-build-system)
    ;; Anthy also contains elisp modules for using anthy within Emacs.
    ;; However, these modules are incompatible with the latest version
    ;; of Emacs.  This is because they rely on the presence of
    ;; last-command-char, which was removed in Emacs 24.3.  So, we
    ;; don't try to install them here at this time.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "test"
               (invoke "./anthy" "--all")))))))
    (home-page "http://anthy.osdn.jp/")
    (synopsis "Japanese input method")
    (description "Anthy is a Japanese input method for converting
hiragana text to mixed kana and kanji.  It is written in the C
programming language.  Anthy stores personal customizations (words it
has learned from the user's input, words the user has explicitly
added, etc.) in the ~/.anthy/ directory.  This package contains the
anthy C libraries, the cannadic and alt-cannadic kana dictionaries, as
well as command-line tools for using anthy and managing
dictionaries.")
    ;; Most of anthy is lgpl2.1+.  However, some files (e.g., from
    ;; alt-cannadic) use gpl2.  See the file "COPYING" in the anthy
    ;; source for details.
    (license (list lgpl2.1+ gpl2))))
