;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages wordnet)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tcl))

(define-public wordnet
  (package
    (name "wordnet")
    (version "3.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://wordnetcode.princeton.edu/"
                                 version "/WordNet-" version ".tar.bz2"))
             (sha256
              (base32
               "08pgjvd2vvmqk3h641x63nxp7wqimb9r30889mkyfh2agc62sjbc"))
             (patches (search-patches
                        "wordnet-CVE-2008-2149.patch"
                        "wordnet-CVE-2008-3908-pt1.patch"
                        "wordnet-CVE-2008-3908-pt2.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-tcl="
                                              (assoc-ref %build-inputs "tcl")
                                              "/lib")
                               (string-append "--with-tk="
                                              (assoc-ref %build-inputs "tk")
                                              "/lib")

                               ;; Provide the `result' field in `Tcl_Interp'.
                               ;; See <https://bugs.gentoo.org/show_bug.cgi?id=452034>.
                               "CFLAGS=-DUSE_INTERP_RESULT -O2")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-libwn-PIC
           (lambda _
             ;; GNU Dico links libWN.a in its wordnet.so plugin, so it needs
             ;; PIC.
             (invoke "make" "-C" "lib" "CFLAGS=-O2 -g -fPIC"
                     "LDFLAGS=-fPIC")))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (assoc-ref outputs "tk"))
                   (tk  (assoc-ref inputs "tk"))
                   (tkv ,(let ((v (package-version tk)))
                           (string-take v (string-index-right v #\.)))))
               ;; Move `wishwn' and `wnb' to BIN.
               (for-each (lambda (prog)
                           (let ((orig (string-append out "/bin/" prog))
                                 (dst  (string-append bin "/bin/" prog))
                                 (dir  (string-append tk "/lib/tk" tkv)))
                             (mkdir-p (dirname dst))
                             (copy-file orig dst)
                             (delete-file orig)
                             (wrap-program dst
                                           `("TK_LIBRARY" "" = (,dir))
                                           `("PATH" ":" prefix
                                             (,(string-append out
                                                              "/bin"))))))
                         '("wishwn" "wnb"))
               #t))))))
    (outputs '("out"
               "tk"))                             ; for the Tcl/Tk GUI

    (inputs (list tk tcl))
    (home-page "https://wordnet.princeton.edu/")
    (synopsis "Lexical database for the English language")
    (description
     "WordNet is a large lexical database of English.  Nouns, verbs,
adjectives and adverbs are grouped into sets of cognitive synonyms (synsets),
each expressing a distinct concept.  Synsets are interlinked by means of
conceptual-semantic and lexical relations.  The resulting network of
meaningfully related words and concepts can be navigated with the browser.
WordNet is also freely and publicly available for download.  WordNet's
structure makes it a useful tool for computational linguistics and natural
language processing.")
    (license x11)))
