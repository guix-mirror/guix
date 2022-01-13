;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2019, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christopher Andersson <christopher@8bits.nu>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2016, 2017, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Marcin Karpezo <sirmacik@wioo.waw.pl>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Noah Landis <noahlandis@posteo.net>
;;; Copyright © 2021 Sergiu Ivanov <sivanov@colimite.fr>
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
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (ice-9 match))

(define-public aspell
  (package
    (name "aspell")
    (version "0.60.8")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/aspell/aspell-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1wi60ankalmh8ds7nplz434jd7j94gdvbahdwsr539rlad8pxdzr"))
      (patches (search-patches "aspell-default-dict-dir.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-filter-path
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Change the default value of 'filter-path' so that filters such
             ;; as 'tex-filter.so' can be found.  By default none of the
             ;; filters would be found.
             (let* ((out    (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib/aspell-"
                                           ,(version-major+minor version))))
               (substitute* "common/config.cpp"
                 (("\"filter-path(.*)DICT_DIR" _ middle)
                  (string-append "\"filter-path" middle
                                 "\"" libdir "\"")))
               #t))))))
    (inputs (list perl))

    (native-search-paths
     ;; This is a Guix-specific environment variable that takes a single
     ;; entry, not an actual search path.
     (list (search-path-specification
            (variable "ASPELL_DICT_DIR")
            (separator #f)
            (files '("lib/aspell")))))

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
;;; Use 'export ASPELL_CONF="dict-dir $HOME/.guix-profile/lib/aspell"' to use
;;; them, or set the Guix-specific 'ASPELL_DICT_DIR', or just do nothing (as
;;; long as 'HOME' is set, that's fine!).
;;;

(define* (aspell-dictionary dict-name full-name
                            #:key version sha256 (prefix "aspell6-"))
  (package
    (name (string-append
           "aspell-dict-"
           ;; Downcase and replace underscore in package names
           ;; to follow Guix naming conventions.
           (string-map (match-lambda
                         (#\_ #\-)
                         (chr chr))
           (string-downcase dict-name))))
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/aspell/dict/" dict-name
                                  "/" prefix dict-name "-"
                                  version ".tar.bz2"))
              (hash (content-hash sha256))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure")))))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "dictdir=" out "/lib/aspell")
               (string-append "datadir=" out "/lib/aspell")))
       #:tests? #f))
    (native-inputs (list aspell which))
    (synopsis (string-append full-name " dictionary for GNU Aspell")) ; XXX: i18n
    (description
     "This package provides a dictionary for the GNU Aspell spell checker.")
    (license gpl2+)
    (properties
      ;; Unfortunately any versions with a trailing 'dash and digit' (eg.: '-0')
      ;; will fail to register as a version.
      `((upstream-name . ,(string-append prefix dict-name))
        (ftp-directory . ,(string-append "/aspell/dict/" dict-name))))
    (home-page "http://aspell.net/")))


(define-public aspell-dict-ar
  (aspell-dictionary "ar" "Arabic"
                     #:version "1.2-0"
		     #:prefix "aspell6-"
                     #:sha256
                     (base32
                      "1avw40bp8yi5bnkq64ihm2rldgw34lk89yz281q9bmndh95a47h4")))

(define-public aspell-dict-be
  (aspell-dictionary "be" "Belarusian"
                     #:version "0.01"
                     #:prefix "aspell5-"
                     #:sha256
                     (base32
                      "1svls9p7rsfi3hs0afh0cssj006qb4v1ik2yzqgj8hm10c6as2sm")))

(define-public aspell-dict-ca
  (let ((version "2.5.0")
        (sha256
         (base32 "0kbi8fi7a1bys31kfqrlh332gyik0cfdmxgl7n15sa9c305rkgwq")))
    (package
      (inherit (aspell-dictionary "ca" "Catalan"
                                  #:version version
                                  #:sha256 sha256))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.softcatala.org/pub/softcatala/aspell/"
                             version "/aspell6-ca-" version ".tar.bz2"))
         (hash (content-hash sha256))))
      (home-page "https://www.softcatala.org/pub/softcatala/aspell/"))))

(define-public aspell-dict-cs
  (aspell-dictionary "cs" "Czech"
                     #:version "20040614-1"
                     #:sha256
                     (base32
                      "0rihj4hsw96pd9casvmpvw3r8040pfa28p1h73x4vyn20zwr3h01")))

(define-public aspell-dict-de
  (aspell-dictionary "de" "German"
                     #:version "20161207-7-0"
                     #:sha256
                     (base32
                      "0wamclvp66xfmv5wff96v6gdlnfv4y8lx3f8wvxyzm5imwgms4n2")))

(define-public aspell-dict-da
  (aspell-dictionary "da" "Danish"
                     #:version "1.6.36-11-0"
                     #:sha256
                     (base32
                      "1xz2haayvwlxgss9jf1x2311a1ixbk75q2vgfprjhibsmb7cpinv")))

(define-public aspell-dict-el
  (aspell-dictionary "el" "Greek"
                     #:version "0.08-0"
                     #:prefix "aspell6-"
                     #:sha256
                     (base32
                      "1ljcc30zg2v2h3w5h5jr5im41mw8jbsgvvhdd2cii2yzi8d0zxja")))

(define-public aspell-dict-en
  (aspell-dictionary "en" "English"
                     #:version "2020.12.07-0"
                     #:sha256
                     (base32
                      "1cwzqkm8gr1w51rpckwlvb43sb0b5nbwy7s8ns5vi250515773sc")))

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

(define-public aspell-dict-fi
  (aspell-dictionary "fi" "Finnish"
                     #:version "0.7-0"
                     #:prefix "aspell6-"
                     #:sha256
                     (base32
                      "07d5s08ba4dd89cmwy9icc01i6fjdykxlb9ravmhdrhi8mxz1mzq")))

(define-public aspell-dict-fr
  (aspell-dictionary "fr" "French"
                     #:version "0.50-3"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "14ffy9mn5jqqpp437kannc3559bfdrpk7r36ljkzjalxa53i0hpr")))

(define-public aspell-dict-grc
  (aspell-dictionary "grc" "Ancient Greek"
                     #:version "0.02-0"
                     #:sha256
                     (base32
                      "1zxr8958v37v260fkqd4pg37ns5h5kyqm54hn1hg70wq5cz8h512")))

(define-public aspell-dict-he
  (aspell-dictionary "he" "Hebrew"
                     #:version "1.0-0"
                     #:sha256
                     (base32
                      "13bhbghx5b8g0119g3wxd4n8mlf707y41vlf59irxjj0kynankfn")))

(define-public aspell-dict-hi
  (aspell-dictionary "hi" "Hindi"
                     #:version "0.02-0"
                     #:prefix "aspell6-"
                     #:sha256
                     (base32
                      "0drs374qz4419zx1lf2k281ydxf2750jk5ailafj1x0ncz27h1ys")))

(define-public aspell-dict-it
  (let ((version "2.4-20070901-0")
        (sha256
         (base32 "0d6ypii3jblprpibazb6ypady536jz62rwxlss1x1raq07rhvvqn")))
    (package
      (inherit (aspell-dictionary "it" "Italian"
                                  #:version version
                                  #:sha256 sha256))

      ;; The version hosted at <https://ftp.gnu.org/gnu/aspell/dict> is even
      ;; more out of date.
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://sourceforge/linguistico/"
                             "Dizionario%20italiano%20per%20Aspell/" version "/"
                             "aspell6-it-" version ".tar.bz2"))
         (hash (content-hash sha256))))
       (home-page
        "http://linguistico.sourceforge.net/pages/dizionario_italiano.html"))))

(define-public aspell-dict-mi
  (aspell-dictionary "mi" "Maori"
                     #:version "0.50-0"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "12bxplpd348yx8d2q8qvahi9dlp7qf28qmanzhziwc7np8rixvmy")))

(define-public aspell-dict-nl
  (aspell-dictionary "nl" "Dutch"
                     #:version "0.50-2"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "0ffb87yjsh211hllpc4b9khqqrblial4pzi1h9r3v465z1yhn3j4")))

(define-public aspell-dict-nn
  (aspell-dictionary "nn" "Norwegian Nynorsk"
                     #:version "0.50.1-1"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "0w2k5l5rbqpliripgqwiqixz5ghnjf7i9ggbrc4ly4vy1ia10rmc")))

(define-public aspell-dict-pl
  (aspell-dictionary "pl" "Polish"
                     #:version "0.51-0"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "1a3ccji6k5gys7l3ilr2lh5pzxgzb7ipc5vb737svl6nqgdy8757")))

(define-public aspell-dict-pt-br
  (aspell-dictionary "pt_BR" "Brazilian Portuguese"
                     #:version "20131030-12-0"
                     #:sha256
                     (base32
                      "1xqlpk21s93c6blkdnpk7l62q9fxjvzdv2x86chl8p2x1gdrj3gb")))

(define-public aspell-dict-pt-pt
  (aspell-dictionary "pt_PT" "Portuguese"
                     #:version "20190329-1-0"
                     #:sha256
                     (base32
                      "0ld0d0ily4jqifjfsxfv4shbicz6ymm2gk56fq9gbzra1j4qnw75")))

(define-public aspell-dict-ru
  (aspell-dictionary "ru" "Russian"
                     #:version "0.99f7-1"
                     #:sha256
                     (base32
                      "0ip6nq43hcr7vvzbv4lwwmlwgfa60hrhsldh9xy3zg2prv6bcaaw")))

(define-public aspell-dict-sl
  (aspell-dictionary "sl" "Slovenian"
                     #:version "0.50-0"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "1l9kc5g35flq8kw9jhn2n0bjb4sipjs4qkqzgggs438kywkx2rp5")))

(define-public aspell-dict-sv
  (aspell-dictionary "sv" "Swedish"
                     #:version "0.51-0"
                     #:prefix "aspell-"
                     #:sha256
                     (base32
                      "02jwkjhr32kvyibnyzgx3smbnm576jwdzg3avdf6zxwckhy5fw4v")))

(define-public aspell-dict-uk
  (aspell-dictionary "uk" "Ukrainian"
                     #:version "1.4.0-0"
                     #:sha256
                     (base32
                      "137i4njvnslab6l4s291s11xijr5jsy75lbdph32f9y183lagy9m")))

(define-public aspell-dict-ro
  (aspell-dictionary "ro" "Romanian"
                     #:version "3.3-2"
                     #:prefix "aspell5-"
                     #:sha256
                     (base32
                      "0gb8j9iy1acdl11jq76idgc2lbc1rq3w04favn8cyh55d1v8phsk")))


;;;
;;; Hunspell packages made from the Aspell word lists.
;;;

(define* (aspell-word-list language synopsis
                           #:optional
                           (nick (string-map (lambda (chr)
                                               (if (char=? #\_ chr)
                                                   #\-
                                                   chr))
                                             (string-downcase language))))
  (package
    (name (string-append "hunspell-dict-" nick))
    (version "2018.04.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/wordlist/SCOWL/"
                    version "/scowl-" version ".tar.gz"))
              (sha256
               (base32
                "11lkrnhwrf5mvrrq45k4mads3n9aswgac8dc25ba61c75alxb5rs"))))
    (native-inputs
     (list tar gzip perl aspell))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "speller/README_en.txt.sh"
               (("\\bdate\\b") ""))))
         (delete 'configure)
         (delete 'check)
         (replace 'build
           (lambda _
             (substitute* "speller/make-hunspell-dict"
               (("zip -9 .*$")
                "return\n"))
             (mkdir "speller/hunspell")

             ;; XXX: This actually builds all the dictionary variants.
             (invoke "make" "-C" "speller" "hunspell")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref %outputs "out"))
                    (hunspell (string-append out "/share/hunspell"))
                    (myspell  (string-append out "/share/myspell"))
                    (doc      (string-append out "/share/doc/"
                                             ,name))
                    (dot-dic  ,(string-append "speller/" language ".dic")))
               (mkdir-p myspell)

               ;; Usually there's only a 'LANGUAGE.dic' file, but for the "en"
               ;; dictionary, there no 'en.dic'.  Instead, there's a set of
               ;; 'en*.dic' files, hence the 'find-files' call below.
               (if (file-exists? dot-dic)
                   (install-file dot-dic hunspell)
                   (for-each (lambda (dic)
                               (install-file dic hunspell))
                             (find-files "speller"
                                         ,(string-append language ".*\\.dic$"))))

               ;; Install affix files corresponding to installed dictionaries
               (for-each (lambda (dic)
                           (install-file (string-append
                                           "speller/" (basename dic ".dic") ".aff")
                                         hunspell))
                         (find-files hunspell ".*\\.dic$"))
               (symlink hunspell (string-append myspell "/dicts"))
               (for-each (lambda (file)
                           (install-file file doc))
                         (find-files "."
                                     "^(Copyright|.*\\.(txt|org|md))$"))
               #t))))))
    (synopsis synopsis)
    (description
     "This package provides a dictionary for the Hunspell spell-checking
library.")
    (home-page "http://wordlist.aspell.net/")
    (license (non-copyleft "file://Copyright"
                           "Word lists come from several sources, all
under permissive licensing terms.  See the 'Copyright' file."))))

(define-syntax define-word-list-dictionary
  (syntax-rules (synopsis)
    ((_ name language (synopsis text))
     (define-public name
       (aspell-word-list language text)))
    ((_ name language nick (synopsis text))
     (define-public name
       (aspell-word-list language text nick)))))

(define-word-list-dictionary hunspell-dict-en
  "en"
  (synopsis "Hunspell dictionary for English"))

(define-word-list-dictionary hunspell-dict-en-au
  "en_AU"
  (synopsis "Hunspell dictionary for Australian English"))

(define-word-list-dictionary hunspell-dict-en-ca
  "en_CA"
  (synopsis "Hunspell dictionary for Canadian English"))

(define-word-list-dictionary hunspell-dict-en-gb
  "en_GB-ise" "en-gb"
  (synopsis "Hunspell dictionary for British English, with -ise endings"))

(define-word-list-dictionary hunspell-dict-en-gb-ize
  "en_GB-ize"
  (synopsis "Hunspell dictionary for British English, with -ize endings"))

(define-word-list-dictionary hunspell-dict-en-us
  "en_US"
  (synopsis "Hunspell dictionary for United States English"))

(define-public ispell
  (package
    (name "ispell")
    (version "3.4.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cs.hmc.edu/~geoff/tars/ispell-"
                           version ".tar.gz"))
       (sha256
        (base32 "0gp1rwn8grkvz28wgisc2j9w9svldnaiahl3lyis118xabqddg47"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Based on local.h.linux
             (let* ((grep (assoc-ref inputs "grep"))
                    (out (assoc-ref outputs "out")))
               (call-with-output-file "local.h"
                 (lambda (port)
                   (format port "#define MINIMENU~%")
                   (format port "#define USG~%")
                   (format port "#define HAS_RENAME~%")
                   (format port "#define CC \"gcc\"~%")
                   (format port "#define POUNDBANG \"#!~a\"~%" (which "sh"))
                   (format port "#define EGREPCMD \"~a/bin/grep -Ei\"~%" grep)
                   (format port "#define BINDIR \"~a/bin\"~%" out)
                   (format port "#define LIBDIR \"~a/lib/ispell\"~%" out)
                   (format port "#define MAN1DIR \"~a/share/man/man1\"~%" out)
                   (format port "#define MAN45DIR \"~a/share/man/man5\"~%" out))))
             #t)))))
    (inputs
     (list grep ncurses))
    (native-inputs
     (list bison))
    (synopsis "Interactive spell-checking tool for Unix")
    (description "Ispell is an interactive spell-checking tool supporting many
European languages.")
    (home-page "https://www.cs.hmc.edu/~geoff/ispell.html")
    (license bsd-3)))
