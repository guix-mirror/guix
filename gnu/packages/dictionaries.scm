;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017, 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages dictionaries)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xml))


(define-public vera
  (package
    (name "vera")
    (version "1.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/vera/vera-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1az0v563jja8xb4896jyr8yv7jd9zacqyfkjd7psb73v7clg1mzz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((out    (assoc-ref %outputs "out"))
                          (info   (string-append out "/share/info"))
                          (html   (string-append out "/share/html"))
                          (source (assoc-ref %build-inputs "source"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (gz     (assoc-ref %build-inputs "gzip"))
                          (texi   (assoc-ref %build-inputs "texinfo")))
                     (setenv "PATH" (string-append gz "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source)

                     (chdir (string-append "vera-" ,version))
                     (mkdir-p info)
                     (mkdir-p html)

                     ;; XXX: Use '--force' because the document is unhappy
                     ;; with Texinfo 5 (yes, documents can be unhappy.)
                     (invoke (string-append texi "/bin/makeinfo")
                             "vera.texi" "--force" "-o"
                             (string-append info "/vera.info"))
                     (invoke (string-append texi "/bin/makeinfo")
                             "vera.texi" "--force" "--html" "-o"
                             (string-append html "/vera.html"))))
      #:modules ((guix build utils))))
    (native-inputs `(("texinfo" ,texinfo)
                     ("tar" ,tar)
                     ("gzip" ,gzip)))
    (home-page "https://savannah.gnu.org/projects/vera/")
    (synopsis "List of acronyms")
    (description
     "V.E.R.A. (Virtual Entity of Relevant Acronyms) is a list of computing
acronyms distributed as an info document.")
    (license license:fdl1.3+)))

(define-public gcide
  (package
    (name "gcide")
    (version "0.52")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/gcide/gcide-" version ".tar.xz"))
              (sha256
               (base32
                "1n3bp91sik66z3ca7mjqbr9nck3hg5ck0c8g84xc0qnfpx5vznh2"))))
    (build-system trivial-build-system)
    (arguments
     '(#:builder (begin
                   (use-modules (guix build utils))
                   (let* ((src     (assoc-ref %build-inputs "source"))
                          (tar     (assoc-ref %build-inputs "tar"))
                          (xz      (assoc-ref %build-inputs "xz"))
                          (out     (assoc-ref %outputs "out"))
                          (datadir (string-append out "/share/gcide")))
                     (set-path-environment-variable "PATH" '("bin")
                                                    (list tar xz))
                     (mkdir-p datadir)
                     (invoke "tar" "-C" datadir
                             "--strip-components=1"
                             "-xvf" src)))
       #:modules ((guix build utils))))
    (native-inputs
     `(("tar" ,tar)
       ("xz" ,xz)))
    (synopsis "GNU Collaborative International Dictionary of English")
    (description
     "GCIDE is a free dictionary based on a combination of sources.  It can
be used via the GNU Dico program or accessed online at
http://gcide.gnu.org.ua/")
    (home-page "http://gcide.gnu.org.ua/")
    (license license:gpl3+)))

(define-public diction
  ;; Not quite a dictionary, not quite a spell checker either…
  (package
    (name "diction")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/diction/diction-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xi4l1x1vvzmzmbhpx0ghmfnwwrhabjwizrpyylmy3fzinzz3him"))))
    (build-system gnu-build-system)
    (synopsis "Identifies wordy and commonly misused phrases")
    (description
     "A package providing two classic Unix commands, style and diction.
Diction is used to identify wordy and commonly misused phrases in a
body of text.  Style instead analyzes surface aspects of a written
work, such as sentence length and other readability measures.")
    (home-page "https://www.gnu.org/software/diction/")
    (license license:gpl3+)))

(define-public ding
  (package
    (name "ding")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.tu-chemnitz.de/pub/Local/urz/" name
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0chjqs3z9zs1w3l7b5lsaj682rgnkf9kibcbzhggqqcn1pbvl5sq"))))
    (build-system gnu-build-system)
    (inputs `(("tk" ,tk)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda _
             (let ((bindir (string-append
                            (assoc-ref %outputs "out") "/bin"))
                   (wish (string-append
                          (assoc-ref %build-inputs "tk")
                          "/bin/wish8.6"))
                   (sharedir (string-append
                              (assoc-ref %outputs "out")
                              "/share/applications"))
                   (libdir (string-append
                            (assoc-ref %outputs "out") "/lib")))
               (mkdir-p bindir)
               (mkdir-p libdir)
               (mkdir-p sharedir)

               (substitute* "ding.desktop"
                 (("Exec=/usr/bin/ding")
                  (string-append "Exec=" bindir "/ding")))
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* "ding" (("exec wish") (string-append "exec " wish))))
               (substitute* "install.sh"
                 (("/bin/cp") "cp")
                 (("/bin/mv") "mv")
                 (("NEEDPROG=\"wish\"")
                  (string-append "NEEDPROG=\"" wish "\""))
                 (("DEFBINDIR=\"/usr/local/bin\"")
                  (string-append "DEFBINDIR=\"" bindir "\""))
                 (("DEFLIBDIR=\"/usr/local/lib\"")
                  (string-append "DEFLIBDIR=\"" libdir "\"")))
               (install-file "ding.desktop" sharedir)
               (install-file "ding.png" sharedir)
               (invoke "./install.sh")))))))
    (synopsis "Dictionary lookup program with a German-English dictionary")
    (description "Ding is a dictionary lookup program for the X window system.
It comes with a German-English dictionary with approximately 270,000 entries.")
    (home-page  "http://www-user.tu-chemnitz.de/~fri/ding/")
    (license license:gpl2+)))

(define-public grammalecte
  (package
    (name "grammalecte")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://grammalecte.net/grammalecte/zip/"
                           "Grammalecte-fr-v" version ".zip"))
       (sha256
        (base32
         "0p005mqgmckkbif3syv3d90rr1zsm5n5vvac5b29nn8g4v8jjpfs"))))
    (build-system python-build-system)
    (home-page "https://grammalecte.net")
    (synopsis  "French spelling and grammar checker")
    (description "Grammalecte is a grammar checker dedicated to the French
language, derived from Lightproof.

Grammalecte aims at helping to write a proper French without distracting users
with false positives.  This grammar checker follows the principle: the less
false positives, the better; if it cannot know with a good chance if
a dubious expression is wrong, it will keep silent.

The package provides the command line interface, along with a server
and a Python library.")
    (license license:gpl3+)))

(define-public translate-shell
  (package
    (name "translate-shell")
    (version "0.9.6.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url"https://github.com/soimort/translate-shell.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1dmh3flldfhnqfay3a6c5hanqcjwrmbly1bq8mlk022qfi1fv33y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'remove-unnecessary-file
           ;; This file gets generated during the build phase.
           (lambda _
             (delete-file "translate")
             #t))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin/trans"))
                    (curl    (assoc-ref inputs "curl"))
                    (fribidi (assoc-ref inputs "fribidi"))
                    (rlwrap  (assoc-ref inputs "rlwrap")))
               (wrap-program bin
                             `("PATH" ":" prefix
                               (,(string-append out "/bin:"
                                                curl "/bin:"
                                                fribidi "/bin:"
                                                rlwrap "/bin")))))
             #t))
         (add-after 'install 'emacs-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (dest  (string-append out "/share/emacs/site-lisp/guix.d/"
                                          ,name "-" ,version))
                    (emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs")))
               (install-file "google-translate-mode.el" dest)
               (emacs-generate-autoloads ,name dest)))))
       #:make-flags (list (string-append "PREFIX=" %output)
                          "NETWORK_ACCESS=no test")
       #:imported-modules (,@%gnu-build-system-modules (guix build emacs-utils))
       #:modules ((guix build gnu-build-system)
                  (guix build emacs-utils)
                  (guix build utils))
       #:test-target "test"))
    (inputs
     `(("curl" ,curl)
       ("fribidi" ,fribidi)
       ("rlwrap" ,rlwrap)))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("util-linux" ,util-linux)))     ; hexdump, for the test
    (home-page "https://www.soimort.org/translate-shell/")
    (synopsis "Translations from the command line")
    (description
     "Translate Shell (formerly Google Translate CLI) is a command-line
translator powered by Google Translate (default), Bing Translator,
Yandex.Translate and Apertium.  It gives you easy access to one of these
translation engines from your terminal.")
    (license license:public-domain)))

(define-public lttoolbox
  (package
    (name "lttoolbox")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/apertium/lttoolbox/releases/download/v"
             version "/lttoolbox-" version ".tar.gz"))
       (sha256
        (base32
         "08y6pf1hl7prwygy1g8h6ndqww18pmb9f3r5988q0pcrp8w6xz6b"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxml2" ,libxml2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://wiki.apertium.org/wiki/Lttoolbox")
    (synopsis "Lexical processing toolbox")
    (description "Lttoolbox is a toolbox for lexical processing, morphological
analysis and generation of words.  Analysis is the process of splitting a
word (e.g. cats) into its lemma \"cat\" and the grammatical information
@code{<n><pl>}.  Generation is the opposite process.")
    (license (list license:gpl2 ; main license
                   license:expat)))) ; utf8/*

(define-public apertium
  (package
    (name "apertium")
    (version "3.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/apertium/apertium/releases/download/v"
             version "/apertium-" version ".tar.gz"))
       (sha256
        (base32
         "0lrx58ipx2kzh1pd3xm1viz05dqyrq38jbnj9dnk92c9ckkwkp4h"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("lttoolbox" ,lttoolbox)
       ("pcre" ,pcre)))
    (native-inputs
     `(("apertium-get"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/apertium/apertium-get")
                 (commit "692d030e68008fc123089cf2446070fe8c6e3a3b")))
           (sha256
            (base32
             "0kgp68azvds7yjwfz57z8sa5094fyk5yr0qxzblrw7bisrrihnav"))))
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ;; python is only required for running the test suite
       ("python" ,python)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; If apertium-get does not exist in the source tree, the build tries
         ;; to download it using an svn checkout. To avoid this, copy
         ;; apertium-get into the source tree.
         (add-after 'unpack 'unpack-apertium-get
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "apertium-get")
                               "apertium/apertium-get")
             #t)))))
    (home-page "https://www.apertium.org/")
    (synopsis "Rule based machine translation system")
    (description "Apertium is a rule based machine translation system
featuring a shallow-transfer machine translation engine.  The design of the
system makes translations fast (translating tens of thousands of words per
second on ordinary desktop computers) and, in spite of the errors, reasonably
intelligible and easily correctable.")
    (license (list license:gpl2 ; main license
                   license:expat)))) ; utf8/*
