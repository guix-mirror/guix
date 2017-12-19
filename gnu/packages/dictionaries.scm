;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tcl))


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
                     (system* (string-append tar "/bin/tar") "xvf" source)

                     (chdir (string-append "vera-" ,version))
                     (mkdir-p info)
                     (mkdir-p html)

                     ;; XXX: Use '--force' because the document is unhappy
                     ;; with Texinfo 5 (yes, documents can be unhappy.)
                     (and (zero?
                           (system* (string-append texi "/bin/makeinfo")
                                    "vera.texi" "--force" "-o"
                                    (string-append info "/vera.info")))
                          (zero?
                           (system* (string-append texi "/bin/makeinfo")
                                    "vera.texi" "--force" "--html" "-o"
                                    (string-append html "/vera.html"))))))
      #:modules ((guix build utils))))
    (native-inputs `(("texinfo" ,texinfo)
                     ("tar" ,tar)
                     ("gzip" ,gzip)))
    (home-page "https://savannah.gnu.org/projects/vera/")
    (synopsis "List of acronyms")
    (description
     "V.E.R.A. (Virtual Entity of Relevant Acronyms) is a list of computing
acronyms distributed as an info document.")
    (license fdl1.3+)))

(define-public gcide
  (package
    (name "gcide")
    (version "0.51")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/gcide/gcide-" version ".tar.xz"))
              (sha256
               (base32
                "1wm0s51ygc6480dq8gwahzr35ls8jgpf34yiwl5yqcaa0i19fdv7"))))
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
                     (zero? (system* "tar" "-C" datadir
                                     "--strip-components=1"
                                     "-xvf" src))))
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
    (license gpl3+)))

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
    (license gpl3+)))

(define-public ding
  (package
    (name "ding")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.tu-chemnitz.de/pub/Local/urz/" name
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "00z97ndwmzsgig9q6y98y8nbxy76pyi9qyj5qfpbbck24gakpz5l"))))
    (build-system gnu-build-system)
    (inputs `(("tk" ,tk)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace
             'install
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
               (zero?
                (system* "./install.sh"))))))))
    (synopsis "Dictionary lookup program with a German-English dictionary")
    (description "Ding is a dictionary lookup program for the X window system.
It comes with a German-English dictionary with approximately 270,000 entries.")
    (home-page  "http://www-user.tu-chemnitz.de/~fri/ding/")
    (license gpl2+)))

(define-public grammalecte
  (package
    (name "grammalecte")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://www.dicollecte.org/grammalecte/zip/"
                           "Grammalecte-fr-v" version ".zip"))
       (sha256
        (base32
         "0bl342i7nqbg8swk3fxashg9liyp3jdnix59pndhy41cpm1xln4i"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-setup.py
           ;; FIXME: "setup.py" contains a typo in 0.6.1 release. The
           ;; issue was reported and fixed upstream
           ;; (https://dicollecte.org/thread.php?prj=fr&t=674). This
           ;; phase can be removed in next release.
           (lambda _
             (substitute* "setup.py"
               (("server_options\\.") "grammalecte-server-options."))
             #t)))))
    (home-page "https://www.dicollecte.org")
    (synopsis  "French spelling and grammar checker")
    (description "Grammalecte is a grammar checker dedicated to the French
language, derived from Lightproof.

Grammalecte aims at helping to write a proper French without distracting users
with false positives.  This grammar checker follows the principle: the less
false positives, the better; if it cannot know with a good chance if
a dubious expression is wrong, it will keep silent.

The package provides the command line interface, along with a server
and a Python library.")
    (license gpl3+)))

(define-public translate-shell
  (package
    (name "translate-shell")
    (version "0.9.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/soimort/" name "/archive/v"
                            version ".tar.gz"))
        (sha256
         (base32
          "1fg6nf1plvgimc57fsdr9rcjbf7jvmk5jrlj5ya509vpdcdgvj2s"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure phase
         (add-after 'install 'emacs-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (dest  (string-append out "/share/emacs/site-lisp"))
                    (emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs")))
               (install-file "google-translate-mode.el" dest)
               (emacs-generate-autoloads ,name dest)))))
       #:make-flags (list (string-append "PREFIX=" %output))
       #:imported-modules (,@%gnu-build-system-modules (guix build emacs-utils))
       #:modules ((guix build gnu-build-system)
                  (guix build emacs-utils)
                  (guix build utils))
       #:test-target "test"))
    (propagated-inputs
     `(("curl" ,curl)
       ("fribidi" ,fribidi)
       ("rlwrap" ,rlwrap)))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("util-linux" ,util-linux))) ; hexdump, for the test
    (home-page "https://www.soimort.org/translate-shell")
    (synopsis "Translations from the command line")
    (description
     "Translate Shell (formerly Google Translate CLI) is a command-line
translator powered by Google Translate (default), Bing Translator,
Yandex.Translate and Apertium.  It gives you easy access to one of these
translation engines from your terminal.")
    (license public-domain)))
