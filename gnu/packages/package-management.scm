;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages package-management)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:select (gpl2+ gpl3+ lgpl2.1+))
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages man)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages bdw-gc))

(define (boot-guile-uri arch)
  "Return the URI for the bootstrap Guile tarball for ARCH."
  (if (string=? "armhf" arch)
      (string-append "http://alpha.gnu.org/gnu/guix/bootstrap/"
                     arch "-linux"
                     "/20150101/guile-2.0.11.tar.xz")
      (string-append "http://alpha.gnu.org/gnu/guix/bootstrap/"
                     arch "-linux"
                     "/20131110/guile-2.0.9.tar.xz")))

(define-public guix-0.8.2
  (package
    (name "guix")
    (version "0.8.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://alpha.gnu.org/gnu/guix/guix-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1a5gnkh17w7fgi5zy63ph64iqdvarkdqypkwgw2iifpqa6jq04zz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          "--localstatedir=/var"
                          "--sysconfdir=/etc"
                          (string-append "--with-bash-completion-dir="
                                         (assoc-ref %outputs "out")
                                         "/etc/bash_completion.d")
                          (string-append "--with-libgcrypt-prefix="
                                         (assoc-ref %build-inputs
                                                    "libgcrypt")))
       #:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'copy-bootstrap-guile
                   (lambda* (#:key system inputs #:allow-other-keys)
                     (define (boot-guile-version arch)
                       (if (string=? "armhf" arch)
                           "2.0.11"
                           "2.0.9"))

                     (define (copy arch)
                       (let ((guile  (assoc-ref inputs
                                                (string-append "boot-guile/"
                                                               arch)))
                             (target (string-append "gnu/packages/bootstrap/"
                                                    arch "-linux/"
                                                    "/guile-"
                                                    (boot-guile-version arch)
                                                    ".tar.xz")))
                         (copy-file guile target)))

                     (copy "i686")
                     (copy "x86_64")
                     (copy "mips64el")
                     (copy "armhf")
                     #t))
                  (add-after
                   'install 'wrap-program
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     ;; Make sure the 'guix' command finds GnuTLS and
                     ;; Guile-JSON automatically.
                     (let* ((out    (assoc-ref outputs "out"))
                            (json   (assoc-ref inputs "guile-json"))
                            (gnutls (assoc-ref inputs "gnutls"))
                            (path   (string-append
                                     json "/share/guile/site/2.0:"
                                     gnutls "/share/guile/site/2.0")))
                       (wrap-program (string-append out "/bin/guix")
                         `("GUILE_LOAD_PATH" ":" prefix (,path))
                         `("GUILE_LOAD_COMPILED_PATH" ":" prefix (,path)))
                       #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("emacs" ,emacs-no-x)))      ;for guix.el
    (inputs
     (let ((boot-guile (lambda (arch hash)
                         (origin
                          (method url-fetch)
                          (uri (boot-guile-uri arch))
                          (sha256 hash)))))
       `(("bzip2" ,bzip2)
         ("gzip" ,gzip)

         ("sqlite" ,sqlite)
         ("libgcrypt" ,libgcrypt)
         ("guile" ,guile-2.0)

         ("boot-guile/i686"
          ,(boot-guile "i686"
                       (base32
                        "0im800m30abgh7msh331pcbjvb4n02smz5cfzf1srv0kpx3csmxp")))
         ("boot-guile/x86_64"
          ,(boot-guile "x86_64"
                       (base32
                        "1w2p5zyrglzzniqgvyn1b55vprfzhgk8vzbzkkbdgl5248si0yq3")))
         ("boot-guile/mips64el"
          ,(boot-guile "mips64el"
                       (base32
                        "0fzp93lvi0hn54acc0fpvhc7bvl0yc853k62l958cihk03q80ilr")))
         ("boot-guile/armhf"
          ,(boot-guile "armhf"
                       (base32
                        "1mi3brl7l58aww34rawhvja84xc7l1b4hmwdmc36fp9q9mfx0lg5"))))))
    (propagated-inputs
     `(("gnutls" ,gnutls)                         ;for 'guix download' & co.
       ("guile-json" ,guile-json)
       ("geiser" ,geiser)))                       ;for guix.el

    (home-page "http://www.gnu.org/software/guix")
    (synopsis "Functional package manager for installed software packages and versions")
    (description
     "GNU Guix is a functional package manager for the GNU system, and is
also a distribution thereof.  It includes a virtual machine image.  Besides
the usual package management features, it also supports transactional
upgrades and roll-backs, per-user profiles, and much more.  It is based on
the Nix package manager.")
    (license gpl3+)))

(define guix-devel
  ;; Development version of Guix.
  ;;
  ;; Note: use a short commit id; when using the long one, the limit on socket
  ;; file names is exceeded while running the tests.
  (let ((commit "a43b55f"))
    (package (inherit guix-0.8.2)
      (version (string-append "0.8.2." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://git.sv.gnu.org/guix.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1r0l8gfh5nxc1j0sqj8ywkg280k9qbj7zsk33z84rvl7l0nwnk88"))
                (file-name (string-append "guix-" version "-checkout"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guix-0.8.2)
         ((#:phases phases)
          `(alist-cons-after
            'unpack 'bootstrap
            (lambda _
              ;; Make sure 'msgmerge' can modify the PO files.
              (for-each (lambda (po)
                          (chmod po #o666))
                        (find-files "." "\\.po$"))

              (zero? (system* "sh" "bootstrap")))
            ,phases))))
      (native-inputs
       `(("autoconf" ,(autoconf-wrapper))
         ("automake" ,automake)
         ("gettext" ,gnu-gettext)
         ("texinfo" ,texinfo)
         ("graphviz" ,graphviz)
         ("help2man" ,help2man)
         ,@(package-native-inputs guix-0.8.2))))))

(define-public guix guix-devel)

(define-public nix
  (package
    (name "nix")
    (version "1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://nixos.org/releases/nix/nix-"
                                 version "/nix-" version ".tar.xz"))
             (sha256
              (base32
               "077hircacgi9y4n6kf48qp4laz1h3ab6sif3rcci1jy13f05w2m3"))))
    (build-system gnu-build-system)
    ;; XXX: Should we pass '--with-store-dir=/gnu/store'?  But then we'd also
    ;; need '--localstatedir=/var'.  But then!  The thing would use /var/nix
    ;; instead of /var/guix.  So in the end, we do nothing special.
    (native-inputs `(("perl" ,perl)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("curl" ,curl)
              ("openssl" ,openssl)
              ("libgc" ,libgc)
              ("sqlite" ,sqlite)
              ("bzip2" ,bzip2)))
    (propagated-inputs `(("perl-www-curl" ,perl-www-curl)
                         ("perl-dbi" ,perl-dbi)
                         ("perl-dbd-sqlite" ,perl-dbd-sqlite)))
    (home-page "http://nixos.org/nix/")
    (synopsis "The Nix package manager")
    (description
     "Nix is a purely functional package manager.  This means that it treats
packages like values in purely functional programming languages such as
Haskell—they are built by functions that don't have side-effects, and they
never change after they have been built.  Nix stores packages in the Nix
store, usually the directory /nix/store, where each package has its own unique
sub-directory.")
    (license lgpl2.1+)))

(define-public stow
  (package
    (name "stow")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/stow/stow-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0arw1nsdlcvd7javkbk2bdvnc31d7dmb6fr25xyyi6ng76cxg2cb"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)))
    (native-inputs
     `(("perl-test-simple" ,perl-test-simple)
       ("perl-test-output" ,perl-test-output)
       ("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "https://www.gnu.org/software/stow/")
    (synopsis "Managing installed software packages")
    (description
     "GNU Stow is a symlink manager.  It generates symlinks to directories
of data and makes them appear to be merged into the same directory.  It is
typically used for managing software packages installed from source, by
letting you install them apart in distinct directories and then create
symlinks to the files in a common directory such as /usr/local.")
    (license gpl2+)))
