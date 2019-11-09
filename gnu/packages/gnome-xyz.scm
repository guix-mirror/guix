;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Leo Prikler <leo.prikler@student.tugraz.at>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu packages gnome-xyz)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xml))

(define-public matcha-theme
  (package
    (name "matcha-theme")
    (version "2019-11-02")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/matcha")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0wci9ahap8kynq8cbyxr7aba9ndb1d4kiq42xvzr34vw1rhcahrr"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (source (assoc-ref %build-inputs "source"))
                (bash (assoc-ref %build-inputs "bash"))
                (coreutils (assoc-ref %build-inputs  "coreutils"))
                (themesdir (string-append out "/share/themes")))
           (setenv "PATH"
                   (string-append coreutils "/bin:"
                                  (string-append bash "/bin:")))
           (copy-recursively source (getcwd))
           (patch-shebang "Install")
           (mkdir-p themesdir)
           (invoke "./Install" "-d" themesdir)
           #t))))
    (inputs
     `(("gtk-engines" ,gtk-engines)))
    (native-inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)))
    (synopsis "Flat design theme for GTK 3, GTK 2 and GNOME-Shell")
    (description "Matcha is a flat Design theme for GTK 3, GTK 2 and
Gnome-Shell which supports GTK 3 and GTK 2 based desktop environments
like Gnome, Unity, Budgie, Pantheon, XFCE, Mate and others.")
    (home-page "https://github.com/vinceliuice/matcha")
    (license license:gpl3+)))

(define-public delft-icon-theme
  (package
    (name "delft-icon-theme")
    (version "1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/madmaxms/iconpack-delft.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0vw3yw9f9ygzfd2k3zrfih3r0vkzlhk1bmsk8sapvk7np24i1z9s"))
       (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") "icons")
         (delete-file "icons/README.md")
         (delete-file "icons/LICENSE")
         (delete-file "icons/logo.jpg")
         (copy-recursively "icons" (string-append %output "/share/icons")))))
    (home-page "https://www.gnome-look.org/p/1199881/")
    (synopsis "Continuation of Faenza icon theme with up to date app icons")
    (description "Delft is a fork of the popular icon theme Faenza with up to
date app icons.  It will stay optically close to the original Faenza icons,
which haven't been updated for some years.  The new app icons are ported from
the Obsidian icon theme.")
    (license license:gpl3)))

(define-public gnome-shell-extension-dash-to-dock
  (package
    (name "gnome-shell-extension-dash-to-dock")
    (version "65")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/micheleg/dash-to-dock.git")
                    (commit (string-append "extensions.gnome.org-v"
                                           version))))
              (sha256
               (base32
                "0ln49l9s0yfl30pi77pz7xlmh63l9vjppi863kry5lay10dsvz47"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list (string-append "INSTALLBASE="
                                         (assoc-ref %outputs "out")
                                         "/share/gnome-shell/extensions"))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib" ,glib "bin")))
    (synopsis "Transforms GNOME's dash into a dock")
    (description "This extension moves the dash out of the
overview, transforming it into a dock for easier application launching and
faster window switching.")
    (home-page "https://micheleg.github.io/dash-to-dock/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-noannoyance
  (package
    (name "gnome-shell-extension-noannoyance")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BjoernDaase/noannoyance.git")
                    (commit "e37b5b3c31f577b4698bc6659bc9fec5ea9ac5d4")))
              (sha256
               (base32
                "0fa8l3xlh8kbq07y4385wpb908zm6x53z81q16xlmin97dln32hh"))
              (file-name (git-file-name name version))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((dst (string-append
                     (assoc-ref %outputs "out")
                     "/share/gnome-shell/extensions/"
                     "noannoyance@daase.net")))
           (mkdir-p dst)
           (copy-recursively (assoc-ref %build-inputs "source") dst)))))
    (synopsis "Removes 'Window is ready' annotation")
    (description "One of the many extensions, that remove this message.
It uses ES6 syntax and claims to be more actively maintained than others.")
    (home-page "https://extensions.gnome.org/extension/2182/noannoyance/")
    (license license:gpl2)))

(define-public numix-theme
  (package
    (name "numix-theme")
    (version "2.6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/numixproject/numix-gtk-theme.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12mw0kr0kkvg395qlbsvkvaqccr90cmxw5rrsl236zh43kj8grb7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check))))
    (native-inputs
     `(("glib:bin" ,glib "bin")             ; for glib-compile-schemas
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("xmllint" ,libxml2)
       ("ruby-sass" ,ruby-sass)))
    (synopsis "Flat theme with light and dark elements")
    (description "Numix is a modern flat theme with a combination of light and
dark elements.  It supports GNOME, Unity, Xfce, and Openbox.")
    (home-page "https://numixproject.github.io")
    (license license:gpl3+)))
