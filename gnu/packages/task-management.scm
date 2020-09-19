;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
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

(define-module (gnu packages task-management)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson))

(define-public taskwarrior
  (package
    (name "taskwarrior")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://taskwarrior.org/download/task-" version ".tar.gz"))
       (sha256 (base32
                "059a9yc58wcicc6xxsjh1ph7k2yrag0spsahp1wqmsq6h7jwwyyq"))))
    (build-system cmake-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("lua" ,lua)
       ("util-linux" ,util-linux "lib")))
    (arguments
     `(#:tests? #f ; No tests implemented.
       #:phases
       (modify-phases %standard-phases
         (add-before
          'patch-source-shebangs 'remove-broken-symlinks
          (lambda _
            ;; These files are broken symlinks - delete them.
            (delete-file "src/cal")
            (delete-file "src/calendar")
            (delete-file "src/tw"))))))
     (home-page "https://taskwarrior.org")
    (synopsis "Command line task manager")
    (description
     "Taskwarrior is a command-line task manager following the Getting Things
Done time management method.  It supports network synchronization, filtering
and querying data, exposing task data in multiple formats to other tools.")
    (license license:expat)))

(define-public blanket
  (package
    (name "blanket")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rafaelmardojai/blanket/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13xip9b2p2ai2jchkck71c849s2rlxzfvlbsgpraw9hswi0rk0jg"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f ;the "Validate appstream file" test fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'wrap-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out               (assoc-ref outputs "out"))
                    (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                    (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (python-path       (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/blanket")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("PYTHONPATH" ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("appstream-glib" ,appstream-glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-good" ,gst-plugins-good) ;for ScaleTempo plugin
       ("gtk+" ,gtk+)
       ("libhandy" ,libhandy)
       ("python-gst" ,python-gst)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://github.com/rafaelmardojai/blanket")
    (synopsis "Ambient sound and noise player")
    (description
     "Blanket provides different ambient sounds and types of noise to listen
to with the goal of improving your focus and enhancing your productivity.
You can also use it to fall asleep in a noisy environment.")
    (license license:gpl3+)))
