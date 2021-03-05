;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages crates-gtk)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-atk-sys-0.9
  (package
    (name "rust-atk-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vzcm1inhax78bcsbapr6mrp4z7lk783csyh200wi91pcxvw2lp5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("atk" ,atk)
       ("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libatk-1")
    (description "FFI bindings to libatk-1")
    (license license:expat)))

(define-public rust-atk-0.8
  (package
    (name "rust-atk")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gk6ijqsd6kh8cki1wznw570499psbppg3d5bqaayagjapxawka4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.9)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     `(("atk" ,atk)
       ("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the ATK library")
    (description "Rust bindings for the ATK library")
    (license license:expat)))

(define-public rust-cairo-rs-0.9
  (package
    (name "rust-cairo-rs")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1f5x6ipfpzz0ffph0pg0xfkdfcbr0jp59714zz857jp88zhg5h65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("cairo" ,cairo)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Cairo library")
    (description "Rust bindings for the Cairo library")
    (license license:expat)))

(define-public rust-cairo-rs-0.8
  (package
    (inherit rust-cairo-rs-0.9)
    (name "rust-cairo-rs")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cairo-rs" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11303v1fv6hsc9n70ak380gknkf0098phpcxqdhkmahqjsx4jw0m"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-rs-0.7
  (package
    (inherit rust-cairo-rs-0.8)
    (name "rust-cairo-rs")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cairo-rs" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "171m98g41avp5mmshqir4ka21napp7ma5fx45wi9mw5hwdyv8pg0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-glib" ,rust-glib-0.8)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-cairo-sys-rs-0.10
  (package
    (name "rust-cairo-sys-rs")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19wch8zc11hbi724mn16hhqyff8kw5c5bsbdlzpxdwfmkadn7lif"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2))))
    (inputs
     `(("cairo" ,cairo)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libcairo")
    (description "This package provides FFI bindings to libcairo.")
    (license license:expat)))

(define-public rust-cairo-sys-rs-0.9
  (package
    (inherit rust-cairo-sys-rs-0.10)
    (name "rust-cairo-sys-rs")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cairo-sys-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qsdy6s57yvscg2rfm7wdqrlhzbn1aq9lhk3dy1vw5f7r81blrgz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11" ,rust-x11-2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))))

(define-public rust-gdk-0.13
  (package
    (name "rust-gdk")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zbb9bwg2z9vlcjj9b59qch3mfmszsrxya7syc5a39v85adq606v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.9)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.9)
        ("rust-gdk-sys" ,rust-gdk-sys-0.10)
        ("rust-gio" ,rust-gio-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.9))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GDK 3 library")
    (description "This package provides Rust bindings for the GDK 3 library.")
    (license license:expat)))

(define-public rust-gdk-0.12
  (package
    (inherit rust-gdk-0.13)
    (name "rust-gdk")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12bmk9vfpk7f03fx22cq9ps00xylsxcpmp8c8r95r1n05xvyirgv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.8)
        ("rust-gdk-sys" ,rust-gdk-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.8))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.9
  (package
    (name "rust-gdk-pixbuf")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12lrk7zwshid8dgx9vg87bk0h4a0ilpi7w48idsrpm4xp4yawvcg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.10)
        ("rust-gio" ,rust-gio-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GdkPixbuf library")
    (description "Rust bindings for the GdkPixbuf library")
    (license license:expat)))

(define-public rust-gdk-pixbuf-0.8
  (package
    (inherit rust-gdk-pixbuf-0.9)
    (name "rust-gdk-pixbuf")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxxca0fkcw2rsd3kl3nvlb8ys4cgxqx4n5isjbv0adk8q624j72"))))
    (arguments
     `(#:cargo-inputs
       (("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-0.7
  (package
    (inherit rust-gdk-pixbuf-0.8)
    (name "rust-gdk-pixbuf")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gdk-pixbuf" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1k2g3w2p57m68bi5sldvkmgjgslgqswrjsijjhqaibdvw67409lp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-fragile" ,rust-fragile-0.3)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio" ,rust-gio-0.7)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.8)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gdk-pixbuf-sys-0.10
  (package
    (name "rust-gdk-pixbuf-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-pixbuf-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13qyxazj9bmw3llvkh6br8v7sypnbin2nxis366ppsa3gy54dziv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgdk_pixbuf-2.0")
    (description "This package provides FFI bindings to @code{libgdk_pixbuf-2.0}.")
    (license license:expat)))

(define-public rust-gdk-pixbuf-sys-0.9
  (package
    (inherit rust-gdk-pixbuf-sys-0.10)
    (name "rust-gdk-pixbuf-sys")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gdk-pixbuf-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1c2andpjb10y7bahh6nxnksh9m3g5qh4mgq9znx634cy1831p6fq"))))
    (arguments
     `(#:tests? #f      ; tests not included in release
       #:cargo-inputs
       (("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gdk-sys-0.10
  (package
    (name "rust-gdk-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s8d2jymffbv2kpwrx53krf7mpy3vdfhbb5i2n02dz80qp7m75ha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.10)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.10)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.10)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk+" ,gtk+)
       ("glib" ,glib)
       ("pango" ,pango)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgdk-3")
    (description "FFI bindings to libgdk-3")
    (license license:expat)))

(define-public rust-gdk-sys-0.9
  (package
    (inherit rust-gdk-sys-0.10)
    (name "rust-gdk-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gdk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1fwgr1b3n0khlkhdq9145m6baz9y8207ya30d30g1gyij6g6gpva"))))
    (arguments
     `(#:tests? #f
       #:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gio-0.9
  (package
    (name "rust-gio")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qc5aqi2ijval5i9df0qryy4wbmayyhy7ng5v9r0fw7zpx105dhz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.10)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-0.4))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Gio library")
    (description "Rust bindings for the Gio library")
    (license license:expat)))

(define-public rust-gio-0.8
  (package
    (inherit rust-gio-0.9)
    (name "rust-gio")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19cnla2ya0mi6wwaabd5mxbq2kzq46dg6jq2z19rpqyc2na0zl8c"))))
    (arguments
     `(#:tests? #f                    ; Not all files included in the tarball.
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1)
        ("rust-serial-test" ,rust-serial-test-0.1)
        ("rust-serial-test-derive" ,rust-serial-test-derive-0.1))))))

(define-public rust-gio-0.7
  (package
    (inherit rust-gio-0.8)
    (name "rust-gio")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gio" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1qv0wc1hqyb81c03h81s3xrl9jslrw23fr4yhygxbhih9k9vaqb2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-fragile" ,rust-fragile-0.3)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.8)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-gio-sys-0.10
  (package
    (name "rust-gio-sys")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1b2f6jvghm2djymj3qrgbqfci2f95gyhdin2pgv2qpcg5xszn92y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1)
        ("rust-winapi" ,rust-winapi-0.3))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgio-2.0")
    (description "This package provides FFI bindings to libgio-2.0.")
    (license license:expat)))

(define-public rust-gio-sys-0.9
  (package
    (inherit rust-gio-sys-0.10)
    (name "rust-gio-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gio-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "049rafihxp3maxg3fwj6062ni9dcfilvhfq6ibnfgsmr89925bag"))))
    (arguments
     `(#:tests? #f              ; Some test libraries not included in release.
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gir-format-check-0.1
  (package
    (name "rust-gir-format-check")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gir-format-check" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0znl4qcgmg0656zk9vjkwdn9wj1zpkd0m0l5jnzmahd80ii7vf4b"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/gtk-rs/gir-format-check")
    (synopsis "File format checker")
    (description "File format checker in Rust.")
    (license license:expat)))

(define-public rust-glib-0.10
  (package
    (name "rust-glib")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ifh56nhvhcrssxqkad876qqrlnl16q6b8ap3f7ncpjinw9m0s0c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-glib-macros" ,rust-glib-macros-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library")
    (description "Rust bindings for the GLib library")
    (license license:expat)))

(define-public rust-glib-0.9
  (package
    (inherit rust-glib-0.10)
    (name "rust-glib")
    (version "0.9.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glib" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h3100mf7kdfxibjz5na0sqzbd2mcsyd8pzivn3666w414x5gys0"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-futures-task" ,rust-futures-task-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-0.8
  (package
    (inherit rust-glib-0.9)
    (name "rust-glib")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glib" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ysy87zrwyzhfpf3d8rkyyr3amwj85wky05fsl7kx95s84l269xy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-glib-macros-0.10
  (package
    (name "rust-glib-macros")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-macros" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fa00s6gnj3hgri9z926199jidczlmjr1db0n4r80sins4k6lj21"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-itertools" ,rust-itertools-0.9)
        ("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
        ("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GLib library, proc macros crate")
    (description "Rust bindings for the GLib library, proc macros crate.")
    (license license:expat)))

(define-public rust-glib-sys-0.10
  (package
    (name "rust-glib-sys")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hfdwilf3dnrv3pnfbwmp3h2afgwvfsapcgjfg8276kflsbvksf7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libglib-2.0")
    (description "This package provides FFI bindings to libglib-2.0.")
    (license license:expat)))

(define-public rust-glib-sys-0.9
  (package
    (inherit rust-glib-sys-0.10)
    (name "rust-glib-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glib-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qhnwfqqcp63mx4q9744rfkq78g6ky2j8ppsxxgw0ipl08w6z1cm"))))
    (arguments
     `(#:tests? #f              ; Some test libraries not included in release.
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gobject-sys-0.10
  (package
    (name "rust-gobject-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1311d3zbdhl1g7ibj1iy1650513yrhxbjxgfhazn52ii1jv368cm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgobject-2.0")
    (description "This package provides FFI bindings to libgobject-2.0.")
    (license license:expat)))

(define-public rust-gobject-sys-0.9
  (package
    (inherit rust-gobject-sys-0.10)
    (name "rust-gobject-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gobject-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nakflbp3gjaas4fw7sn3p1p32khyfpcq1h06z7yqd10yq2ail9i"))))
    (arguments
     `(#:tests? #f              ; Some test libraries not included in release.
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-gtk-0.8
  (package
    (name "rust-gtk")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "13ygzblfv40l2kp70rnjymz7vk2g2wdjs04lhmk9q8wh0bbyiqc7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atk" ,rust-atk-0.8)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-cc" ,rust-cc-1)
        ("rust-gdk" ,rust-gdk-0.12)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.8)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gdk-sys" ,rust-gdk-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-gtk-sys" ,rust-gtk-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.8)
        ("rust-pango-sys" ,rust-pango-sys-0.9))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     `(("atk" ,atk)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the GTK+ 3 library")
    (description "This package provides Rust bindings for the GTK+ 3 library.")
    (license license:expat)))

(define-public rust-gtk-rs-lgpl-docs-0.1
  (package
    (name "rust-gtk-rs-lgpl-docs")
    (version "0.1.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-rs-lgpl-docs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xm3lm15j8yfn2jzh3sz6hrq2g2k917ahnp5caxw9c7z8sgr9f4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1))))
    (home-page "https://gtk-rs.org/")
    (synopsis "LGPL-licensed docs for Gtk-rs crates")
    (description
     "LGPL-licensed docs for Gtk-rs crates.")
    (license license:lgpl2.0)))

(define-public rust-gtk-sys-0.9
  (package
    (name "rust-gtk-sys")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gtk-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hd4w49iaivzjkbxi0bhabqp1ifkzg9g47n822sh12xlqxhgdpjk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ;missing files
       #:cargo-inputs
       (("rust-atk-sys" ,rust-atk-sys-0.9)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gdk-sys" ,rust-gdk-sys-0.9)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("gtk+" ,gtk+)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgtk-3")
    (description "This package provides FFI bindings to libgtk-3.")
    (license license:expat)))

(define-public rust-pango-0.9
  (package
    (name "rust-pango")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f64hynd9vq6966wb66mrg5kq9q371bkhncp37nqrgdyh22hcdwr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.10)
        ("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pango-sys" ,rust-pango-sys-0.10))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     `(("pango" ,pango)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the Pango library")
    (description "Rust bindings for the Pango library")
    (license license:expat)))

(define-public rust-pango-0.8
  (package
    (inherit rust-pango-0.9)
    (name "rust-pango")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pango" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xq50950il3228grzs4xvc5s6phxcl5l50grz6syvs0vixr6p70y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-0.7
  (package
    (inherit rust-pango-0.8)
    (name "rust-pango")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pango" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11np7nxb69g3kid2l78b7k519x1wk1c3f9yy7swgzy24n5qs0grr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-glib" ,rust-glib-0.8)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pango-sys-0.10
  (package
    (name "rust-pango-sys")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1468xzyk2wanxb2b32fsmwk6bnafdaicxl5b4301dlb2ic66bli4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.10)
        ("rust-gobject-sys" ,rust-gobject-sys-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-system-deps" ,rust-system-deps-1))))
    (inputs
     `(("pango" ,pango)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libpango-1.0")
    (description "This package provides FFI bindings to @code{libpango-1.0}.")
    (license license:expat)))

(define-public rust-pango-sys-0.9
  (package
    (inherit rust-pango-sys-0.10)
    (name "rust-pango-sys")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pango-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zdynikh6jrfa31fpmbrwnz46a57idci73zzkf0z0g3vj223vfc6"))))
    (arguments
     `(#:tests? #f                  ; Some test files not included in release.
       #:cargo-inputs
       (("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-pangocairo-0.9
  (package
    (name "rust-pangocairo")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pangocairo" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bap3h78hdqdyln58349qjjbcv45m8a0a16c4n9fprdj1my0gldx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.8)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pangocairo-sys" ,rust-pangocairo-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))
    (inputs
     `(("gtk+" ,gtk+)))
    (home-page "https://gtk-rs.org/")
    (synopsis "Rust bindings for the PangoCairo library")
    (description
     "Rust bindings for the PangoCairo library.")
    (license license:expat)))

(define-public rust-pangocairo-0.8
  (package
    (inherit rust-pangocairo-0.9)
    (name "rust-pangocairo")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pangocairo" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0qjiwsp73x3w0493dzycyjzxnzwq7ixwmf1ccr5r41pjhxbnk1kl"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.7)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-glib" ,rust-glib-0.8)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango" ,rust-pango-0.7)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pangocairo-sys" ,rust-pangocairo-sys-0.10)
        ("rust-gtk-rs-lgpl-docs" ,rust-gtk-rs-lgpl-docs-0.1))
       #:cargo-development-inputs
       (("rust-gir-format-check" ,rust-gir-format-check-0.1))))))

(define-public rust-pangocairo-sys-0.10
  (package
    (name "rust-pangocairo-sys")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pangocairo-sys" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1zlyf5vajarnxg5034b8qa5w5zajv96lfvlgiki26svpmcqip4m3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-shell-words" ,rust-shell-words-0.1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("gtk+" ,gtk+)))
    (home-page "https://gtk-rs.org/")
    (synopsis "FFI bindings to libgtk-3")
    (description "This package provides FFI bindings to libgtk-3.")
    (license license:expat)))
