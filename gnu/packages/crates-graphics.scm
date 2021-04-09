;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Antoine Côté <antoine.cote@posteo.net>
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

(define-module (gnu packages crates-graphics)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video))

;;;
;;; Please: Try to add new module packages in alphabetic order.

(define-public rust-andrew-0.3
  (package
    (name "rust-andrew")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "andrew" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kx79z6mh7wwp4pz683bdya54h7w7wpzjcwf834fwbv4vl4znjlc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-rusttype" ,rust-rusttype-0.9)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-xdg" ,rust-xdg-2)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/trimental/andrew")
    (synopsis "Provides convenient drawing of objects to buffers")
    (description
     "The @code{andrew} crate provides convenient drawing of objects such as
shapes, lines and text to buffers.")
    (license license:expat)))

(define-public rust-andrew-0.2
  (package
    (inherit rust-andrew-0.3)
    (name "rust-andrew")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "andrew" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pmklwcwy8g1jras46fz8xcny779zfqpg4riksrbdhkjk3w0jzwv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-line-drawing" ,rust-line-drawing-0.7)
        ("rust-rusttype" ,rust-rusttype-0.7)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-xdg" ,rust-xdg-2)
        ("rust-xml-rs" ,rust-xml-rs-0.8))
       #:cargo-development-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.4))))
    (inputs
     `(("wayland" ,wayland)))))

(define-public rust-ansi-colours-1
  (package
    (name "rust-ansi-colours")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_colours" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dnqmpk68mzvncj37jlv0362kdgsgjxg010c6psagimgh4m303qx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1))
       #:cargo-development-inputs
       (("rust-delta-e" ,rust-delta-e-0.2)
        ("rust-lab" ,rust-lab-0.4))))
    (home-page "https://github.com/mina86/ansi_colours")
    (synopsis "Palette converter between true-colour and ANSI terminal")
    (description
     "@code{ansi_colours} is a library which converts between 24-bit sRGB
colours and 8-bit colour palette used by ANSI terminals such as @code{xterm} on
@code{rxvt-unicode} in 256-colour mode.
The most common use case is when using 24-bit colours in a terminal emulator
which only support 8-bit colour palette.  This package allows true-colours to be
approximated by values supported by the terminal.")
    (license license:lgpl3+)))

(define-public rust-ansi-term-0.12
  (package
    (name "rust-ansi-term")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1ljmkbilxgmhavxvxqa7qvm6f3fjggi7q2l3a72q9x0cxjvrnanm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (license license:expat)))

(define-public rust-ansi-term-0.11
  (package
    (inherit rust-ansi-term-0.12)
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))))

(define-public rust-ansi-term-0.9
  (package
    (inherit rust-ansi-term-0.11)
    (name "rust-ansi-term")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansi_term" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xif1bh938qpfc3d0f9xgidibpm65xix11w9gszwqnia00q7rb13"))))
    (arguments `())))

(define-public rust-aom-sys-0.2
  (package
    (name "rust-aom-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aom-sys" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "03a0xhaafjn0hlpcf9ba73hv557m0jqnmj9wl57wzrcnka96zvgj"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.54)
         ("rust-metadeps" ,rust-metadeps-1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libaom" ,libaom)
       ("clang" ,clang)
       ("llvm" ,llvm)))
    (home-page "https://github.com/rust-av/aom-rs")
    (synopsis "FFI bindings to aom")
    (description "This package provides FFI bindings to aom.")
    (license license:expat)))

(define-public rust-aom-sys-0.1
  (package
    (inherit rust-aom-sys-0.2)
    (name "rust-aom-sys")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "aom-sys" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1bqcpkycv1d67r6jcl9npfbw6rkl829rdq9w6vlpb0rjqxp0xzsn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.53)
        ("rust-metadeps" ,rust-metadeps-1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libaom" ,libaom)
       ("clang" ,clang)
       ("llvm" ,llvm)))))

(define-public rust-ascii-canvas-2
  (package
    (name "rust-ascii-canvas")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ascii-canvas" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a9s8vrbc5jr6ry5ygjyfqmbs9gyya1v6dsxzsczpai8z4nvg3pz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t  ;; TODO: failes due to an unresolved import
       #:cargo-inputs
       (("rust-term" ,rust-term-0.5))))
    (home-page "https://github.com/nikomatsakis/ascii-canvas")
    (synopsis "Simple canvas for drawing lines and styled text and emitting to
the terminal")
    (description "@code{ASCII} canvas is a simple Rust library that allows you
to draw lines and colored text and then write them to the terminal.  It uses
the term library to handle the ANSI nonsense and hence it works on Windows,
Mac, and Unix.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-avif-serialize-0.6
  (package
    (name "rust-avif-serialize")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "avif-serialize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "130wq838lslkcqcp2kjci7q3aq9qpir07pvxndc81xqbn63wvdjg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.5))))
    (home-page "https://lib.rs/avif-serialize")
    (synopsis "Writer for AVIF header structure (MPEG/HEIF/MIAF/ISO-BMFF)")
    (description
     "This package provides a minimal writer for AVIF header structure.  This
is a tiny alternative to @code{libavif}.  It creates the jungle of
MPEG/HEIF/MIAF/ISO-BMFF ``boxes'' as appropriate for AVIF files.  It supports
alpha channel embedding.")
    (license license:bsd-3)))

(define-public rust-cgl-0.3
  (package
    (name "rust-cgl")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zs7skrsyrsm759vfy2cygkx52fx91b567a12bpaz1sf4d8hbv8c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; only available on macOS
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/cgl-rs")
    (synopsis "Rust bindings for CGL on Mac")
    (description "Rust bindings for CGL on Mac.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cgl-0.2
  (package
    (inherit rust-cgl-0.3)
    (name "rust-cgl")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0j8ayr8pbwvyv6l8r7m5z197rs3pqn97085w9j4rfn7yfh5yrrsm"))))
    (arguments
     `(#:skip-build? #t     ; only available on macOS
       #:cargo-inputs
       (("rust-gleam" ,rust-gleam-0.6)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-cgmath-0.17
  (package
    (name "rust-cgmath")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rvgila6ivr0dh1bxza450a4yfwdi2pwj3h1vnwg0jy4xk6l8f98"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t     ; Crate won't build without glium.
       #:cargo-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-serde" ,rust-serde-1)
        ("rust-simd" ,rust-simd-0.2))
       #:cargo-development-inputs
       (;("rust-glium" ,rust-glium-0.23)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/brendanzab/cgmath")
    (synopsis "Linear algebra and mathematics library")
    (description
     "This package provides a linear algebra and mathematics library
for computer graphics.")
    (license license:asl2.0)))

(define-public rust-cgmath-0.16
  (package
    (inherit rust-cgmath-0.17)
    (name "rust-cgmath")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07754c03v3srzf64ghsl3fggrdi4kjy6l3vyq2d2wfjfixybb934"))))
    (arguments
     `(#:skip-build? #t     ; Crate won't build without glium.
       #:cargo-inputs
       (("rust-approx" ,rust-approx-0.1)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.1)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde" ,rust-serde-1)
        ("rust-simd" ,rust-simd-0.2))
       #:cargo-development-inputs
       (;("rust-glium" ,rust-glium-0.19)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-core-graphics-0.22
  (package
    (name "rust-core-graphics")
    (version "0.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11lx6xw8nc9fpd552g60qa0cxh0maah8j2m26vkq0aslkgv3b7r6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-graphics-rs")
    (synopsis "Bindings to Core Graphics for macOS")
    (description
     "This package provides bindings to Core Graphics for macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-graphics-0.21
  (package
    (inherit rust-core-graphics-0.22)
    (name "rust-core-graphics")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i9gwzkil9k276317by0mi1pxz036h412dmcp1bzmlq4adj5anha"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics-types" ,rust-core-graphics-types-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-graphics-0.19
  (package
    (inherit rust-core-graphics-0.21)
    (name "rust-core-graphics")
    (version "0.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "08z9pgwfc0wb5v3ns7rnb2010q9g42b5vfwhp9fv4spawrs9725k"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.7)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-graphics-0.17
  (package
    (inherit rust-core-graphics-0.21)
    (name "rust-core-graphics")
    (version "0.17.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1acm3vygngnilzlr6klym5ywh7kfzh2xxrh2l41152hwmdl0jyan"))))
    (arguments
     `(#:skip-build? #t     ; only for macOS
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-core-graphics-types-0.1
  (package
    (name "rust-core-graphics-types")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12vqf0n5mjjcqjksdd82n2zh8hfda2zpiiqsr522c2266j5vcs1s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings for some fundamental Core Graphics types")
    (description
     "This package provides bindings for some fundamental Core Graphics
types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-video-sys-0.1
  (package
    (name "rust-core-video-sys")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-video-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a1qbn50jrb5hxrfshyb7y0f3pbf4ily6i6nciv7bn8ac4isvv1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.7)
        ("rust-core-graphics" ,rust-core-graphics-0.19)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-metal" ,rust-metal-0.18)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/luozijun/rust-core-video-sys")
    (synopsis "Bindings to CoreVideo.framework for macOS and iOS")
    (description
     "This package provides bindings to CoreVideo.framework for macOS
and iOS.")
    (license license:expat)))

(define-public rust-dav1d-sys-0.3
  (package
    (name "rust-dav1d-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dav1d-sys" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1jdxhnlxcml6jd67lx78ifzkn1xm18zfk4li7vjdh3fa61i073kx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bindgen" ,rust-bindgen-0.54)
        ("rust-metadeps" ,rust-metadeps-1))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("dav1d" ,dav1d)
       ("clang" ,clang)
       ("llvm" ,llvm)))
    (home-page "https://github.com/rust-av/dav1d-rs")
    (synopsis "FFI bindings to dav1d")
    (description "This package provides FFI bindings to dav1d.")
    (license license:expat)))

(define-public rust-euclid-0.20
  (package
    (name "rust-euclid")
    (version "0.20.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "euclid" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0c3hbl0kvc53k6nws0v9d46hi0giza1j079sqx2bgl4wfw65nshc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-mint" ,rust-mint-0.5)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/servo/euclid")
    (synopsis "Geometry primitives")
    (description "Geometry primitives written in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-eui48-0.3
  (package
    (name "rust-eui48")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "eui48" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mmdhczfdxwv5v5h90ydqkx0mdqiv0h2clshm2cm4qlwp0gacw29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/abaumhauer/eui48")
    (synopsis "Library to generate and parse IEEE EUI-48 and EUI-64")
    (description
     "This package provides a library to generate and parse IEEE EUI-48 and
EUI-64, also known as MAC-48 media access control addresses.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gfx-0.18
  (package
    (name "rust-gfx")
    (version "0.18.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nqmxqi3x4ni0g78g77a6aldrv8cfvzhnpqhxyd2ap4aa3wldph1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-draw-state" ,rust-draw-state-0.8)
        ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "High-performance, bindless graphics API")
    (description
     "This package provides a high-performance, bindless graphics API.")
    (license license:asl2.0)))

(define-public rust-gfx-core-0.9
  (package
    (name "rust-gfx-core")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_core" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0haldr99n12d90vqgvl77n59hywlklhdff85j2aljaz1yapdvyvm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-mint" ,rust-mint-0.5)
        ("rust-draw-state" ,rust-draw-state-0.8)
        ("rust-serde" ,rust-serde-1)
        ("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "Core library of Gfx-rs")
    (description "This package is a core library of Gfx-rs.")
    (license license:asl2.0)))

(define-public rust-gfx-device-gl-0.16
  (package
    (name "rust-gfx-device-gl")
    (version "0.16.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_device_gl" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1g5yg19jvxdmviljyakhd6253bnb2qg7v8iscf48ihc0ldgki70h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-gfx-gl" ,rust-gfx-gl-0.6)
        ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/gfx-rs/gfx")
    (synopsis "OpenGL backend for gfx-rs")
    (description "This package provides the openGL backend for gfx-rs.")
    (license license:asl2.0)))

(define-public rust-gfx-gl-0.6
  (package
    (name "rust-gfx-gl")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gfx_gl" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ppzj4bgjawdqz3fvnscqk8lnmgh95pwzh0v96vwy809cxj83lzj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/gfx-rs/gfx_gl")
    (synopsis "OpenGL bindings for gfx, based on gl-rs")
    (description
     "This package provides OpenGL bindings for gfx, based on gl-rs.")
    (license license:asl2.0)))

(define-public rust-gif-0.11
  (package
    (name "rust-gif")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1i4n9fwg3zrp07pi5zsgyza2gl8lqnap6fj6875lfy121xbbmvq2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-color-quant" ,rust-color-quant-1)
        ("rust-weezl" ,rust-weezl-0.1))))
    (home-page "https://github.com/image-rs/image-gif")
    (synopsis "GIF decoder and encoder")
    (description "This package provides a GIF decoder and encoder in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gif-0.10
  (package
    (inherit rust-gif-0.11)
    (name "rust-gif")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bw174f7civdfgryvc8pvyhicpr96hzdajnda4s3y8iv3ch907a7"))))
    (arguments
     `(#:tests? #f      ; tests not included in release
       #:cargo-inputs
       (("rust-color-quant" ,rust-color-quant-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-lzw" ,rust-lzw-0.10))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.3))))))

(define-public rust-gl-0.11
  (package
    (name "rust-gl")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1wcqpyhck0xriffkmgmldy33lwk2044hb4l02d44vm4fbvicin6p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.10))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "OpenGL bindings for rust")
    (description "This package provides OpenGL bindings for rust.")
    (license license:asl2.0)))

(define-public rust-gl-generator-0.14
  (package
    (name "rust-gl-generator")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl-generator" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-khronos-api" ,rust-khronos-api-3)
        ("rust-log" ,rust-log-0.4)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis "Code generators for bindings to the Khronos OpenGL APIs")
    (description
     "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (license license:asl2.0)))

(define-public rust-gl-generator-0.13
  (package
    (inherit rust-gl-generator-0.14)
    (name "rust-gl-generator")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl-generator" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jpqjqpyrl73sf8y20p5rv50qz8glnsvv9infg8h4vi52zgbp66a"))))))

(define-public rust-gl-generator-0.11
 (package
   (inherit rust-gl-generator-0.13)
   (name "rust-gl-generator")
   (version "0.11.0")
   (source
    (origin
      (method url-fetch)
      (uri (crate-uri "gl-generator" version))
      (file-name
       (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "1gdchvay0k0g931b2ki33mkfixcw4radk5b8sqsm29rahxg3v8ir"))))))

(define-public rust-gl-generator-0.10
  (package
    (name "rust-gl-generator")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gl_generator" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0146yd4i9wbgfrhnkc04w7n7civbanznc0q87skp6v7p7hbszzx0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-xml-rs" ,rust-xml-rs-0.8)
        ("rust-khronos-api" ,rust-khronos-api-3))))
    (home-page "https://github.com/brendanzab/gl-rs/")
    (synopsis
     "Code generators for creating bindings to the Khronos OpenGL APIs")
    (description
     "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (license license:asl2.0)))

(define-public rust-gleam-0.6
  (package
    (name "rust-gleam")
    (version "0.6.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gleam" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1iazvk3kvw3620gm6x8hy2x1lz51k04acl78cr3ppryhk5y0vqfa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.13))))
    (home-page "https://github.com/servo/gleam")
    (synopsis "Generated OpenGL bindings and wrapper for Servo")
    (description
     "Generated OpenGL bindings and wrapper for Servo.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-glutin-0.26
  (package
    (name "rust-glutin")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18szbh4dixcr7pmymvbrpv21hv0wrpii5w03rv2534bb2ywwpq8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-cocoa" ,rust-cocoa-0.23)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-wayland-egl" ,rust-wayland-egl-0.28)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.24))))
    (inputs
     `(("rust-wayland-client" ,rust-wayland-client-0.28)
       ("rust-wayland-egl" ,rust-wayland-egl-0.28)))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Cross-platform OpenGL context provider")
    (description "This package provides an OpenGL context provider.")
    (license license:asl2.0)))

(define-public rust-glutin-0.22
  (package
    (inherit rust-glutin-0.26)
    (name "rust-glutin")
    (version "0.22.0-alpha5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0lilr4f335m1fq1acmshd51zblfaglw1hha6lhalnc1fw3cg0aag"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-cocoa" ,rust-cocoa-0.19)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.5)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.20))))))

(define-public rust-glutin-0.21
  (package
    (inherit rust-glutin-0.22)
    (name "rust-glutin")
    (version "0.21.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ggyyqn7dvz4yx5ygqfvnxwfb78wvdm5y6xqw5my1b4x61dv6wak"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.2)
        ("rust-cocoa" ,rust-cocoa-0.18)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys" ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.5)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys-0.1)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit-0.19))))))

(define-public rust-glutin-egl-sys-0.1
  (package
    (name "rust-glutin-egl-sys")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-egl-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k1x1frdp4wp47qkai8zzmgqxzpfcn7780m29qgd92lbnbrxwbkp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3)
        ("rust-gl-generator" ,rust-gl-generator-0.13))))
    (home-page "https://github.com/rust-windowing/glutin")
    (synopsis "Egl bindings for glutin")
    (description "The egl bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-emscripten-sys-0.1
  (package
    (name "rust-glutin-emscripten-sys")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_emscripten_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wb3qfxg3jh6ibb7bxmlmvf4jcpzck3pn0035g1sds3nvx343pl0"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Emscripten bindings for glutin")
    (description "This package provides Emscripten bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-gles2-sys-0.1
  (package
    (name "rust-glutin-gles2-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_gles2_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00wisv3a7818bpw5nnqwibmh1bw032izix2l3657q2kkidq4w2g8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Gles2 bindings for glutin")
    (description "This package provides gles2 bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-glx-sys-0.1
  (package
    (name "rust-glutin-glx-sys")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_glx_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l8kk60kq5v6hl1qr6ym2arzvbsgkh71aa8485cp901bq27kqfby"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14)
        ("rust-x11-dl" ,rust-x11-dl-2))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Glx bindings for glutin")
    (description "This package provides glx bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-glutin-wgl-sys-0.1
  (package
    (name "rust-glutin-wgl-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin_wgl_sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15hns8b3i7iy366m61dg7jlr7wgzz8z8cakgbj3apnv92ld9b99x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.14))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "Wgl bindings for glutin")
    (description "This package provides wgl bindings for glutin.")
    (license license:asl2.0)))

(define-public rust-ical-0.7
  (package
    (name "rust-ical")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ical" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kvk1pgas67rnp0n4424lxxs8y3n1h0fw3ap8jbfcxqdmlap57sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/Peltoche/ical-rs")
    (synopsis "Ical/Vcard parser for Rust")
    (description
     "This library parse the ICalendar format defined in RFC5545, as well as
similar formats like VCard.")
    (license license:asl2.0)))

(define-public rust-ichwh-0.3
  (package
    (name "rust-ichwh")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ichwh" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0m6628yw3l812hjknmh5b5gcvhn6as9gzjz60h54zjxyy4w5ss7a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://gitlab.com/avandesa/ichwh-rs")
    (synopsis "Asynchronous implementation of @command{which}")
    (description
     "@code{ichwh} aims to be a fully-asynchronous clone of GNU which.  The
main job of @command{which} is to search for executables on the current
PATH.")
    (license license:expat)))

(define-public rust-image-0.23
  (package
    (name "rust-image")
    (version "0.23.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dg9z5sbc389spp7pm23n2b1k0gdd8hjdb8hhsp3k3npx9vl1q3w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-color-quant" ,rust-color-quant-1)
        ("rust-gif" ,rust-gif-0.11)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.16)
        ("rust-ravif" ,rust-ravif-0.6)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.6))))
    (home-page "https://github.com/image-rs/image")
    (synopsis "Imaging library written in Rust")
    (description
     "This package is an imaging library written in Rust.  It provides basic
filters and decoders for the most common image formats.")
    (license license:expat)))

(define-public rust-image-0.22
  (package
    (inherit rust-image-0.23)
    (name "rust-image")
    (version "0.22.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jpbd0p1q7xx6395ba9ikz2k4cfp26qczisa8m2v15w3hzd2mv88"))))
    (arguments
     `(#:tests? #f      ; Some test images are missing from the release.
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.15)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.3))
       #:cargo-development-inputs
       (("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.9))))))

(define-public rust-image-0.21
  (package
    (inherit rust-image-0.22)
    (name "rust-image")
    (version "0.21.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1sv534xp8yyn7jj0q6yn2bgng1350f962g81sv8v7c6pgi31wdrm"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.14)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.2))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.3)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.6))))))

(define-public rust-image-0.20
  (package
    (inherit rust-image-0.21)
    (name "rust-image")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01058n0jcw25pq5shn7lkvywv8c28xsxb3nwwyb4r16ijm1mnrj4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png-0.12)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-tiff" ,rust-tiff-0.2))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.2)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.6))))))

(define-public rust-imgref-1
  (package
    (name "rust-imgref")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "imgref" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19dd5xss3nd40avv8az2kzicpxx71c2akiqznr616hki30w9vj07"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/kornelski/imgref")
    (synopsis "2D slice of a @code{Vec}")
    (description
     "This package provides a trivial struct for interchange of 2d-dimensional
pixel buffers with width, height and stride.")
    (license license:cc0)))

(define-public rust-jpeg-decoder-0.1
  (package
    (name "rust-jpeg-decoder")
    (version "0.1.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jpeg-decoder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0lc428qgffh2a1agkq0p26mvf9rjaiswpywy5883j99mqypg0mh2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Some test files missing.
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-rayon" ,rust-rayon-1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-png" ,rust-png-0.14)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/image-rs/jpeg-decoder")
    (synopsis "JPEG decoder")
    (description "JPEG decoder written in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-line-drawing-0.7
  (package
    (name "rust-line-drawing")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "line_drawing" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fcna7hq1g1kkkqy07hydscx5d2zgb6gskz3vnsvsif8h8ysvisw"))))
    (build-system cargo-build-system)
    (arguments
     ;; This version does not specify any versions on dependants.
     `(#:tests? #f      ; Cannot compile line_drawing for the test suite.
       #:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-bresenham" ,rust-bresenham-0.1)
        ("rust-image" ,rust-image-0.22) ; 0.17?
        ("rust-rand" ,rust-rand-0.6))))
    (home-page "https://github.com/expenses/line_drawing")
    (synopsis "Collection of line-drawing algorithms")
    (description
     "This package provides a collection of line-drawing algorithms for use in
graphics and video games.")
    (license license:expat)))

(define-public rust-lyon-geom-0.14
  (package
    (name "rust-lyon-geom")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_geom" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "178z4cqqmyw0rsabbgx9phkjxjzcnq0604062lqjlq87k063216a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-euclid" ,rust-euclid-0.20)
        ("rust-arrayvec" ,rust-arrayvec-0.4)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nical/lyon")
    (synopsis "2D graphics rendering on the GPU using tessellation")
    (description
     "This package provides 2D graphics rendering on the GPU using tessellation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lyon-path-0.14
  (package
    (name "rust-lyon-path")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lyon_path" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0qk8x46w0sf6j04l6gvhgn9kr4ymcqkmkh67w8wqahm54jn5gjqb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lyon-geom" ,rust-lyon-geom-0.14)
        ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/nical/lyon")
    (synopsis "Types and utilities to store, build and iterate over 2D paths")
    (description
     "Types and utilities to store, build and iterate over 2D paths.")
    (license (list license:expat license:asl2.0))))

(define-public rust-osmesa-sys-0.1
  (package
    (name "rust-osmesa-sys")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "osmesa-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fq1q1zcgfb0qydrg9r2738jlwc4hqxgb9vj11z72bjxx7kfrkw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-shared-library" ,rust-shared-library-0.1))))
    (home-page "https://crates.io/crates/osmesa-sys")
    (synopsis "OSMesa library bindings for Rust")
    (description "This package provides OSMesa library bindings for Rust.")
    (license license:cc0)))

(define-public rust-piston-0.49
  (package
    (name "rust-piston")
    (version "0.49.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1y0rbw92mzagqmwk79wv9axq0m7aid0s0d5cppyzh33wrxhdl3xj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-pistoncore-input" ,rust-pistoncore-input-0.28)
        ("rust-pistoncore-window" ,rust-pistoncore-window-0.44)
        ("rust-pistoncore-event-loop" ,rust-pistoncore-event-loop-0.49))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Piston game engine core libraries")
    (description
     "The Piston game engine core libraries.")
    (license license:expat)))

(define-public rust-piston-float-1
  (package
    (name "rust-piston-float")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-float" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0r35aasycms79hf2vf1ap40kkp8ywgl4hmfkf762dq8jwd3vw07r"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/float")
    (synopsis
      "Traits for generic floats in game development")
    (description
      "Traits for generic floats in game development")
    (license license:expat)))

(define-public rust-piston-gfx-texture-0.40
  (package
    (name "rust-piston-gfx-texture")
    (version "0.40.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-gfx_texture" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nr5awdgk3njfvfanszrv4gxz93f6skid1c8yijswccygripchqz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gfx" ,rust-gfx-0.18)
        ("rust-image" ,rust-image-0.22)
        ("rust-piston-texture" ,rust-piston-texture-0.8)
        ("rust-gfx-core" ,rust-gfx-core-0.9))))
    (home-page "https://github.com/pistondevelopers/gfx_texture")
    (synopsis
      "Gfx texture representation that works nicely with Piston libraries")
    (description "This package provides a Gfx texture representation that works
nicely with Piston libraries.")
    (license license:expat)))

(define-public rust-piston-graphics-api-version-0.2
  (package
    (name "rust-piston-graphics-api-version")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-graphics_api_version" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1b5p6s45jqv057lpbxkiq3yrdjjhvcynmi2vjf8292rf0yh4hky5"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/PistonDevelopers/graphics_api_version")
    (synopsis
      "A library for storing graphics API versions")
    (description
      "This package provides a library for storing graphics API versions")
    (license license:expat)))

(define-public rust-piston-shaders-graphics2d-0.3
  (package
    (name "rust-piston-shaders-graphics2d")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-shaders_graphics2d" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1dhh9bv4q19gdnj9d1nqq0yrvzs6gcn0c5j1p1f3xzyzq7d1gg4p"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/PistonDevelopers/shaders")
    (synopsis "Shaders for 2D graphics in Rust")
    (description "Shaders for 2D graphics in Rust")
    (license license:expat)))

(define-public rust-piston-texture-0.8
  (package
    (name "rust-piston-texture")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-texture" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1pcv5my49b8xzqcb87wqh2ndgvr4s9ipys96s0h9j2plxrj3bjb2"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
      "https://github.com/pistondevelopers/texture")
    (synopsis "A generic library for textures")
    (description
      "This package provides a generic library for textures")
    (license license:expat)))

(define-public rust-piston-viewport-1
  (package
    (name "rust-piston-viewport")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston-viewport" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16378hcy41b7x3zj2z4har0wq6fl4r62kf9p106jjl8hg2dv3aq1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-piston-float" ,rust-piston-float-1))))
    (home-page "https://github.com/PistonDevelopers/viewport")
    (synopsis "Library for storing viewport information")
    (description
     "This package provides a library for storing viewport information.")
    (license license:expat)))

(define-public rust-piston-window-0.105
  (package
    (name "rust-piston-window")
    (version "0.105.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston_window" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "05n2905gkp5ck25kbq95ia6pj1xz63dpp247jz3xcw1d41xpvi95"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gfx-device-gl" ,rust-gfx-device-gl-0.16)
        ("rust-gfx" ,rust-gfx-0.18)
        ("rust-piston2d-graphics" ,rust-piston2d-graphics-0.35)
        ("rust-piston" ,rust-piston-0.49)
        ("rust-shader-version" ,rust-shader-version-0.6)
        ("rust-pistoncore-glutin-window" ,rust-pistoncore-glutin-window-0.63)
        ("rust-piston2d-gfx-graphics" ,rust-piston2d-gfx-graphics-0.66)
        ("rust-piston-texture" ,rust-piston-texture-0.8))))
    (home-page "https://github.com/pistondevelopers/piston_window")
    (synopsis "Official Piston window wrapper for the Piston game engine")
    (description
     "The official Piston window wrapper for the Piston game engine.")
    (license license:expat)))

(define-public rust-piston2d-gfx-graphics-0.66
  (package
    (name "rust-piston2d-gfx-graphics")
    (version "0.66.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston2d-gfx_graphics" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1pmlkf5rl6pr0c1lqm0059xwj9pwlws7gaq9w6r9d916di6fzki1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gfx" ,rust-gfx-0.18)
        ("rust-piston-shaders-graphics2d" ,rust-piston-shaders-graphics2d-0.3)
        ("rust-piston-gfx-texture" ,rust-piston-gfx-texture-0.40)
        ("rust-shader-version" ,rust-shader-version-0.6)
        ("rust-draw-state" ,rust-draw-state-0.8))))
    (home-page "https://github.com/PistonDevelopers/gfx_graphics")
    (synopsis "Gfx 2D back-end for the Piston game engine")
    (description
     "This package provides a Gfx 2D back-end for the Piston game engine.")
    (license license:expat)))

(define-public rust-piston2d-graphics-0.35
  (package
    (name "rust-piston2d-graphics")
    (version "0.35.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "piston2d-graphics" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dx2fanxc2pj76hc5l72x0fh4qg9gchjlr8rmbhdk6jpggcmq56g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-interpolation" ,rust-interpolation-0.2)
        ("rust-rusttype" ,rust-rusttype-0.7)
        ("rust-piston-texture" ,rust-piston-texture-0.8)
        ("rust-piston-viewport" ,rust-piston-viewport-1)
        ("rust-read-color" ,rust-read-color-1)
        ("rust-vecmath" ,rust-vecmath-1)
        ("rust-fnv" ,rust-fnv-1))))
    (home-page "https://github.com/pistondevelopers/graphics")
    (synopsis "Library for 2D graphics that works with multiple back-ends")
    (description "This package provides a library for 2D graphics that works
with multiple back-ends.")
    (license license:expat)))

(define-public rust-pistoncore-event-loop-0.49
  (package
    (name "rust-pistoncore-event-loop")
    (version "0.49.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-event_loop" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h9ij9vx42xg39198yxdlpk842pli5jqm2kwswiv3bqqcji0fwsm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-pistoncore-input" ,rust-pistoncore-input-0.28)
        ("rust-pistoncore-window" ,rust-pistoncore-window-0.44))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Piston event loop for games and interactive applications")
    (description "This package provides a Piston event loop for games and
interactive applications.")
    (license license:expat)))

(define-public rust-pistoncore-glutin-window-0.63
  (package
    (name "rust-pistoncore-glutin-window")
    (version "0.63.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-glutin_window" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0dhbyxarv5i742d400bmqdqq3f8c25kcgcg0xavrc18dc913rixc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-gl" ,rust-gl-0.11)
        ("rust-glutin" ,rust-glutin-0.21)
        ("rust-pistoncore-input" ,rust-pistoncore-input-0.28)
        ("rust-pistoncore-window" ,rust-pistoncore-window-0.44)
        ("rust-shader-version" ,rust-shader-version-0.6))))
    (home-page "https://github.com/pistondevelopers/glutin_window")
    (synopsis "Piston window back-end using the Glutin library")
    (description
     "This package provides a Piston window back-end using the Glutin library.")
    (license license:expat)))

(define-public rust-pistoncore-input-0.28
  (package
    (name "rust-pistoncore-input")
    (version "0.28.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-input" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1rrcz9px098m3nx98gvrvzirfdp3vg03cblfkcrp4wnvswc0hwq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-piston-viewport" ,rust-piston-viewport-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-bitflags" ,rust-bitflags-1))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Structure for user input")
    (description
     "This package provides a structure for user input.")
    (license license:expat)))

(define-public rust-pistoncore-window-0.44
  (package
    (name "rust-pistoncore-window")
    (version "0.44.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pistoncore-window" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18qy3nnpb9jczvkiyzzznamck0pzgiyi6073jrkldnci6b3in10q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-piston-graphics-api-version"
         ,rust-piston-graphics-api-version-0.2)
        ("rust-pistoncore-input" ,rust-pistoncore-input-0.28))))
    (home-page "https://github.com/PistonDevelopers/piston")
    (synopsis "Library for window abstraction")
    (description
     "This package provides a library for window abstraction.")
    (license license:expat)))

(define-public rust-png-0.16
  (package
    (name "rust-png")
    (version "0.16.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ipl44q3vy4kvx6j296vk7d4v8gvcg203lrkvvixwixq1j98fciw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-deflate" ,rust-deflate-0.8)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.3))))
    (home-page "https://github.com/image-rs/image-png.git")
    (synopsis "PNG decoding and encoding library in pure Rust")
    (description
     "This package is a PNG decoding and encoding library in pure Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-png-0.15
  (package
    (inherit rust-png-0.16)
    (name "rust-png")
    (version "0.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "10x2qkhyfnm3si5vgx77r2ik811gaap7ahi825wfxgsb0lirm1gg"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-crc32fast" ,rust-crc32fast-1)
        ("rust-deflate" ,rust-deflate-0.7)
        ("rust-inflate" ,rust-inflate-0.4))
       #:cargo-development-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ;; TODO: glium has many cyclic dependencies with other packages
        ;;("rust-glium" ,rust-glium-0.24)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-term" ,rust-term-0.6))))))

(define-public rust-png-0.14
  (package
    (inherit rust-png-0.15)
    (name "rust-png")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nf3a8r9p9zrj4x30b48f7yv18dz9xkmrq9b3lnzmpnhzn0z9nk3"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-deflate" ,rust-deflate-0.7)
        ("rust-inflate" ,rust-inflate-0.4)
        ("rust-num-iter" ,rust-num-iter-0.1))
       #:cargo-development-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ;; TODO: glium has many cyclic dependencies with other packages
        ;; ("rust-glium" ,rust-glium-0.22)
        ("rust-glob" ,rust-glob-0.2)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-term" ,rust-term-0.4))))))

(define-public rust-png-0.12
  (package
    (inherit rust-png-0.14)
    (name "rust-png")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "png" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nqlc8lqf8ncv3kj0gzlxwli61dbbxcjlrp176kvilw4sl09cjzm"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-deflate" ,rust-deflate-0.7)
        ("rust-inflate" ,rust-inflate-0.4)
        ("rust-num-iter" ,rust-num-iter-0.1))
       #:cargo-development-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ;; TODO: gluum has many cyclic dependencies with other packages
        ;; ("rust-glium" ,rust-glium-0.21)
        ("rust-glob" ,rust-glob-0.2)
        ("rust-term" ,rust-term-0.4))))))

(define-public rust-rav1e-0.4
  (package
    (name "rust-rav1e")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rav1e" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02cpgzycfgnflnv8sck6ajasa7abfgdzn6b4jv01sf6r21yfipbq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aom-sys" ,rust-aom-sys-0.2)
        ("rust-arbitrary" ,rust-arbitrary-0.4)
        ("rust-arg-enum-proc-macro" ,rust-arg-enum-proc-macro-0.3)
        ("rust-arrayvec" ,rust-arrayvec-0.5)
        ("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-av-metrics" ,rust-av-metrics-0.6)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bitstream-io" ,rust-bitstream-io-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-cc" ,rust-cc-1)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-clap" ,rust-clap-2)
        ("rust-console" ,rust-console-0.14)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-crossbeam" ,rust-crossbeam-0.8)
        ("rust-dav1d-sys" ,rust-dav1d-sys-0.3)
        ("rust-fern" ,rust-fern-0.6)
        ("rust-image" ,rust-image-0.23)
        ("rust-interpolate-name" ,rust-interpolate-name-0.2)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-ivf" ,rust-ivf-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libfuzzer-sys" ,rust-libfuzzer-sys-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-nasm-rs" ,rust-nasm-rs-0.2)
        ("rust-noop-proc-macro" ,rust-noop-proc-macro-0.3)
        ("rust-num-derive" ,rust-num-derive-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-paste" ,rust-paste-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-chacha" ,rust-rand-chacha-0.3)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-rust-hawktracer" ,rust-rust-hawktracer-0.7)
        ("rust-rustc-version" ,rust-rustc-version-0.3)
        ("rust-scan-fmt" ,rust-scan-fmt-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-simd-helpers" ,rust-simd-helpers-0.1)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-v-frame" ,rust-v-frame-0.2)
        ("rust-vergen" ,rust-vergen-3)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-y4m" ,rust-y4m-0.7))))
    (home-page "https://github.com/xiph/rav1e")
    (synopsis "Fast and safe AV1 encoder")
    (description
     "@code{rav1e} is an AV1 video encoder.  It is designed to eventually
cover all use cases, though in its current form it is most suitable for cases
where libaom (the reference encoder) is too slow.")
    (license license:bsd-2)))

(define-public rust-ravif-0.6
  (package
    (name "rust-ravif")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ravif" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gyc7w1fz3qdk95cdpkj185dm6lskxfp329xm69waxc565fcz9rx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-avif-serialize" ,rust-avif-serialize-0.6)
        ("rust-imgref" ,rust-imgref-1)
        ("rust-loop9" ,rust-loop9-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-rav1e" ,rust-rav1e-0.4)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rgb" ,rust-rgb-0.8))))
    (home-page "https://lib.rs/ravif")
    (synopsis "Library for encoding images in AVIF format")
    (description
     "This package is a rav1e-based pure Rust library for encoding images in
AVIF format.")
    (license license:bsd-3)))

(define-public rust-raw-window-handle-0.3
  (package
    (name "rust-raw-window-handle")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "raw-window-handle" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04c2wir7qq3g2b143yav52a1g5ack8ffqx2bpmrn9bc0dix1li0a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/rust-windowing/raw-window-handle")
    (synopsis "Interoperability library for Rust Windowing applications")
    (description
     "Interoperability library for Rust Windowing applications.")
    (license license:expat)))

(define-public rust-resize-0.3
  (package
    (name "rust-resize")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "resize" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ai83laz5yvg4r7z9q8d1m0vq0fqj2ycyprw5fxzxyzaj3im7rmr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-png" ,rust-png-0.15))))
    (home-page "https://github.com/PistonDevelopers/resize")
    (synopsis "Simple image resampling library in pure Rust")
    (description
     "This package provides a simple image resampling library in pure Rust.")
    (license license:expat)))

(define-public rust-rgb-0.8
  (package
    (name "rust-rgb")
    (version "0.8.25")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rgb" version))
       (file-name (string-append name "-" version ".crate"))
       (sha256
        (base32
         "1lrv3x5h1lvdzg1qqr8aiysz978m35zpjdkyicnvkarnh8zkqzr8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytemuck" ,rust-bytemuck-1)
        ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://lib.rs/crates/rgb")
    (synopsis "Struct for sharing pixels between crates")
    (description
     "This package provides @code{struct RGB/RGBA/etc.} for sharing pixels
between crates + convenience methods for color manipulation.  It allows no-copy
high-level interoperability.  It also adds common convenience methods and
implements standard Rust traits to make `RGB`/`RGBA` pixels and slices
first-class Rust objects.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.12
  (package
    (name "rust-smithay-client-toolkit")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rjdszpf8pns99gyy8f5axf01ckc33f30dddfazyfg45xfii6vii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-andrew" ,rust-andrew-0.3)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.6)
        ("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-memmap2" ,rust-memmap2-0.1)
        ("rust-nix" ,rust-nix-0.18)
        ("rust-wayland-client" ,rust-wayland-client-0.28)
        ("rust-wayland-cursor" ,rust-wayland-cursor-0.28)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.28))))
    (home-page "https://github.com/smithay/client-toolkit")
    (synopsis "Toolkit for making client Wayland applications")
    (description
     "This package provides a toolkit for making client Wayland applications.")
    (license license:expat)))

(define-public rust-smithay-client-toolkit-0.6
  (package
    (inherit rust-smithay-client-toolkit-0.12)
    (name "rust-smithay-client-toolkit")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m20687zs36l6xak2s5k9s7qp78ly8xfjpbmrhacp7whfn4hx5lk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-andrew" ,rust-andrew-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memmap" ,rust-memmap-0.7)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.23))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-image" ,rust-image-0.21)
        ("rust-wayland-client" ,rust-wayland-client-0.23))))))

(define-public rust-smithay-client-toolkit-0.4
  (package
    (inherit rust-smithay-client-toolkit-0.6)
    (name "rust-smithay-client-toolkit")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-client-toolkit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yj8yzd0lhqpsgq0x4iikl9a02q2hnkky81brk938alv0ibqrjrc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-andrew" ,rust-andrew-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memmap" ,rust-memmap-0.7)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-protocols" ,rust-wayland-protocols-0.21))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-image" ,rust-image-0.20)
        ("rust-wayland-client" ,rust-wayland-client-0.21))))))

(define-public rust-smithay-clipboard-0.6
  (package
    (name "rust-smithay-clipboard")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14dwisd56cbr80zf719l3fh0n8pm1fjmvry9lsbhdbccf8cv525b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.12)
        ("rust-wayland-client" ,rust-wayland-client-0.28))))
    (home-page "https://github.com/smithay/smithay-clipboard")
    (synopsis "Access to the Wayland clipboard for client applications")
    (description
     "This package provides access to the Wayland clipboard for client
applications.")
    (license license:expat)))

(define-public rust-smithay-clipboard-0.3
  (package
    (inherit rust-smithay-clipboard-0.6)
    (name "rust-smithay-clipboard")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smithay-clipboard" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1h7qhcx44cgwncgpn5llky0c56vgsg9mqrkybb2z37vsxxia4rwn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.14)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.6))
       #:cargo-development-inputs
       (("rust-andrew" ,rust-andrew-0.2))))
    (inputs
     `(("wayland" ,wayland)))))

(define-public rust-tiff-0.6
  (package
    (name "rust-tiff")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ds48vs919ccxa3fv1www7788pzkvpg434ilqkq7sjb5dmqg8lws"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-jpeg-decoder" ,rust-jpeg-decoder-0.1)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.4)
        ("rust-weezl" ,rust-weezl-0.1))))
    (home-page "https://github.com/image-rs/image-tiff")
    (synopsis "TIFF decoding and encoding library in pure Rust")
    (description
     "This package provides TIFF decoding and encoding library in pure Rust.")
    (license license:expat)))

(define-public rust-tiff-0.5
  (package
    (inherit rust-tiff-0.6)
    (name "rust-tiff")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tiff" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bzzvxcx21pzryxgd7x7a1himiqs2y4k55754wzlr56sqj3qlfrz"))))
    (arguments
     `(#:tests? #f      ; not all test files included
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.3))))))

(define-public rust-tiff-0.3
  (package
    (inherit rust-tiff-0.5)
    (name "rust-tiff")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zgmbny2f8rssqmjdfvnysy0vqwcvlwl6q9f5yixhavlqk7w5dyp"))))
    (arguments
     `(#:tests? #f      ; Tests images not included with release.
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-derive" ,rust-num-derive-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-tiff-0.2
  (package
    (inherit rust-tiff-0.3)
    (name "rust-tiff")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tiff" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1kn7psgpacns337vvqh272rkqwnakmjd51rc7ygwnc03ibr38j0y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-lzw" ,rust-lzw-0.10)
        ("rust-num-derive" ,rust-num-derive-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2))))))

(define-public rust-wayland-client-0.28
  (package
    (name "rust-wayland-client")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mxnflzv9s3qpcp0z7kqvrzki5bknfar9n9yky06f8ivs00vxgdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.18)
        ("rust-scoped-tls" ,rust-scoped-tls-1))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     `(("rust-bitflags" ,rust-bitflags-1)
       ("rust-downcast-rs" ,rust-downcast-rs-1)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-nix" ,rust-nix-0.18)
       ("rust-scoped-tls" ,rust-scoped-tls-1)
       ("rust-wayland-commons" ,rust-wayland-commons-0.28)
       ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
       ("rust-wayland-sys" ,rust-wayland-sys-0.28)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Rust bindings to the standard C implementation of the wayland protocol")
    (description
     "This package provides Rust bindings to the standard C implementation of
the wayland protocol, client side.")
    (license license:expat)))

(define-public rust-wayland-client-0.23
  (package
    (inherit rust-wayland-client-0.28)
    (name "rust-wayland-client")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nmw2kz70llc5mxwzg6bglnqy0qnyr9224zjmq9czazgw3mq045g"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.23)
        ("rust-wayland-sys" ,rust-wayland-sys-0.23)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs `())))

(define-public rust-wayland-client-0.21
  (package
    (inherit rust-wayland-client-0.23)
    (name "rust-wayland-client")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-client" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04r7dy074hhdalsi1day482wvmczr40hg7qvrnzkgxpakrgkx5j9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.21))
       #:cargo-development-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-tempfile" ,rust-tempfile-3))))))

(define-public rust-wayland-commons-0.28
  (package
    (name "rust-wayland-commons")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0mid1sgy3bmiywnrhsr31b8w6zvk1ll2ci2as15ddv8pczvm0128"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.18)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-smallvec" ,rust-smallvec-1))))
    (inputs
     `(("rust-nix" ,rust-nix-0.18)
       ("rust-once-cell" ,rust-once-cell-1)
       ("rust-smallvec" ,rust-smallvec-1)
       ("rust-wayland-sys" ,rust-wayland-sys-0.28)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Types and structures used by wayland-client and wayland-server")
    (description
     "This package provides common types and structures used by wayland-client
and wayland-server.")
    (license license:expat)))

(define-public rust-wayland-commons-0.23
  (package
    (inherit rust-wayland-commons-0.28)
    (name "rust-wayland-commons")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nyvcs6xxxzqgh0wvc7z0fgi89bf3h9p4qrbf77bnfbwlb8v0rmv"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-sys" ,rust-wayland-sys-0.23))))
    (inputs `())))

(define-public rust-wayland-commons-0.21
  (package
    (inherit rust-wayland-commons-0.23)
    (name "rust-wayland-commons")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-commons" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1v1jpcsnn6cwwy5ii5pdl58i6b9slmi8mn4my4fpwrlbfsb8ih20"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21))))))

(define-public rust-wayland-cursor-0.28
  (package
    (name "rust-wayland-cursor")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-cursor" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pvf96a9hg7b40vyvamcg491sa0006fr9bzf1xkaf8q22qn15syn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nix" ,rust-nix-0.18)
        ("rust-xcursor" ,rust-xcursor-0.3))))
    (inputs
     `(("rust-nix" ,rust-nix-0.18)
       ("rust-wayland-client" ,rust-wayland-client-0.28)
       ("rust-xcursor" ,rust-xcursor-0.3)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Bindings to libwayland-cursor")
    (description
     "This crate provides helpers to load the system provided cursor images
and load them into WlBuffers as well as obtain the necessary metadata to
properly display animated cursors.")
    (license license:expat)))

(define-public rust-wayland-egl-0.28
  (package
    (name "rust-wayland-egl")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-egl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xd7iap0x4sidmy9dv02cdnxjhnbk9li7r7f39x9cg0i8xs50ly6"))))
    (build-system cargo-build-system)
    (inputs
     `(("rust-wayland-client" ,rust-wayland-client-0.28)
       ("rust-wayland-sys" ,rust-wayland-sys-0.28)))
    ;; For the PKG_CONFIG_PATH environment variable.
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Bindings to libwayland-egl")
    (description
     "This crate provides bindings for OpenGL/Vulkan support for
Wayland client apps.  It allows to create an EGLSurface from any
WlSurface, which can then play the role of the base surface for
initializing an OpenGL or Vulkan context.")
    (license license:expat)))

(define-public rust-wayland-protocols-0.28
  (package
    (name "rust-wayland-protocols")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c0sw13qssrvf3jgygwqpiimpaagz3haxn9jridd4k85sfs856ii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1))))
    (inputs
     `(("rust-bitflags" ,rust-bitflags-1)
       ("rust-wayland-client" ,rust-wayland-client-0.28)
       ("rust-wayland-commons" ,rust-wayland-commons-0.28)
       ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
       ("rust-wayland-server" ,rust-wayland-server-0.28)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generated API for the officials Wayland protocol extensions")
    (description
     "This package provides a generated API for the officials Wayland protocol
extensions.")
    (license license:expat)))

(define-public rust-wayland-protocols-0.23
  (package
    (inherit rust-wayland-protocols-0.28)
    (name "rust-wayland-protocols")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ygwbzqlnks5xzafka3c8ag6k92g2h6ygj2xsmvjfx2n6rj8dhkc"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-wayland-commons" ,rust-wayland-commons-0.23)
        ("rust-wayland-server" ,rust-wayland-server-0.23)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))))
    (inputs `())))

(define-public rust-wayland-protocols-0.21
  (package
    (inherit rust-wayland-protocols-0.23)
    (name "rust-wayland-protocols")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-protocols" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0i91yh3nxk9llhly2ly3nvlfx0lbpvyq919cgmnyx3j25bmf5zaa"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-server" ,rust-wayland-server-0.21)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.21))))))

(define-public rust-wayland-scanner-0.28
  (package
    (name "rust-wayland-scanner")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0g8ky63qk27in7zajycj3fyydsxlj19hanfcvr8d7z5kcxbvl43h"))))
    (build-system cargo-build-system)
    (inputs
     `(("rust-proc-macro2" ,rust-proc-macro2-1)
       ("rust-quote" ,rust-quote-1)
       ("rust-xml-rs" ,rust-xml-rs-0.8)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "Generate Rust APIs from XML Wayland protocol files")
    (description
     "Wayland Scanner generates Rust APIs from XML Wayland protocol files.
It is intended for use with wayland-sys.  You should only need this crate if
you are working on custom Wayland protocol extensions.  Look at the
wayland-client crate for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-scanner-0.23
  (package
    (inherit rust-wayland-scanner-0.28)
    (name "rust-wayland-scanner")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0g8wcphykjrcpslznyi3qccx1pckw97rckq5b295nfbg6r3j5c4k"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-0.6)
        ("rust-xml-rs" ,rust-xml-rs-0.8))))))

(define-public rust-wayland-scanner-0.21
  (package
    (inherit rust-wayland-scanner-0.23)
    (name "rust-wayland-scanner")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-scanner" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17mp49v7w0p0x5ry628lj2llljnwkr9aj9g4bqqhfibid32jhf5z"))))))

(define-public rust-wayland-server-0.28
  (package
    (name "rust-wayland-server")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09jfdjfqhjfcpiz4csgh60ymfkmz1cl3jmxyzq9hzcp0kyyxix93"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-nix" ,rust-nix-0.18)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-scoped-tls" ,rust-scoped-tls-1))))
    (inputs
     `(("rust-bitflags" ,rust-bitflags-1)
       ("rust-downcast-rs" ,rust-downcast-rs-1)
       ("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-nix" ,rust-nix-0.18)
       ("rust-parking-lot" ,rust-parking-lot-0.11)
       ("rust-scoped-tls" ,rust-scoped-tls-1)
       ("rust-wayland-commons" ,rust-wayland-commons-0.28)
       ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
       ("rust-wayland-sys" ,rust-wayland-sys-0.28)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis
     "Bindings to the standard C implementation of the wayland protocol")
    (description
     "This package provides Rust bindings to the standard C implementation of
the wayland protocol, server side.")
    (license license:expat)))

(define-public rust-wayland-server-0.23
  (package
    (inherit rust-wayland-server-0.28)
    (name "rust-wayland-server")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ccsalq6gnf07klnbjx2dxcbibhw03rqsgi578p913s3zsjlcg8a"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.23)
        ("rust-wayland-sys" ,rust-wayland-sys-0.23)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.23))))
    (inputs `())))

(define-public rust-wayland-server-0.21
  (package
    (inherit rust-wayland-server-0.23)
    (name "rust-wayland-server")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-server" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ayn4wlrpg0fw04prri9awpkjvbzjil0d3l3a8zs9pdbnspvw6ah"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-downcast-rs" ,rust-downcast-rs-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-nix" ,rust-nix-0.14)
        ("rust-wayland-commons" ,rust-wayland-commons-0.21)
        ("rust-wayland-sys" ,rust-wayland-sys-0.21)
        ("rust-wayland-scanner" ,rust-wayland-scanner-0.21))))))

(define-public rust-wayland-sys-0.28
  (package
    (name "rust-wayland-sys")
    (version "0.28.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16f03jsy7q6p2wpaazc4w4kycyyk0fz7lacpdbcizl9m1i7874v7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libwayland (assoc-ref inputs "wayland")))
               (substitute* (find-files "src" "\\.rs$")
                 (("libwayland.*\\.so" shared-lib)
                  (string-append libwayland "/lib/" shared-lib)))
               #t))))))
    (inputs
     `(("rust-dlib" ,rust-dlib-0.4)
       ("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-pkg-config" ,rust-pkg-config-0.3)))
    (propagated-inputs
     `(("wayland" ,wayland)))
    (home-page "https://github.com/smithay/wayland-rs")
    (synopsis "FFI bindings to the various @file{libwayland-*.so} libraries")
    (description
     "This package provides FFI bindings to the various
@file{libwayland-*.so} libraries.  You should only need this crate if
you are working on custom Wayland protocol extensions.  Look at the
crate @code{rust-wayland-client} for usable bindings.")
    (license license:expat)))

(define-public rust-wayland-sys-0.23
  (package
    (inherit rust-wayland-sys-0.28)
    (name "rust-wayland-sys")
    (version "0.23.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1x2qafvj8hd2x5qfaan2dfpw9amg0f5g9sqrkdy7qvbddsl8jknr"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-dlib" ,rust-dlib-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2))))
    (inputs `())
    (propagated-inputs `())))

(define-public rust-wayland-sys-0.21
  (package
    (inherit rust-wayland-sys-0.23)
    (name "rust-wayland-sys")
    (version "0.21.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wayland-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a0ndgkg98pvmkv44yya4f7mxzjaxylknqh64bpa05w0azyv02jj"))))))

(define-public rust-winit-0.24
  (package
    (name "rust-winit")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15zmpx5ip6ziqhds7md1s0ri0blhxfa8fg1ylg84pf0frrpxlkns"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cocoa" ,rust-cocoa-0.24)
        ("rust-core-foundation" ,rust-core-foundation-0.9)
        ("rust-core-graphics" ,rust-core-graphics-0.22)
        ("rust-core-video-sys" ,rust-core-video-sys-0.1)
        ("rust-dispatch" ,rust-dispatch-0.2)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-mio-extras" ,rust-mio-extras-2)
        ("rust-ndk" ,rust-ndk-0.2)
        ("rust-ndk-glue" ,rust-ndk-glue-0.2)
        ("rust-ndk-sys" ,rust-ndk-sys-0.2)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.12)
        ("rust-stdweb" ,rust-stdweb-0.4)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))))
    (inputs
     `(("rust-wayland-client" ,rust-wayland-client-0.28)))
    (home-page "https://github.com/rust-windowing/winit")
    (synopsis "Window creation library")
    (description
     "Winit is a window creation and management library. It can create
windows and lets you handle events (for example: the window being
resized, a key being pressed, a mouse movement, etc.) produced by
window.

Winit is designed to be a low-level brick in a hierarchy of libraries.
Consequently, in order to show something on the window you need to use
the platform-specific getters provided by winit, or another library.")
    (license license:asl2.0)))

(define-public rust-winit-0.20
  (package
    (inherit rust-winit-0.24)
    (name "rust-winit")
    (version "0.20.0-alpha6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1g5cchl97zcg525j6jdr77yby8cmhwv1qqwcd3sf4l4zl263195z"))
       (patches
         (list
           (origin
             (method url-fetch)
             (uri "https://github.com/rust-windowing/winit/commit/d1c6506865c7bddbb5fb4d80a613e43ddc1370b5.patch")
             (file-name (string-append name "-fix-bindings.patch"))
             (sha256
              (base32
               "03q4bvdq86kii53d0vsywv08g8vqirf9h1lz2cl6rcc7gjfynpds")))))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-calloop" ,rust-calloop-0.4)
        ("rust-cocoa" ,rust-cocoa-0.19)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-core-video-sys" ,rust-core-video-sys-0.1)
        ("rust-dispatch" ,rust-dispatch-0.1)
        ("rust-instant" ,rust-instant-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.10)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.6)
        ("rust-stdweb" ,rust-stdweb-0.4)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wayland-client" ,rust-wayland-client-0.23)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))
       #:cargo-development-inputs
       (("rust-console-log" ,rust-console-log-0.1)
        ("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-image" ,rust-image-0.21))))))

(define-public rust-winit-0.19
  (package
    (inherit rust-winit-0.20)
    (name "rust-winit")
    (version "0.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a4lnfyvlc4jabhs30wlmkgdjv7qhbplmyp833kl7ykjni5yp5hy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-cocoa" ,rust-cocoa-0.18)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-image" ,rust-image-0.21)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-smithay-client-toolkit" ,rust-smithay-client-toolkit-0.4)
        ("rust-wayland-client" ,rust-wayland-client-0.21)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2))))))

(define-public rust-x11-2
  (package
    (name "rust-x11")
    (version "2.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wz7l6dlbraa9zalh9i45v9wibvkir9m2m1sg0jnzcbcaj9d1v3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/erlepereira/x11-rs")
    (synopsis "X11 library bindings for Rust")
    (description "This crate provides X11 library bindings for Rust.")
    (license license:expat)))

(define-public rust-x11-clipboard-0.5
  (package
    (name "rust-x11-clipboard")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-clipboard" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17c5yxxhknrp7y9mc7mp85ra8q4jw12c174m9yzbfr1vs2pkgsg5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-xcb" ,rust-xcb-0.9))))
    (home-page "https://github.com/quininer/x11-clipboard")
    (synopsis "x11 clipboard support for Rust")
    (description "This package provides x11 clipboard support for Rust.")
    (license license:expat)))

(define-public rust-x11-clipboard-0.4
  (package
    (inherit rust-x11-clipboard-0.5)
    (name "rust-x11-clipboard")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-clipboard" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nqdnswiyj28b1izjp5rzbc67cxpb5c8p4vh1xyndkirzs84vqqk"))))
    (arguments
     `(#:tests? #f  ; Tests require display server.
       #:cargo-inputs (("rust-xcb" ,rust-xcb-0.9))))
    (native-inputs
     `(("python" ,python)))))

(define-public rust-x11-dl-2
  (package
    (name "rust-x11-dl")
    (version "2.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-dl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y7yq4sfvv56shk4v3s7gvlrwk9d0migj622fl4i4c5klpiq3y9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-maybe-uninit" ,rust-maybe-uninit-2)
        ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/erlepereira/x11-rs.git")
    (synopsis "X11 library bindings for Rust")
    (description "This package provides X11 library bindings for Rust.")
    (license license:cc0)))

(define-public rust-y4m-0.7
  (package
    (name "rust-y4m")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "y4m" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bhdgb7hgx7j92nm6ij5n8wisp50j8ff66ks14jzwdw2mwhrjam7"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/image-rs/y4m")
    (synopsis "YUV4MPEG2 (@file{.y4m}) encoder and decoder")
    (description
     "This package provides a YUV4MPEG2 (@file{.y4m}) encoder and decoder.")
    (license license:expat)))

(define-public rust-y4m-0.5
  (package
    (inherit rust-y4m-0.7)
    (name "rust-y4m")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "y4m" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1933677by64y06zfgip2yq8b2dza8xnljhaksx93czq90b54kscz"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-resize" ,rust-resize-0.3))))))
