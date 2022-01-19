;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020–2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.ccom>
;;; Copyright © 2021, 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
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

(define-module (gnu packages rust-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg))

(define-public agate
  (package
    (name "agate")
    (version "2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "agate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mhrl4j5r6wzsnwpqsbgzny5vqschyjm3gnk4y88har7skk7j19v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-configparser" ,rust-configparser-2)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-getopts" ,rust-getopts-0.2)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.22)
        ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/mbrubeck/agate")
    (synopsis "Very simple server for the Gemini hypertext protocol")
    (description
     "Agate is a server for the Gemini network protocol, built with the Rust
programming language.  It has very few features, and can only serve static
files.  It uses async I/O, and should be quite efficient even when running on
low-end hardware and serving many concurrent requests.")
    (license (list license:expat license:asl2.0))))

(define-public alfis
  (package
    (name "alfis")
    (version "0.6.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Revertron/Alfis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kgzz92mkyzn7mbsdpik1q21kl38i4almn01k99nww3p0vgx9514"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=dns::client::tests::test_tcp_client"
         "--skip=dns::client::tests::test_udp_client")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'relax-requirements
           (lambda _
             (substitute*
                 "guix-vendor/rust-x25519-dalek-1.2.0.tar.gz/Cargo.toml"
               (("version = \"=1.3\"") "version = \"^1.3\"")))))
       #:cargo-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-simplelog" ,rust-simplelog-0.11)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
        ("rust-x25519-dalek" ,rust-x25519-dalek-1)
        ("rust-ecies-ed25519" ,rust-ecies-ed25519-0.5)
        ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.9)
        ("rust-signature" ,rust-signature-1)
        ("rust-blakeout" ,rust-blakeout-0.3)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-0.7" ,rust-rand-0.7) ;For ed25519-dalek
        ("rust-sqlite" ,rust-sqlite-0.26)
        ("rust-uuid" ,rust-uuid-0.8)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-ureq" ,rust-ureq-2)
        ("rust-lru" ,rust-lru-0.7)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-tinyfiledialogs" ,rust-tinyfiledialogs-3)
        ("rust-web-view" ,rust-web-view-0.7)
        ("rust-open" ,rust-open-2)
        ("rust-thread-priority" ,rust-thread-priority-0.4)
        ("rust-winres" ,rust-winres-0.1))
       #:cargo-development-inputs
       (("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list atk
           gtk
           glib
           pango
           webkitgtk-with-libsoup2))
    (home-page "https://github.com/Revertron/Alfis")
    (synopsis "Alternative Free Identity System")
    (description
     "This project represents a minimal blockchain without cryptocurrency,
capable of sustaining any number of domain names in a bunch of original
alternative zones.")
    (license license:agpl3+)))

(define-public bat
  (package
    (name "bat")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bvlgh2yf6f1ski86hd13lda4cr51wyyg1ycsxwjpn0dbb0a8wqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-colours" ,rust-ansi-colours-1)
        ("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-bugreport" ,rust-bugreport-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-clap" ,rust-clap-2)
        ("rust-clircle" ,rust-clircle-0.3)
        ("rust-console" ,rust-console-0.15)
        ("rust-content-inspector" ,rust-content-inspector-0.2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-encoding" ,rust-encoding-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-git2" ,rust-git2-0.13)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-grep-cli" ,rust-grep-cli-0.1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-path-abs" ,rust-path-abs-0.5)
        ("rust-regex" ,rust-regex-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-syntect" ,rust-syntect-4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-wild" ,rust-wild-2))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-nix" ,rust-nix-0.23)
        ("rust-predicates" ,rust-predicates-2)
        ("rust-serial-test" ,rust-serial-test-0.5)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-wait-timeout" ,rust-wait-timeout-0.2))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgit2 zlib))
    (home-page "https://github.com/sharkdp/bat")
    (synopsis "@command{cat} clone with syntax highlighting and git integration")
    (description
     "@command{bat} is a drop-in @command{cat} replacement featuring syntax
highlighting for a large number of languages, git integration, and automatic
paging.")
    (license (list license:expat license:asl2.0))))

(define-public diffr
  (package
    (name "diffr")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diffr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1b0mz1ki2ksxni6g49x5l5j9ijpyhc11mywvxr9i9h3nr098nc5l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; https://github.com/mookid/diffr/issues/79
       #:cargo-test-flags
       '("--release" "--"
         "--skip=tests::success"
         "--skip=test_cli::color_invalid_attribute_name"
         "--skip=test_cli::color_invalid_color_not_done"
         "--skip=test_cli::color_invalid_color_value_ansi"
         "--skip=test_cli::color_invalid_color_value_name"
         "--skip=test_cli::color_invalid_color_value_rgb"
         "--skip=test_cli::color_invalid_face_name"
         "--skip=test_cli::color_ok"
         "--skip=test_cli::color_ok_multiple"
         "--skip=test_cli::color_only_face_name"
         "--skip=test_cli::debug_flag")
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-diffr-lib" ,rust-diffr-lib-0.1)
        ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/mookid/diffr")
    (synopsis "Longest Common Sequence based diff highlighting tool")
    (description
     "This package provides an @acronym{LCS, longest common sequence} based diff
highlighting tool to ease code review from your terminal.")
    (license license:expat)))

(define-public drill
  (package
    (name "drill")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "drill" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1m73d7rzi0p5c1hn0081d2235kcyapdza7h0vqf5jhnirpnjn793"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-clap" ,rust-clap-2)
         ("rust-colored" ,rust-colored-1)
         ("rust-csv" ,rust-csv-1)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-regex" ,rust-regex-1)
         ("rust-reqwest" ,rust-reqwest-0.10)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-url" ,rust-url-2)
         ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/fcsonline/drill")
    (synopsis "HTTP load testing application")
    (description
      "Drill is a HTTP load testing application written in Rust inspired by
Ansible syntax.  Benchmark files can be written in YAML.")
    (license license:gpl3)))

(define-public dutree
  (package
    (name "dutree")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dutree" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1611h27i8fm3jndscd6w65z8z7w09nnrm61vdgs9kb8ln57gqm8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-signal-hook" ,rust-signal-hook-0.1)
        ("rust-terminal-size" ,rust-terminal-size-0.1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://ownyourbits.com/2018/03/25/analyze-disk-usage-with-dutree/")
    (synopsis "Command line tool to analyze disk usage")
    (description
     "@command{dutree} is command line tool to analyze disk usage.
Features include:
@enumerate
@item coloured output, according to the @code{LS_COLORS} environment variable.
@item display the file system tree.
@item ability to aggregate small files.
@item ability to exclude files or directories.
@item ability to compare different directories.
@item fast, written in Rust.
@end enumerate\n")
    (license license:gpl3)))

(define-public exa
  (package
    (name "exa")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "exa" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s902xgplz1167k0r7x235p914lprpsqy2if0kpa1mlb0fswqqq4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-datetime" ,rust-datetime-0.4)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-git2" ,rust-git2-0.9)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-locale" ,rust-locale-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-natord" ,rust-natord-1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-number-prefix" ,rust-number-prefix-0.3)
        ("rust-scoped-threadpool" ,rust-scoped-threadpool-0.1)
        ("rust-term-grid" ,rust-term-grid-0.1)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-users" ,rust-users-0.9)
        ("rust-zoneinfo-compiled" ,rust-zoneinfo-compiled-0.4))
       #:cargo-development-inputs
       (("rust-datetime" ,rust-datetime-0.4))
       #:phases
       (modify-phases %standard-phases
         ;; Ignoring failing tests.
         ;; Reported in https://github.com/ogham/exa/issues/318
         (add-before 'check 'disable-failing-tests
           (lambda _
             (substitute* "src/options/mod.rs"
               (("^.*fn oneline_across.*" oneline-across)
                (string-append "#[ignore]\n" oneline-across)))

             (substitute* "src/options/view.rs"
               (("test!\\(across:.*") "")
               (("test!\\(cr:.*") "")
               (("test!\\(empty:.*") "")
               (("test!\\(gracross:.*") "")
               (("test!\\(grid:.*") "")
               (("test!\\(icons:.*") "")
               (("test!\\(just_binary:.*") "")
               (("test!\\(just_blocks:.*") "")
               (("test!\\(just_bytes:.*") "")
               (("test!\\(just_git:.*") "")
               (("test!\\(just_group:.*") "")
               (("test!\\(just_header:.*") "")
               (("test!\\(just_inode:.*") "")
               (("test!\\(just_links:.*") "")
               (("test!\\(leg:.*") "")
               (("test!\\(lid:.*") "")
               (("test!\\(original_g:.*") ""))
             #t))
         (add-after 'install 'install-extras
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (man1  (string-append share "/man/man1")))
               (install-file "contrib/man/exa.1" man1)
               (mkdir-p (string-append out "/etc/bash_completion.d"))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (copy-file "contrib/completions.bash"
                          (string-append out "/etc/bash_completion.d/exa"))
               (copy-file "contrib/completions.fish"
                          (string-append share "/fish/vendor_completions.d/exa.fish"))
               (copy-file "contrib/completions.zsh"
                          (string-append share "/zsh/site-functions/_exa"))
               #t))))))
    (inputs
     (list libgit2 zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://the.exa.website/")
    (synopsis "Modern replacement for ls")
    (description "@code{exa} is a modern replacement for the command-line
program @code{ls}.  It uses colours to distinguish file types and metadata.  It
also knows about symlinks, extended attributes, and Git.")
    (license license:expat)))

(define-public fd
  (package
    (name "fd")
    (version "8.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fd-find" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "124a5r8hpk2phs1288jybh34d48yxy44wr7gv5ggchs272gs2jam"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-dirs" ,rust-dirs-2)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-jemallocator" ,rust-jemallocator-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-lscolors" ,rust-lscolors-0.7)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.6)
        ("rust-users" ,rust-users-0.10)
        ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs
       (("rust-diff" ,rust-diff-0.1)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-tempdir" ,rust-tempdir-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc.so")))
             #t))
         (add-after 'install 'install-extra
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (install-completion
                     (lambda (completion out-dir)
                       (for-each
                        (lambda (f)
                          (install-file f (string-append out out-dir)))
                        (find-files "target/release/build/" completion)))))
               ;; Manpages
               (install-file "doc/fd.1" (string-append out "/share/man/man1"))
               ;; Completions
               (install-completion "^fd.bash$" "/etc/bash_completion.d")
               (install-completion "^fd.fish$" "/share/fish/vendor_completions.d")
               (install-completion "^_fd$" "/share/zsh/site-functions")
               (rename-file (string-append out "/etc/bash_completion.d/fd.bash")
                            (string-append out "/etc/bash_completion.d/fd"))
               #t))))))
    (inputs (list jemalloc))
    (home-page "https://github.com/sharkdp/fd")
    (synopsis "Simple, fast and user-friendly alternative to find")
    (description
     "@code{fd} is a simple, fast and user-friendly alternative to @code{find}.
While it does not seek to mirror all of find's powerful functionality, it
provides defaults for 80% of the use cases.")
    (license (list license:expat license:asl2.0))))

(define-public hexyl
  (package
    (name "hexyl")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hexyl" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sipag77196467idbznbk5q5lwhqz85zw7y1pwg9b27jxqyk04rp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/sharkdp/hexyl")
    (synopsis "Command-line hex viewer")
    (description
     "This package provides a command line hex viewer.  It uses a colored output
for distinguishing different kinds of bytes such as NULL bytes, printable ASCII
characters, ASCII whitespace characters, other ASCII characters and non-ASCII.")
    (license (list license:expat license:asl2.0))))

(define-public hyperfine
  (package
    (name "hyperfine")
    (version "1.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyperfine" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0m5lrvx6wwkxqdc5digm1k4diiaqcg5j4pia77s5nw1aam7k51hy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-clap" ,rust-clap-2)
        ("rust-colored" ,rust-colored-2)
        ("rust-csv" ,rust-csv-1)
        ("rust-indicatif" ,rust-indicatif-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rust-decimal" ,rust-rust-decimal-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-statistical" ,rust-statistical-1)
        ("rust-version-check" ,rust-version-check-0.9)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3))
       #:phases
       (modify-phases %standard-phases
        (add-after 'install 'install-more
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out   (assoc-ref outputs "out"))
                   (share (string-append out "/share/"))
                   (man   (string-append share "man/man1"))
                   (bash  (string-append share "bash-completion/completions"))
                   (fish  (string-append share "fish/vendor_completions.d"))
                   (zsh   (string-append share "zsh/site-functions")))
              (install-file "doc/hyperfine.1" man)
              (for-each (cut install-file <> bash)
                        (find-files "target/release/build" "^hyperfine.bash$"))
              (rename-file (string-append bash "/hyperfine.bash")
                           (string-append bash "/hyperfine"))
              (for-each (cut install-file <> fish)
                        (find-files "target/release/build" "^hyperfine.fish$"))
              (for-each (cut install-file <> zsh)
                        (find-files "target/release/build" "^_hyperfine$"))))))))
    (home-page "https://github.com/sharkdp/hyperfine")
    (synopsis "Command-line benchmarking tool")
    (description
     "This package provides a command-line benchmarking tool.")
    (license (list license:expat license:asl2.0))))

(define-public i3status-rust
  (package
    (name "i3status-rust")
    (version "0.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/greshake/i3status-rust")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "i3status-rust-enable-unstable-features.patch"))
       (sha256
        (base32 "00gzm3g297s9bfp13vnb623p7dfac3g6cdhz2b3lc6l0kmnnqs1s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '("pulseaudio" "libpulse-binding")
       #:install-source? #f
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-chrono-tz" ,rust-chrono-tz-0.5)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-dbus" ,rust-dbus-0.9)
        ("rust-dbus-tree" ,rust-dbus-tree-0.9)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-nix" ,rust-nix-0.20)
        ("rust-nl80211" ,rust-nl80211-0.0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-swayipc" ,rust-swayipc-2)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-cpuprofiler" ,rust-cpuprofiler-0.0)
        ("rust-inotify" ,rust-inotify-0.9)
        ("rust-libpulse-binding" ,rust-libpulse-binding-2)
        ("rust-maildir" ,rust-maildir-0.5)
        ("rust-notmuch" ,rust-notmuch-0.6)
        ("rust-progress" ,rust-progress-0.2))
       #:cargo-development-inputs
       (("rust-assert-fs" ,rust-assert-fs-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enable-unstable-features
           (lambda _
             (setenv "RUSTC_BOOTSTRAP" "1")))
         (add-after 'unpack 'fix-resources-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (substitute* "src/util.rs"
                 (("/usr/share/i3status-rust") share)))))
         (add-after 'install 'install-resources
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "files" (string-append out "/share")))))
         (add-after 'install 'wrap-i3status
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map
                           (lambda (input)
                             (string-append (assoc-ref inputs input) "/bin"))
                           '("alsa-utils" "coreutils" "curl" "dbus" "ibus" "iproute"
                             "kdeconnect" "lm-sensors" "pulseaudio"
                             "openssl"
                             "setxkbmap" "speedtest-cli" "xdg-utils" "xrandr"
                             "zlib"))))
               (wrap-program (string-append out "/bin/i3status-rs")
                 `("PATH" prefix ,paths))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("alsa-utils" ,alsa-utils)
       ("bash-minimal" ,bash-minimal)
       ("coreutils" ,coreutils)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("ibus" ,ibus)
       ("iproute" ,iproute)
       ("kdeconnect" ,kdeconnect)
       ("lm-sensors" ,lm-sensors)
       ("pulseaudio" ,pulseaudio)
       ("openssl" ,openssl)
       ("setxkbmap" ,setxkbmap)
       ("speedtest-cli" ,speedtest-cli)
       ("xdg-utils" ,xdg-utils)
       ("xrandr" ,xrandr)
       ("zlib" ,zlib)))
    (home-page "https://github.com/greshake/i3status-rust")
    (synopsis "i3status, written in pure Rust")
    (description "@code{i3status-rs} is a feature-rich and resource-friendly
replacement for i3status, written in pure Rust.  It provides a way to display
@code{blocks} of system information (time, battery status, volume, etc) on the i3
bar.  It is also compatible with sway.")
    (license license:gpl3)))

(define-public ripgrep
  (package
    (name "ripgrep")
    (version "13.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ripgrep" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gv4imhjgxmyxaa996yshcjlakmrjw9pf4rycp90pq675cn9sz7k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-grep" ,rust-grep-0.2)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-jemallocator" ,rust-jemallocator-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-termcolor" ,rust-termcolor-1))
       #:cargo-development-inputs
       (("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-walkdir" ,rust-walkdir-2))
       #:modules ((ice-9 match)
                  (guix build cargo-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-manpage
           ;; NOTE: This is done before 'check so that there's only one output
           ;; directory with the man page.
           (lambda* (#:key outputs #:allow-other-keys)
             (match (find-files "target" "^rg\\.1$")
               ((manpage)
                (install-file manpage (string-append
                                       (assoc-ref outputs "out")
                                       "/share/man/man1"))))
             #t)))
       #:features '("pcre2")))
    (native-inputs
     (list asciidoc pcre2 pkg-config))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Line-oriented search tool")
    (description
     "ripgrep is a line-oriented search tool that recursively searches
your current directory for a regex pattern while respecting your
gitignore rules.")
    (license (list license:unlicense license:expat))))

(define-public git-interactive-rebase-tool
  (package
    (name "git-interactive-rebase-tool")
    (version "2.1.0")
    (source
     (origin
       ;; crates.io does not provide the test data.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitmaro/git-interactive-rebase-tool")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "173spqqpyc00kvfmldjmjfqizh9b4spq4xw4bskd4dny8qcpz28d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       ;; https://github.com/MitMaro/git-interactive-rebase-tool/issues/586
       '("--release" "--" "--skip=tests::success")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-crossterm" ,rust-crossterm-0.19)
        ("rust-git2" ,rust-git2-0.13)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-xi-unicode" ,rust-xi-unicode-0.3))
       #:cargo-development-inputs
       (("rust-concat-idents" ,rust-concat-idents-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rstest" ,rust-rstest-0.6)
        ("rust-serial-test" ,rust-serial-test-0.5)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list zlib))
    (home-page "https://gitrebasetool.mitmaro.ca/")
    (synopsis "Terminal based sequence editor for git interactive rebase")
    (description
     "This application is a terminal-based sequence editor for git interactive
rebase.")
    (license license:gpl3+)))

(define-public rust-cbindgen
  (package
    (name "rust-cbindgen")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name (string-append name "-" version ".tar.xz"))
        (sha256
         (base32
          "0673pq96hs7waavkv58v2pakpxpsfyjvbraa5kyl2b44phgdzcid"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("clap" ,rust-clap-2)
        ("log" ,rust-log-0.4)
        ("proc-macro2" ,rust-proc-macro2-1)
        ("quote" ,rust-quote-1)
        ("serde" ,rust-serde-1)
        ("serde-json" ,rust-serde-json-1)
        ("syn" ,rust-syn-1)
        ("tempfile" ,rust-tempfile-3)
        ("toml" ,rust-toml-0.5))))
    (home-page "https://github.com/eqrion/cbindgen/")
    (synopsis "Tool for generating C bindings to Rust code")
    (description
     "This package provides a tool for generating C/C++ bindings to Rust code.")
    (license license:mpl2.0)))

(define-public rust-cbindgen-0.19
  (package
    (inherit rust-cbindgen)
    (name "rust-cbindgen")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yld9fni9g9mzg4r42zfk79aq9mzm2sfzzjrrx4vir4lp4qqqwiq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-serial-test" ,rust-serial-test-0.5))))
    (native-inputs
     (list python-cython))))

(define-public rust-cbindgen-0.16
  (package
    (inherit rust-cbindgen)
    (name "rust-cbindgen")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "049cai626nzw0km03parx4sxwaxgbr7i5ifjbjwnfxkqkj5k2i4k"))))
    (arguments
     `(#:tests? #false                  ;missing files
       #:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-serial-test" ,rust-serial-test-0.5))))))

(define-public rust-cbindgen-0.15
  (package
    (inherit rust-cbindgen)
    (name "rust-cbindgen")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0dgf49zij9rxnf0lv4k5gcmx1mxcz16czkk6q63anz0xp8ds3xhx"))))
    (arguments
     `(#:tests? #false                  ;missing files
       #:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-toml" ,rust-toml-0.5))))))

(define-public rust-cbindgen-0.14
  (package
    (inherit rust-cbindgen)
    (name "rust-cbindgen")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ppwqbzydxlg9a24lynzfk60xrvqw4k31mpz1wrk6lbf88zf8nxi"))))))

(define-public rust-cbindgen-0.12
  (package
    (inherit rust-cbindgen)
    (name "rust-cbindgen")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "13jzbmjz1bmmfr0i80hw6ar484mgabx3hbpb2ynhk0ddqi0yr58m"))))))

(define-public sniffglue
  (package
    (name "sniffglue")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sniffglue" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q1kwkw1hq38qgvc6j4b5l9m85a6lpn1jls4bm27c5kha9cg8l24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-dhcp4r" ,rust-dhcp4r-0.2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-dns-parser" ,rust-dns-parser-0.8)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.20)
        ("rust-nom" ,rust-nom-6)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-pcap-sys" ,rust-pcap-sys-0.1)
        ("rust-pktparse" ,rust-pktparse-0.5)
        ("rust-reduce" ,rust-reduce-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-syscallz" ,rust-syscallz-0.15)
        ("rust-tls-parser" ,rust-tls-parser-0.10)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-users" ,rust-users-0.11))
       #:cargo-development-inputs
       (("rust-boxxy" ,rust-boxxy-0.11))))
    (inputs
     (list libpcap libseccomp))
    (home-page "https://github.com/kpcyrd/sniffglue")
    (synopsis "Secure multithreaded packet sniffer")
    (description
     "This package provides a network sniffer written in Rust.  Packets
are parsed concurrently using a thread pool to utilize all cpu cores.  A goal
of the project is to be runnable on untrusted networks without crashing.")
    (license license:gpl3)))

(define-public spotify-tui-0.25
  (package
    (name "spotify-tui")
    (version "0.25.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spotify-tui" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "08bpihkdv3rmcksnxp4cz04kawjs6spmwa3wr2k27b30x3q9cd4r"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-anyhow" ,rust-anyhow-1)
         ("rust-arboard" ,rust-arboard-1)
         ("rust-backtrace" ,rust-backtrace-0.3)
         ("rust-clap" ,rust-clap-2)
         ("rust-crossterm" ,rust-crossterm-0.20)
         ("rust-dirs" ,rust-dirs-3)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-rspotify" ,rust-rspotify-0.10)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-serde-yaml" ,rust-serde-yaml-0.8)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-tui" ,rust-tui-0.16)
         ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://github.com/Rigellute/spotify-tui")
    (synopsis "Terminal user interface for Spotify")
    (description "This package provides a terminal user interface for Spotify")
    (license (list license:expat license:asl2.0))))

(define-public tectonic
  (package
    (name "tectonic")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tectonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hzyqpjxya6g1ifb3hvjvj0zl2aigx898pz7h5pl46z50jp2pdc8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-build-flags '("--release" "--features" "external-harfbuzz")
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-error-chain" ,rust-error-chain-0.12)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-open" ,rust-open-1)
        ("rust-quick-xml" ,rust-quick-xml-0.22)
        ("rust-serde" ,rust-serde-1)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tectonic-bridge-core" ,rust-tectonic-bridge-core-0.3)
        ("rust-tectonic-bundles" ,rust-tectonic-bundles-0.2)
        ("rust-tectonic-docmodel" ,rust-tectonic-docmodel-0.1)
        ("rust-tectonic-engine-bibtex" ,rust-tectonic-engine-bibtex-0.1)
        ("rust-tectonic-engine-xdvipdfmx" ,rust-tectonic-engine-xdvipdfmx-0.1)
        ("rust-tectonic-engine-xetex" ,rust-tectonic-engine-xetex-0.1)
        ("rust-tectonic-errors" ,rust-tectonic-errors-0.2)
        ("rust-tectonic-geturl" ,rust-tectonic-geturl-0.3)
        ("rust-tectonic-io-base" ,rust-tectonic-io-base-0.3)
        ("rust-tectonic-status-base" ,rust-tectonic-status-base-0.2)
        ("rust-tectonic-xdv" ,rust-tectonic-xdv-0.1)
        ("rust-tectonic-xetex-layout" ,rust-tectonic-xetex-layout-0.1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-url" ,rust-url-2)
        ("rust-watchexec" ,rust-watchexec-1)
        ("rust-zip" ,rust-zip-0.5))
       #:cargo-development-inputs
       (("rust-filetime" ,rust-filetime-0.2)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-headers" ,rust-headers-0.2)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-tokio" ,rust-tokio-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (copy-recursively "docs/src" doc)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("harfbuzz" ,harfbuzz)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://tectonic-typesetting.github.io/")
    (synopsis "Complete, embeddable TeX/LaTeX engine")
    (description
     "This package provides a modernized, complete, embeddable
TeX/LaTeX engine.  Tectonic is forked from the XeTeX extension to the
classic Web2C implementation of TeX and uses the TeXLive distribution
of support files.")
    (license license:expat)))

(define-public hex
  (package
    (name "hex")
    (version "0.4.2")
    (source
     (origin
       ;; crates.io does not provide the test data.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sitkevij/hex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03x27nixdlnkkrh85gy4152arp02kpjwq0i9dn9p73lyr24s64lv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-no-color" ,rust-no-color-0.1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1))))
    (home-page "https://github.com/sitkevij/hex")
    (synopsis "Hexadecimal colorized view of a file")
    (description
     "@command{hx} accepts a file path as input and outputs a hexadecimal
colorized view to stdout.")
    (license license:expat)))

(define-public tokei
  (package
    (name "tokei")
    (version "12.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokei" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "000w549v1bpw7r57xw656p40ywf1gimvxxx5cjnri2js0xg927x4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-0.7)
        ("rust-clap" ,rust-clap-2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-dirs" ,rust-dirs-3)
        ("rust-encoding-rs-io" ,rust-encoding-rs-io-0.1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-grep-searcher" ,rust-grep-searcher-0.1)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-tera" ,rust-tera-1)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-git2" ,rust-git2-0.13)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgit2 openssl zlib))
    (home-page "https://tokei.rs")
    (synopsis "Count code, quickly")
    (description
     "Tokei is a program that displays statistics about your code.  Tokei will
show number of files, total lines within those files and code, comments, and
blanks grouped by language.")
    (license (list license:expat license:asl2.0))))

(define-public vivid
  (package
    (name "vivid")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vivid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01fds6dm19bqgqydaa6n051v9l4wh9rb5d6sr9akwp2cc0fs43b7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-colours" ,rust-ansi-colours-1)
        ("rust-clap" ,rust-clap-2)
        ("rust-dirs" ,rust-dirs-3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rust-embed" ,rust-rust-embed-5)
        ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (home-page "https://github.com/sharkdp/vivid")
    (synopsis "LS_COLORS environment variable manager")
    (description
     "vivid is a generator for the @code{LS_COLORS} environment variable that
controls the colorized output of ls, tree, fd, bfs, dust and many other tools.

It uses a YAML configuration format for the filetype-database and the color
themes.  In contrast to @command{dircolors}, the database and the themes are
organized in different files.  This allows users to choose and customize color
themes independent from the collection of file extensions.  Instead of using
cryptic ANSI escape codes, colors can be specified in the RRGGBB format and
will be translated to either truecolor (24-bit) ANSI codes or 8-bit codes for
older terminal emulators.")
    (license (list license:expat license:asl2.0))))

(define-public watchexec
  (package
    (name "watchexec")
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "watchexec-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wp424gzw1zmax5yy5gya15knl24rjx8gi9c7palvq807q3cnj65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh (string-append out "/share/zsh/site-functions/_watchexec"))
                    (doc (string-append out "/share/doc/watchexec-" ,version)))
               (mkdir-p (dirname zsh))
               ;; FIXME: The crates.io source does not provide zsh
               ;; completions.  But the GitHub source does not compile.
               ;;
               ;; (copy-file "completions/zsh" zsh)
               (install-file "README.md" doc)))))
       #:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-embed-resource" ,rust-embed-resource-1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-watchexec" ,rust-watchexec-1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-insta" ,rust-insta-1))))
    (home-page "https://github.com/watchexec/watchexec")
    (synopsis "Executes commands in response to file modifications")
    (description
     "@command{watchexec} is a simple, standalone tool that watches a path and
runs a command whenever it detects modifications.")
    (license license:asl2.0)))

(define-public rbw
  (package
    (name "rbw")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rbw" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zszp9hvilpikbd66b5zbvspks0spv8dh0yry0sxnc5yqvl2ixnf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'relax-requirements
           (lambda _
             (substitute*
                 "guix-vendor/rust-password-hash-0.3.2.tar.gz/Cargo.toml"
               (("version = \">=1, <1.1.0\"") "version = \">=1\""))
             (substitute*
                 "guix-vendor/rust-rsa-0.5.0.tar.gz/Cargo.toml"
               (("version = \">=1, <1.5\"") "version = \"^1\""))
             (substitute*
                 "Cargo.toml"
               (("version = \"1.4\"") "version = \"^1\"")))))
       #:cargo-inputs
       (("rust-aes" ,rust-aes-0.7)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base32" ,rust-base32-0.4)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-block-modes" ,rust-block-modes-0.8)
        ("rust-block-padding" ,rust-block-padding-0.2)
        ("rust-daemonize" ,rust-daemonize-0.4)
        ("rust-directories" ,rust-directories-4)
        ("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-hkdf" ,rust-hkdf-0.11)
        ("rust-hmac" ,rust-hmac-0.11)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.23)
        ("rust-paw" ,rust-paw-1)
        ("rust-pbkdf2" ,rust-pbkdf2-0.9)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-region" ,rust-region-3)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-rsa" ,rust-rsa-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
        ("rust-serde-repr" ,rust-serde-repr-0.1)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-textwrap" ,rust-textwrap-0.11)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-totp-lite" ,rust-totp-lite-1)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-0.8)
        ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://git.tozt.net/rbw")
    (synopsis "Unofficial Bitwarden CLI")
    (description "This package is an unofficial command line client for
Bitwarden.  Although Bitwarden ships with a command line client, but
it's limited by being stateless, which makes it very difficult to use.  This
client avoids that problem by maintaining a background process which is able
to hold the keys in memory, similar to the way that ssh-agent or gpg-agent
work.  This allows the client to be used in a much simpler way, with the
background agent taking care of maintaining the necessary state.")
    (license license:expat)))

(define-public rust-analyzer
  (package
    (name "rust-analyzer")
    (version "2022-01-10")
    (source
     (origin
       ;; The crate at "crates.io" is empty.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rust-analyzer/rust-analyzer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ci85bp8xwqrk8nqr8sh6yj8njgd98nhgnhaks2g00c77wwyra41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f             ; virtual manifest
       #:cargo-test-flags
       '("--release" "--"
         "--skip=tests::test_version_check" ;it need rustc's version
         ;; FIXME: Guix's rust does not install source in
         ;; %out/lib/rustlib/src/rust so "can't load standard library from
         ;; sysroot"
         "--skip=tests::test_loading_rust_analyzer"
         ;; Failed to run rustfmt from toolchain 'stable'.  Please run `rustup
         ;; component add rustfmt --toolchain stable` to install it
         "--skip=tests::sourcegen::sourcegen_assists_docs" ;need rustfmt
         "--skip=tests::sourcegen_ast::sourcegen_ast"      ;same

         "--skip=tidy::cargo_files_are_tidy"    ;not needed
         "--skip=tidy::check_licenses"          ;it runs cargo metadata
         "--skip=tidy::check_merge_commits"     ;it runs git rev-list
         "--skip=tidy::check_code_formatting"   ;need rustfmt as cargo fmt
         "--skip=tidy::generate_grammar"        ;same
         "--skip=tidy::generate_assists_tests") ;same
       #:cargo-development-inputs
       (("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-derive-arbitrary" ,rust-derive-arbitrary-1)
        ("rust-expect-test" ,rust-expect-test-1)
        ("rust-oorandom" ,rust-oorandom-11.1)
        ("rust-quote" ,rust-quote-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-tracing-tree" ,rust-tracing-tree-0.2)
        ("rust-ungrammar" ,rust-ungrammar-1))
       #:cargo-inputs
       (("rust-always-assert" ,rust-always-assert-0.1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-anymap" ,rust-anymap-0.12)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.14)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-chalk-ir" ,rust-chalk-ir-0.75)
        ("rust-chalk-recursive" ,rust-chalk-recursive-0.75)
        ("rust-chalk-solve" ,rust-chalk-solve-0.75)
        ("rust-countme" ,rust-countme-3)
        ("rust-cov-mark" ,rust-cov-mark-2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-dissimilar" ,rust-dissimilar-1)
        ("rust-dot" ,rust-dot-0.1)
        ("rust-drop-bomb" ,rust-drop-bomb-0.1)
        ("rust-either" ,rust-either-1)
        ("rust-ena" ,rust-ena-0.14)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-fst" ,rust-fst-0.4)
        ("rust-home" ,rust-home-0.5)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-jod-thread" ,rust-jod-thread-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-lsp-server" ,rust-lsp-server-0.5)
        ("rust-lsp-types" ,rust-lsp-types-0.91)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-mimalloc" ,rust-mimalloc-0.1)
        ("rust-miow" ,rust-miow-0.4)
        ("rust-notify" ,rust-notify-5)
        ("rust-object" ,rust-object-0.28)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-perf-event" ,rust-perf-event-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.8)
        ("rust-pulldown-cmark-to-cmark" ,rust-pulldown-cmark-to-cmark-7)
        ("rust-rowan" ,rust-rowan-0.15)
        ("rust-rustc-ap-rustc-lexer" ,rust-rustc-ap-rustc-lexer-725)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-salsa" ,rust-salsa-0.17)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
        ("rust-typed-arena" ,rust-typed-arena-2)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-smol-str" ,rust-smol-str-0.1)
        ("rust-snap" ,rust-snap-1)
        ("rust-text-size" ,rust-text-size-1)
        ("rust-threadpool" ,rust-threadpool-1)
        ("rust-tikv-jemalloc-ctl" ,rust-tikv-jemalloc-ctl-0.4)
        ("rust-tikv-jemallocator" ,rust-tikv-jemallocator-0.4)
        ("rust-url" ,rust-url-2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-write-json" ,rust-write-json-0.1)
        ("rust-xflags" ,rust-xflags-0.2)
        ("rust-xshell" ,rust-xshell-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (let ((bash (string-append "#!" (which "bash"))))
               (with-directory-excursion "crates/parser/test_data/lexer/ok"
                 (substitute* "single_line_comments.txt"
                   (("SHEBANG 19")
                    (string-append "SHEBANG "
                                   (number->string (string-length bash))))
                   (("#!/usr/bin/env bash") bash))))))
         (add-before 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/rust-analyzer-"
                                        ,version)))
               (copy-recursively "docs" doc))))
         (add-before 'install 'chdir
           (lambda _
             (chdir "crates/rust-analyzer")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (rust-src-path (search-input-directory
                                    inputs "/lib/rustlib/src/rust/library")))
               ;; if environment variable RUST_SRC_PATH is not set, set it,
               ;; make rust-analyzer work out of box.
               (with-directory-excursion bin
                 (let* ((prog "rust-analyzer")
                        (wrapped-file (string-append (dirname prog)
                                                     "/." (basename prog) "-real"))
                        (prog-tmp (string-append wrapped-file "-tmp")))
                   (link prog wrapped-file)
                   (call-with-output-file prog-tmp
                     (lambda (port)
                       (format port "#!~a
if test -z \"${RUST_SRC_PATH}\";then export RUST_SRC_PATH=~S;fi;
exec -a \"$0\" \"~a\" \"$@\""
                               (which "bash")
                               rust-src-path
                               (canonicalize-path wrapped-file))))
                   (chmod prog-tmp #o755)
                   (rename-file prog-tmp prog))))))
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/rust-analyzer-"
                                        ,version)))
               (chdir "../..")
               (install-file "LICENSE-MIT" doc)
               (install-file "LICENSE-APACHE" doc)))))))
    (native-inputs (list rust-src))
    (home-page "https://rust-analyzer.github.io/")
    (synopsis "Experimental Rust compiler front-end for IDEs")
    (description "Rust-analyzer is a modular compiler frontend for the Rust
language.  It is a part of a larger rls-2.0 effort to create excellent IDE
support for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-c
  (package
    (name "rust-cargo-c")
    (version "0.8.1+cargo-0.53")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-c" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0fwdxhdj2963xr6xfqr56i7hikhsdv562vgxq2dj3h2mi3dil1k6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cbindgen" ,rust-cbindgen-0.19)
        ("rust-cargo" ,rust-cargo-0.53) ;
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.9)
        ("rust-semver" ,rust-semver-0.10)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-regex" ,rust-regex-1))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl libssh2 openssl zlib))
    (home-page "https://github.com/lu-zero/cargo-c")
    (synopsis "Build and install C-compatible libraries")
    (description
     "This package produces and installs a correct pkg-config file, a static
library and a dynamic library, and a C header to be used by any C (and
C-compatible) software.")
    (license license:expat)))

(define-public swayhide
  (package
    (name "swayhide")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swayhide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x172ffj0lfmbv5nix708l1mfsizxzy74gpxp5amvx0bbaq0p78s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-exitcode" ,rust-exitcode-1)
        ("rust-swayipc" ,rust-swayipc-2))))
    (home-page "https://github.com/NomisIV/swayhide/")
    (synopsis "Swallow windows on swaywm")
    (description "swayhide hides the currently active terminal (by moving it
to the scratchpad), then it executes the supplied command.  When the child
process has finished, the terminal is moved back.  This is useful if your
workflow includes opening graphical programs from the terminal, as the locked
terminal won't have to take up any space.")
    (license license:gpl3+)))

(define-public tealdeer
  (package
    (name "tealdeer")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tealdeer" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cwf46k2rszcpydrqajnm4dvhggr3ms7sjma0jx02ch4fjicxch7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/"))
                    (fish (string-append out "/share/fish/vendor_completions.d/")))
               (mkdir-p bash)
               (mkdir-p fish)
               (copy-file "bash_tealdeer"
                          (string-append bash "tealdeer"))
               (copy-file "fish_tealdeer"
                          (string-append fish "tealdeer.fish"))))))
       #:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         ;; These tests go to the network
         "--skip=test_quiet_old_cache"
         "--skip=test_quiet_cache"
         "--skip=test_quiet_failures"
         "--skip=test_pager_flag_enable"
         "--skip=test_markdown_rendering"
         "--skip=test_spaces_find_command"
         "--skip=test_autoupdate_cache"
         "--skip=test_update_cache")
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-app-dirs2" ,rust-app-dirs2-2)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-docopt" ,rust-docopt-1)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-pager" ,rust-pager-0.15)
        ("rust-reqwest" ,rust-reqwest-0.10)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-xdg" ,rust-xdg-2))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-escargot" ,rust-escargot-0.5)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-predicates" ,rust-predicates-1)
        ;; This earlier version is required to fix a bug.
        ;; Remove rust-remove-dir-all-0.5.2 when tealdeer gets upgraded
        ("rust-remove-dir-all" ,rust-remove-dir-all-0.5.2)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/dbrgn/tealdeer/")
    (synopsis "Fetch and show tldr help pages for many CLI commands")
    (description
     "This package fetches and shows tldr help pages for many CLI commands.
Full featured offline client with caching support.")
    (license (list license:expat license:asl2.0))))

(define-public git-absorb
  (package
    (name "git-absorb")
    (version "0.6.6")
    (source
     (origin
       ;; crates.io does not include the manual page.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tummychow/git-absorb")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04v10bn24acify34vh5ayymsr1flcyb05f3az9k1s2m6nlxy5gb9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-clap" ,rust-clap-2)
        ("rust-git2" ,rust-git2-0.13)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-slog" ,rust-slog-2)
        ("rust-slog-async" ,rust-slog-async-2)
        ("rust-slog-term" ,rust-slog-term-2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-version-requirements
           (lambda _
             (substitute* "Cargo.toml"
               (("2.5") "2")
               (("~2.3\"") "2\"")
               (("~2.33\"") "2\"")      ; clap
               (("3.1") "3"))))
         (add-after 'install 'install-manual-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (man   (string-append out "/share/man/man1")))
               (install-file "Documentation/git-absorb.1" man)))))))
    (inputs
     (list zlib))
    (home-page "https://github.com/tummychow/git-absorb")
    (synopsis "Git tool for making automatic fixup commits")
    (description
     "@code{git absorb} automatically absorbs staged changes into their
current branch.  @code{git absorb} will automatically identify which commits
are safe to modify, and which staged changes belong to each of those commits.
It will then write @code{fixup!} commits for each of those changes.")
    (license license:bsd-3)))

(define-public zoxide
  (package
    (name "zoxide")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zoxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ih01l3xp8plicxhmyxjkq12ncpdb8954jcj3dh3lwvkhvw29nkk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-askama" ,rust-askama-0.10)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-clap" ,rust-clap-3)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-ordered-float" ,rust-ordered-float-2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-serde" ,rust-serde-1)
        ("rust-tempfile" ,rust-tempfile-3))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-seq-macro" ,rust-seq-macro-0.2))))
    (home-page "https://github.com/ajeetdsouza/zoxide/")
    (synopsis "Fast way to navigate your file system")
    (description
     "Zoxide is a fast replacement for your @command{cd} command.  It keeps
track of the directories you use most frequently, and uses a ranking algorithm
to navigate to the best match.")
    (license license:expat)))
