;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.ccom>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

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

(define-public bat
  (package
    (name "bat")
    (version "0.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0qlk032dd6zxda1v7clah33nafxygaw3x7f73ajwlvk956nrn1js"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-colours" ,rust-ansi-colours-1)
        ("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-bugreport" ,rust-bugreport-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-clircle" ,rust-clircle-0.3)
        ("rust-console" ,rust-console-0.14)
        ("rust-content-inspector" ,rust-content-inspector-0.2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-encoding" ,rust-encoding-0.2)
        ("rust-error-chain" ,rust-error-chain-0.12)
        ("rust-git2" ,rust-git2-0.13)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-grep-cli" ,rust-grep-cli-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-path-abs" ,rust-path-abs-0.5)
        ("rust-semver" ,rust-semver-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-syntect" ,rust-syntect-4)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-wild" ,rust-wild-2))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-nix" ,rust-nix-0.21)
        ("rust-predicates" ,rust-predicates-1)
        ("rust-serial-test" ,rust-serial-test-0.5)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-wait-timeout" ,rust-wait-timeout-0.2))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libgit2" ,libgit2)
       ("zlib" ,zlib)))
    (home-page "https://github.com/sharkdp/bat")
    (synopsis "@command{cat} clone with syntax highlighting and git integration")
    (description
     "@command{bat} is a drop-in @command{cat} replacement featuring syntax
highlighting for a large number of languages, git integration, and automatic
paging.")
    (license (list license:expat license:asl2.0))))

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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://github.com/fcsonline/drill")
    (synopsis "HTTP load testing application")
    (description
      "Drill is a HTTP load testing application written in Rust inspired by
Ansible syntax.  Benchmark files can be written in YAML.")
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
     `(("libgit2" ,libgit2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (inputs `(("jemalloc" ,jemalloc)))
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
     `(#:rust ,rust-1.46
       #:modules ((guix build cargo-build-system)
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
     `(("asciidoc" ,asciidoc)
       ("pcre2" ,pcre2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Line-oriented search tool")
    (description
     "ripgrep is a line-oriented search tool that recursively searches
your current directory for a regex pattern while respecting your
gitignore rules.")
    (license (list license:unlicense license:expat))))

(define-public rust-cbindgen
  (package
    (name "rust-cbindgen")
    (version "0.13.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name (string-append name "-" version ".crate"))
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
     `(("python-cython" ,python-cython)))))

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
     `(("libpcap" ,libpcap)
       ("libseccomp" ,libseccomp)))
    (home-page "https://github.com/kpcyrd/sniffglue")
    (synopsis "Secure multithreaded packet sniffer")
    (description
     "This package provides a network sniffer written in Rust.  Packets
are parsed concurrently using a thread pool to utilize all cpu cores.  A goal
of the project is to be runnable on untrusted networks without crashing.")
    (license license:gpl3)))

(define-public tectonic
  (package
    (name "tectonic")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tectonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rjkfmbam81anpdqs2qafcmd5bf7y898c8a7iqqqwkbl1hfw4sqs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-1.52
       #:cargo-build-flags '("--release" "--features" "external-harfbuzz")
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
        ("rust-tectonic-bridge-core" ,rust-tectonic-bridge-core-0.2)
        ("rust-tectonic-bundles" ,rust-tectonic-bundles-0.1)
        ("rust-tectonic-docmodel" ,rust-tectonic-docmodel-0.1)
        ("rust-tectonic-engine-bibtex" ,rust-tectonic-engine-bibtex-0.1)
        ("rust-tectonic-engine-xdvipdfmx" ,rust-tectonic-engine-xdvipdfmx-0.1)
        ("rust-tectonic-engine-xetex" ,rust-tectonic-engine-xetex-0.1)
        ("rust-tectonic-errors" ,rust-tectonic-errors-0.2)
        ("rust-tectonic-geturl" ,rust-tectonic-geturl-0.2)
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
     `(("pkg-config" ,pkg-config)))
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libgit2" ,libgit2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://tokei.rs")
    (synopsis "Count code, quickly")
    (description
     "Tokei is a program that displays statistics about your code.  Tokei will
show number of files, total lines within those files and code, comments, and
blanks grouped by language.")
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

(define-public rust-analyzer
  (package
    (name "rust-analyzer")
    (version "2021-06-07")
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
         "06bc3s5kjwpyr2cq79p0306a9bqp3xp928d750ybby9npq2dvj3z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-1.52
       #:install-source? #f             ; virtual manifest
       #:cargo-test-flags
       '("--release" "--"
         "--skip=tests::test_version_check"         ;; It need rustc's version
         ;; FIXME: Guix's rust not install source in %out/lib/rustlib/src/rust
         ;; so "can't load standard library from sysroot"
         "--skip=tests::test_loading_rust_analyzer"
         "--skip=tidy::cargo_files_are_tidy"        ;; Not need
         "--skip=tidy::check_licenses"              ;; It run cargo metadata.
         "--skip=tidy::check_merge_commits"         ;; It run git rev-list.
         "--skip=tidy::check_code_formatting"       ;; Need rustfmt as cargo fmt
         "--skip=tidy::generate_grammar"            ;; Same
         "--skip=tidy::generate_assists_tests")     ;; Same
       #:cargo-inputs
       (("rust-always-assert" ,rust-always-assert-0.1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-anymap" ,rust-anymap-0.12)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.13)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-chalk-ir" ,rust-chalk-ir-0.68)
        ("rust-chalk-recursive" ,rust-chalk-recursive-0.68)
        ("rust-chalk-solve" ,rust-chalk-solve-0.68)
        ("rust-countme" ,rust-countme-2)
        ("rust-cov-mark" ,rust-cov-mark-1)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-dissimilar" ,rust-dissimilar-1)
        ("rust-dot" ,rust-dot-0.1)
        ("rust-drop-bomb" ,rust-drop-bomb-0.1)
        ("rust-either" ,rust-either-1)
        ("rust-ena" ,rust-ena-0.14)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-expect-test" ,rust-expect-test-1)
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
        ("rust-lsp-types" ,rust-lsp-types-0.89)
        ("rust-memmap2" ,rust-memmap2-0.2)
        ("rust-mimalloc" ,rust-mimalloc-0.1)
        ("rust-miow" ,rust-miow-0.3)
        ("rust-notify" ,rust-notify-5)
        ("rust-object" ,rust-object-0.24)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-oorandom" ,rust-oorandom-11.1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-perf-event" ,rust-perf-event-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.8)
        ("rust-pulldown-cmark-to-cmark" ,rust-pulldown-cmark-to-cmark-6)
        ("rust-quote" ,rust-quote-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rowan" ,rust-rowan-0.13)
        ("rust-rustc-ap-rustc-lexer" ,rust-rustc-ap-rustc-lexer-721)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-salsa" ,rust-salsa-0.17)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-smol-str" ,rust-smol-str-0.1)
        ("rust-snap" ,rust-snap-1)
        ("rust-text-size" ,rust-text-size-1)
        ("rust-threadpool" ,rust-threadpool-1)
        ("rust-tikv-jemalloc-ctl" ,rust-tikv-jemalloc-ctl-0.4)
        ("rust-tikv-jemallocator" ,rust-tikv-jemallocator-0.4)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2)
        ("rust-tracing-tree" ,rust-tracing-tree-0.1)
        ("rust-ungrammar" ,rust-ungrammar-1)
        ("rust-url" ,rust-url-2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-write-json" ,rust-write-json-0.1)
        ("rust-xflags" ,rust-xflags-0.2)
        ("rust-xshell" ,rust-xshell-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-build-failures
           (lambda _
             (chmod ".cargo/config" 420)
             #t))
         (add-before 'check 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (string-append "#!" (which "bash"))))
               (with-directory-excursion "crates/syntax/test_data/lexer/ok"
                 (substitute* "0010_single_line_comments.txt"
                   (("SHEBANG 19")
                    (string-append "SHEBANG "
                                   (number->string (string-length bash))))
                   (("#!/usr/bin/env bash") bash))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "target/release/rust-analyzer"
                           (string-append (assoc-ref outputs "out")
                                          "/bin")))))))
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
     `(#:rust ,rust-1.52                ;inherited from rust-cargo
       #:cargo-inputs
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("libssh2" ,libssh2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/lu-zero/cargo-c")
    (synopsis "Build and install C-compatible libraries")
    (description
     "This package produces and installs a correct pkg-config file, a static
library and a dynamic library, and a C header to be used by any C (and
C-compatible) software.")
    (license license:expat)))

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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://github.com/dbrgn/tealdeer/")
    (synopsis "Fetch and show tldr help pages for many CLI commands")
    (description
     "This package fetches and shows tldr help pages for many CLI commands.
Full featured offline client with caching support.")
    (license (list license:expat license:asl2.0))))

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
