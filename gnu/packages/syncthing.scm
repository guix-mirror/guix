;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2016, 2017, 2018, 2019 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages syncthing)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang))

(define-public syncthing
  (package
    (name "syncthing")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/syncthing/syncthing"
                                  "/releases/download/v" version
                                  "/syncthing-source-v" version ".tar.gz"))
              (sha256
               (base32
                "1wdjh8xw09s1nfkpc95v04619gqa4dpbygp2y5l35ww4g916lv3s"))
              (modules '((guix build utils)))
              ;; Delete bundled ("vendored") free software source code.
              (snippet '(begin
                          (delete-file-recursively "vendor")
                          #t))))
    (build-system go-build-system)
    ;; The primary Syncthing executable goes to "out", while the auxiliary
    ;; server programs and utility tools go to "utils".  This reduces the size
    ;; of "out" by ~80 MiB.
    (outputs '("out" "utils"))
    ;; When updating Syncthing, check 'go.mod' in the source distribution to
    ;; ensure we are using the correct versions of these dependencies.
    (inputs
     `(("go-github-com-audriusbutkevicius-go-nat-pmp"
        ,go-github-com-audriusbutkevicius-go-nat-pmp)
       ("go-github-com-bkaradzic-go-lz4" ,go-github-com-bkaradzic-go-lz4)
       ("go-github-com-calmh-xdr" ,go-github-com-calmh-xdr)
       ("go-github-com-chmduquesne-rollinghash"
        ,go-github-com-chmduquesne-rollinghash)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ("go-github-com-golang-groupcache-lru"
        ,go-github-com-golang-groupcache-lru)
       ("go-github-com-jackpal-gateway" ,go-github-com-jackpal-gateway)
       ("go-github-com-kballard-go-shellquote"
        ,go-github-com-kballard-go-shellquote)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
       ("go-github-com-oschwald-geoip2-golang"
        ,go-github-com-oschwald-geoip2-golang)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
       ("go-github-com-sasha-s-go-deadlock" ,go-github-com-sasha-s-go-deadlock)
       ("go-github-com-syncthing-notify" ,go-github-com-syncthing-notify)
       ("go-github-com-syndtr-goleveldb" ,go-github-com-syndtr-goleveldb)
       ("go-github-com-thejerf-suture" ,go-github-com-thejerf-suture)
       ("go-golang-org-x-time" ,go-golang-org-x-time)
       ("go-gopkg.in-ldap.v2" ,go-gopkg.in-ldap.v2)
       ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
       ("go-github-com-shirou-gopsutil" ,go-github-com-shirou-gopsutil)
       ("go-github-com-prometheus-client-golang"
        ,go-github-com-prometheus-client-golang)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-github-com-audriusbutkevicius-recli"
        ,go-github-com-audriusbutkevicius-recli)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
       ("go-github-com-vitrun-qart" ,go-github-com-vitrun-qart)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-github-com-flynn-archive-go-shlex"
        ,go-github-com-flynn-archive-go-shlex)
       ("go-github-com-getsentry-raven-go" ,go-github-com-getsentry-raven-go)
       ("go-github-com-maruel-panicparse" ,go-github-com-maruel-panicparse)
       ("go-github-com-ccding-go-stun" ,go-github-com-ccding-go-stun)
       ("go-github-com-audriusbutkevicius-pfilter" ,go-github-com-audriusbutkevicius-pfilter)
       ("go-github-com-lucas-clemente-quic-go" ,go-github-com-lucas-clemente-quic-go)

       ;; For tests
       ("go-github-com-d4l3k-messagediff" ,go-github-com-d4l3k-messagediff)))

    (arguments
     `(#:import-path "github.com/syncthing/syncthing"
       ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'increase-test-timeout
           (lambda _
             (substitute* "src/github.com/syncthing/syncthing/build.go"
               (("120s") "999s"))
             #t))

         (replace 'build
           (lambda _
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (invoke "go" "run" "build.go" "-no-upgrade"))))

         (replace 'check
           (lambda _
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (invoke "go" "run" "build.go" "test"))))

         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (utils (assoc-ref outputs "utils"))
                   (src "src/github.com/syncthing/syncthing/bin/"))
               (install-file (string-append src "/syncthing")
                             (string-append out "/bin"))
               (delete-file (string-append src "/syncthing"))
               (copy-recursively "src/github.com/syncthing/syncthing/bin/"
                                 (string-append utils "/bin"))
               #t)))

         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (utils (assoc-ref outputs "utils"))
                    (man "/share/man")
                    (man-section (string-append man "/man"))
                    (src "src/github.com/syncthing/syncthing/man/"))
               ;; Install all the man pages to "out".
               (for-each
                 (lambda (file)
                   (install-file file
                                 (string-append out man-section
                                                (string-take-right file 1))))
                 (find-files src "\\.[1-9]"))
               ;; Copy all the man pages to "utils"
               (copy-recursively (string-append out man)
                                 (string-append utils man))
               ;; Delete extraneous man pages from "out" and "utils",
               ;; respectively.
               (delete-file (string-append out man "/man1/stdiscosrv.1"))
               (delete-file (string-append out man "/man1/strelaysrv.1"))
               (delete-file (string-append utils man "/man1/syncthing.1"))
             #t))))))
    (synopsis "Decentralized continuous file system synchronization")
    (description "Syncthing is a peer-to-peer file synchronization tool that
supports a wide variety of computing platforms.  It uses the Block Exchange
Protocol.")
    (home-page "https://github.com/syncthing/syncthing")
    (license mpl2.0)))

(define-public go-github-com-audriusbutkevicius-go-nat-pmp
  (let ((commit "452c97607362b2ab5a7839b8d1704f0396b640ca")
        (revision "0"))
    (package
      (name "go-github-com-audriusbutkevicius-go-nat-pmp")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AudriusButkevicius/go-nat-pmp")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1accmpl1llk16a19nlyy991fqrgfay6l53gb64hgmdfmqljdvbk7"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/AudriusButkevicius/go-nat-pmp"))
      (synopsis "Port mapping and discovery of external IP address")
      (description "This package provides a Go client for the NAT-PMP internet
protocol for port mapping and discovering the external IP address of a
firewall.")
      (home-page "https://github.com/AudriusButkevicius/go-nat-pmp")
      (license asl2.0))))

(define-public go-github-com-audriusbutkevicius-recli
  (package
    (name "go-github-com-audriusbutkevicius-recli")
    (version "0.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AudriusButkevicius/recli")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m1xna1kb78pkmr1lfmvvnpk9j7c4x71j3a7c6vj7zpzc4srpsmf"))))
    (build-system go-build-system)
    (inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)))
    (arguments
     `(#:import-path "github.com/AudriusButkevicius/recli"))
    (synopsis "Reflection-based CLI generator")
    (description "For a given struct, @code{recli} builds a set of
@code{urfave/cli} commands which allows you to modify it from the command line.
It is useful for generating command line clients for your application
configuration that is stored in a Go struct.")
    (home-page "https://github.com/AudriusButkevicius/recli")
    (license mpl2.0)))

(define-public go-github-com-bkaradzic-go-lz4
  (let ((commit "7224d8d8f27ef618c0a95f1ae69dbb0488abc33a")
        (revision "0"))
    (package
      (name "go-github-com-bkaradzic-go-lz4")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/bkaradzic/go-lz4")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "10lmya17vdqg2pvqni0p73iahni48s1v11ya9a0hcz4jh5vw4dkb"))))
      (build-system go-build-system)
      (arguments
        `(#:import-path "github.com/bkaradzic/go-lz4"))
      (synopsis "LZ4 compression algorithm")
      (description "This package provides @code{go-lz4}, a Go implementation of
the LZ4 compression algorithm.")
      (home-page "https://github.com/bkaradzic/go-lz4")
      (license bsd-2))))

(define-public go-github-com-calmh-du
  (package
    (name "go-github-com-calmh-du")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/calmh/du")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qb3a6y3p9nkyn3s66k6zcm16y8n8578qh23ddj14cxf2scrr2n2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/calmh/du"))
    (synopsis "Get total and available disk space of a given volume")
    (description "This is a Go implementation of `du`.  It provides disk usage
information, such as how much storage space is available, free, and used.")
    (home-page "https://github.com/calmh/du")
    (license public-domain)))

(define-public go-github-com-calmh-xdr
  (package
    (name "go-github-com-calmh-xdr")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/calmh/xdr")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "072wqdncz3nd4a3zkhvzzx1y3in1lm29wfvl0d8wrnqs5pyqh0mh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/calmh/xdr"))
    (synopsis "XDR marshalling and unmarshalling")
    (description "XDR is an External Data Representation (XDR)
marshalling and unmarshalling library in Go.  It uses code generation and not
reflection.")
    (home-page "https://github.com/calmh/xdr")
    (license expat)))

(define-public go-github-com-d4l3k-messagediff
  (package
    (name "go-github-com-d4l3k-messagediff")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/d4l3k/messagediff")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "104hl8x57ciaz7mzafg1vp9qggxcyfm8hsv9bmlihbz9ml3nyr8v"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/d4l3k/messagediff"))
    (synopsis "Diff arbitrary Go structs")
    (description "Messagediff is a library for calculating diffs of arbitrary
structs in the Go programming language.")
    (home-page "https://github.com/d4l3k/messagediff")
    (license expat)))

(define-public go-github-com-gobwas-glob
  (package
    (name "go-github-com-gobwas-glob")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gobwas/glob")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jxk1x806zn5x86342s72dq2qy64ksb3zrvrlgir2avjhwb18n6z"))))
    (build-system go-build-system)
    (arguments
      `(#:import-path "github.com/gobwas/glob"))
    (synopsis "Go globbing library")
    (description "This package provides a Go implementation of globs.")
    (home-page "https://github.com/gobwas/glob")
    (license expat)))


(define-public go-github-com-golang-groupcache-lru
  (let ((commit "869f871628b6baa9cfbc11732cdf6546b17c1298")
        (revision "2"))
    (package
      (name "go-github-com-golang-groupcache-lru")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/golang/groupcache")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r4nk8129bvx50qb4xzjaay39b2h6k7cbdqqzdlanmc82ygczsbw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/golang/groupcache/lru"
         #:unpack-path "github.com/golang/groupcache"))
      (synopsis "Groupcache is a caching and cache-filling library")
      (description "Groupcache is a caching and cache-filling library, intended
as a replacement for memcached in many cases.  It provides a data loading
mechanism with caching and de-duplication that works across a set of peer
processes.")
      (home-page "https://github.com/golang/groupcache")
      (license asl2.0))))

(define-public go-github-com-golang-snappy
  (let ((commit "553a641470496b2327abcac10b36396bd98e45c9")
        (revision "0"))
    (package
      (name "go-github-com-golang-snappy")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/golang/snappy")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kssxnih1l722hx9219c7javganjqkqhvl3i0hp0hif6xm6chvqk"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/golang/snappy"))
      (synopsis "Snappy compression format in the Go programming language")
      (description "This package provides a Go implementation of the Snappy
compression format.")
      (home-page "https://github.com/golang/snappy")
      (license bsd-3))))

(define-public go-github-com-jackpal-gateway
  (package
    (name "go-github-com-jackpal-gateway")
    (version "1.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jackpal/gateway")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ird5xmizj632l3dq24s2xgb8w1dn6v8xznlqz252gvngyr2gjl1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/jackpal/gateway"))
    (synopsis "Discover the address of a LAN gateway")
    (description "@code{gateway} is a Go library for discovering the IP
address of the default LAN gateway.")
    (home-page "https://github.com/jackpal/gateway")
    (license bsd-3)))

(define-public go-github-com-lib-pq
  (package
    (name "go-github-com-lib-pq")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lib/pq")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08j1smm6rassdssdks4yh9aspa1dv1g5nvwimmknspvhx8a7waqz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/lib/pq"
       ;; The tests seem to fail without access to the network or a running
       ;; Postgres instance.
       #:tests? #f))
    (synopsis "Golang Postgres driver for Go's database/sql")
    (description "This package provides a pure Go Postgres driver for Go's
database/sql package.")
    (home-page "https://github.com/lib/pq")
    (license expat)))

(define-public go-github-com-oschwald-geoip2-golang
  (package
    (name "go-github-com-oschwald-geoip2-golang")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oschwald/geoip2-golang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jivzl15jb8n16rm1b2w97kf7vna5dd3kcz865wfi233qay075j2"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-github-com-oschwald-maxminddb-golang"
        ,go-github-com-oschwald-maxminddb-golang)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (arguments
     `(#:import-path "github.com/oschwald/geoip2-golang"
       #:tests? #f)) ; Requires some unpackaged software and test data
    (synopsis "MaxMind GeoIP2 reader")
    (description "This package provides a library for reading MaxMind
GeoLite2 and GeoIP2 databases in Go.")
    (home-page "https://github.com/oschwald/geoip2-golang")
    (license isc)))

(define-public go-github-com-oschwald-maxminddb-golang
  (package
    (name "go-github-com-oschwald-maxminddb-golang")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oschwald/maxminddb-golang")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "100wd5qv00pkcm6cb8c4x5gavc9jnn7drh6xrqh85hzci4rils66"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (arguments
     `(#:import-path "github.com/oschwald/maxminddb-golang"
       #:tests? #f)) ; Requires some unpackaged software and test data
    (synopsis "MaxMind DB Reader for Go")
    (description "This is a Go reader for the MaxMind DB format.  Although
this can be used to read GeoLite2 and GeoIP2 databases, @code{geoip2} provides a
higher-level API for doing so.")
    (home-page "https://github.com/oschwald/maxminddb-golang")
    (license isc)))

(define-public go-github-com-stathat-go
  (let ((commit "74669b9f388d9d788c97399a0824adbfee78400e")
        (revision "0"))
    (package
      (name "go-github-com-stathat-go")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/stathat/go")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zzlsl24dyr202qkr2pay22m6d0gb7ssms77wgdx0r0clgm7dihw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/stathat/go"))
      (synopsis "Post statistics to StatHat")
      (description "This is a Go package for posting to a StatHat account.")
      (home-page "https://github.com/stathat/go")
      (license expat))))

(define-public go-github-com-rcrowley-go-metrics
  (let ((commit "e181e095bae94582363434144c61a9653aff6e50")
        (revision "1"))
    (package
      (name "go-github-com-rcrowley-go-metrics")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rcrowley/go-metrics")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pwkyw801hy7n94skzk6h177zqcil6ayrmb5gs3jdpsfayh8ia5w"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/rcrowley/go-metrics"))
      (propagated-inputs
       `(("go-github-com-stathat-go" ,go-github-com-stathat-go)))
      (synopsis "Go port of Coda Hale's Metrics library")
      (description "This package provides a Go implementation of Coda Hale's
Metrics library.")
      (home-page "https://github.com/rcrowley/go-metrics")
      (license bsd-2))))

(define-public go-github-com-sasha-s-go-deadlock
  (package
    (name "go-github-com-sasha-s-go-deadlock")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sasha-s/go-deadlock")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13p7b7pakd9k1c2k0fs1hfim3c8mivz679977ai6zb01s4aw7gyg"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/sasha-s/go-deadlock"))
    (propagated-inputs
     `(("go-github-com-petermattis-goid" ,go-github-com-petermattis-goid)))
    (synopsis "Deadlock detection in go")
    (description "This package provides tools for detecting deadlocks at
run-time in Go.")
    (home-page "https://github.com/sasha-s/go-deadlock")
    (license asl2.0)))

(define-public go-github-com-syndtr-goleveldb
  (let ((commit "c3a204f8e96543bb0cc090385c001078f184fc46")
        (revision "3"))
    (package
      (name "go-github-com-syndtr-goleveldb")
      (version (git-version "1.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/syndtr/goleveldb")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "18cac90dim2z5g28vazzibxa058q2ynihsmf0vbscwzba6l5s5ms"))))
      (build-system go-build-system)
      (propagated-inputs
       `(("go-github-com-golang-snappy" ,go-github-com-golang-snappy)))
      (arguments
       `(#:import-path "github.com/syndtr/goleveldb/leveldb"
         #:unpack-path "github.com/syndtr/goleveldb"
         #:tests? #f)) ; XXX needs 'github.com/onsi/gomega' package
      (synopsis "LevelDB key/value database")
      (description "This is an implementation of the LevelDB key / value
database in Go.")
      (home-page "https://github.com/syndtr/goleveldb")
      (license bsd-2))))

(define-public go-github-com-thejerf-suture
  (package
    (name "go-github-com-thejerf-suture")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thejerf/suture")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03bdrl78jfwk0kw40lj63ga9cxhgccgss8yi9lp5j0m0ml7921gh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/thejerf/suture"))
    (synopsis "Supervisor trees for Go")
    (description "Suture provides Erlang-ish supervisor trees for Go.
\"Supervisor trees\" -> \"sutree\" -> \"suture\" -> holds your code together
when it's trying to die.

It is intended to deal gracefully with the real failure cases that can occur
with supervision trees (such as burning all your CPU time endlessly restarting
dead services), while also making no unnecessary demands on the \"service\"
code, and providing hooks to perform adequate logging with in a production
environment")
    (home-page "https://github.com/thejerf/suture")
    (license expat)))

(define-public go-github-com-vitrun-qart
  (let ((commit "bf64b92db6b05651d6c25a3dabf2d543b360c0aa")
        (revision "0"))
    (package
      (name "go-github-com-vitrun-qart")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vitrun/qart")
                      (commit commit)))
                (file-name (string-append "go-github-com-vitrun-qart-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1xk7qki703xmay9ghi3kq2bjf1iw9dz8wik55739d6i7sn77vvkc"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/vitrun/qart"))
      (synopsis "Create QR codes with an embedded image")
      (description "This package provides a library for embedding
human-meaningful graphics in QR codes.  However, instead of scribbling on
redundant pieces and relying on error correction to preserve the meaning,
@code{qart} engineers the encoded values to create the picture in a code with no
inherent errors.")
      (home-page "https://github.com/vitrun/qart")
      (license bsd-3))))

(define-public go-github-com-chmduquesne-rollinghash
  (let ((commit "a60f8e7142b536ea61bb5d84014171189eeaaa81")
        (revision "0"))
    (package
      (name "go-github-com-chmduquesne-rollinghash")
      (version (git-version "4.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chmduquesne/rollinghash.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0fpaqq4zb0wikgbhn7vwqqj1h865f5xy195vkhivsp922p7qwsjr"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/chmduquesne/rollinghash/"))
      (synopsis "Rolling hashes in Go")
      (description "This package provides a Go implementation of several rolling
hashes.")
      (home-page "https://github.com/chmduquesne/rollinghash")
      (license expat))))

(define-public go-github-com-petermattis-goid
  (let ((commit "b0b1615b78e5ee59739545bb38426383b2cda4c9")
        (revision "1"))
    (package
      (name "go-github-com-petermattis-goid")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/petermattis/goid.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ghfxn045r0bbn2vszw897lxzmhnm4k59aypjvpxl0pbzsw9ab2c"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/petermattis/goid"))
      (synopsis "Identify the running goroutine")
      (description "This package offers a method of programmatically retrieving
the current goroutine's ID.")
      (home-page "https://github.com/petermattis/goid")
      (license asl2.0))))

(define-public go-github-com-kballard-go-shellquote
  (let ((commit "95032a82bc518f77982ea72343cc1ade730072f0")
        (revision "1"))
    (package
      (name "go-github-com-kballard-go-shellquote")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/kballard/go-shellquote.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rspvmnsikdq95jmx3dykxd4k1rmgl98ryjrysvl0cf18hl1vq80"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/kballard/go-shellquote"))
      (synopsis "Shell-style string joins and splits")
      (description "Shellquote provides utilities for joining/splitting strings
using sh's word-splitting rules.")
      (home-page "https://github.com/kballard/go-shellquote")
      (license expat))))

(define-public go-github-com-syncthing-notify
  (let ((commit "69c7a957d3e261f9744f46b3dd4d608d8480ad90")
        (revision "5"))
    (package
      (name "go-github-com-syncthing-notify")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/syncthing/notify")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mmdzyfnmjabyhbipl4bggw4w5nlxyyjp0d93qd824kj07kmsr1f"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/syncthing/notify"))
      (propagated-inputs
       `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
      (synopsis "File system event notification library")
      (description "This package provides @code{notify}, a file system event
notification library in Go.")
      (home-page "https://github.com/syncthing/notify")
      (license expat))))

(define-public go-github-com-beorn7-perks-quantile
  (let ((commit "4c0e84591b9aa9e6dcfdf3e020114cd81f89d5f9")
        (revision "0"))
    (package
      (name "go-github-com-beorn7-perks-quantile")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/beorn7/perks.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hrybsql68xw57brzj805xx2mghydpdiysv3gbhr7f5wlxj2514y"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/beorn7/perks/quantile"
         #:unpack-path "github.com/beorn7/perks"))
      (synopsis "Compute approximate quantiles over an unbounded data stream")
      (description "Perks contains the Go package @code{quantile} that computes
approximate quantiles over an unbounded data stream within low memory and CPU
bounds.")
      (home-page "https://github.com/beorn7/perks")
      (license expat))))

(define-public go-github-com-golang-protobuf-proto
    (package
      (name "go-github-com-golang-protobuf-proto")
      (version "1.3.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/golang/protobuf.git")
                       (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "15am4s4646qy6iv0g3kkqq52rzykqjhm4bf08dk0fy2r58knpsyl"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/golang/protobuf/proto"
         #:unpack-path "github.com/golang/protobuf"
         #:tests? #f ; requires unpackaged golang.org/x/sync/errgroup
         ))
      (synopsis "Go support for Protocol Buffers")
      (description "This package provides Go support for the Protocol Buffers
data serialization format.")
      (home-page "https://github.com/golang/protobuf")
      (license bsd-3)))

(define-public go-github-com-prometheus-client-model
  (let ((commit "14fe0d1b01d4d5fc031dd4bec1823bd3ebbe8016")
        (revision "2"))
    (package
      (name "go-github-com-prometheus-client-model")
      (version (git-version "0.0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/prometheus/client_model.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zdmk6rbbx39cvfz0r59v2jg5sg9yd02b4pds5n5llgvivi99550"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/prometheus/client_model"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (propagated-inputs
       `(("go-github-com-golang-protobuf-proto"
          ,go-github-com-golang-protobuf-proto)))
      (synopsis "Data model artifacts for Prometheus")
      (description "This package provides data model artifacts for Prometheus.")
      (home-page "https://github.com/prometheus/client_model")
      (license asl2.0))))

(define-public go-github-com-matttproud-golang-protobuf-extensions-pbutil
  (let ((commit "c12348ce28de40eed0136aa2b644d0ee0650e56c")
        (revision "0"))
    (package
      (name "go-github-com-matttproud-golang-protobuf-extensions-pbutil")
      (version (git-version "1.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri
            (git-reference
              (url "https://github.com/matttproud/golang_protobuf_extensions.git")
              (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1d0c1isd2lk9pnfq2nk0aih356j30k3h1gi2w0ixsivi5csl7jya"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/matttproud/golang_protobuf_extensions/pbutil"
         #:unpack-path "github.com/matttproud/golang_protobuf_extensions"))
      (propagated-inputs
       `(("go-github-com-golang-protobuf-proto"
          ,go-github-com-golang-protobuf-proto)))
      (synopsis "Streaming Protocol Buffers in Go")
      (description "This package provides various Protocol Buffer
extensions for the Go language, namely support for record length-delimited
message streaming.")
      (home-page "https://github.com/matttproud/golang_protobuf_extensions")
      (license asl2.0))))

(define-public go-github-com-prometheus-common
    (package
      (name "go-github-com-prometheus-common")
      (version "0.4.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/prometheus/common.git")
                       (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0sf4sjdckblz1hqdfvripk3zyp8xq89w7q75kbsyg4c078af896s"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/prometheus/common"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/github.com/prometheus/common/expfmt/testdata/")
                      ".*\\.gz$"))
               #t))
           ;; Source-only package
           (delete 'build))))
      (propagated-inputs
       `(("go-github-com-golang-protobuf-proto"
          ,go-github-com-golang-protobuf-proto)
         ("go-github-com-matttproud-golang-protobuf-extensions-pbutil"
          ,go-github-com-matttproud-golang-protobuf-extensions-pbutil)
         ("go-github-com-prometheus-client-model"
          ,go-github-com-prometheus-client-model)))
      (synopsis "Prometheus metrics")
      (description "This package provides tools for reading and writing
Prometheus metrics.")
      (home-page "https://github.com/prometheus/common")
      (license asl2.0)))

(define-public go-github-com-prometheus-procfs
    (package
      (name "go-github-com-prometheus-procfs")
      (version "0.0.4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/prometheus/procfs.git")
                       (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1z5jq5rjala0a0di4nwk1rai0z9f73qwqj6mgcbpjbg2qknlb544"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/prometheus/procfs"
         ;; The tests require Go modules, which are not yet supported in Guix's
         ;; Go build system.
         #:tests? #f))
      (synopsis "Go library for reading @file{/proc}")
      (description "The @code{procfs} Go package provides functions to retrieve
system, kernel, and process metrics from the @file{/proc} pseudo file system.")
      (home-page "https://github.com/prometheus/procfs")
      (license asl2.0)))

(define-public go-github-com-prometheus-client-golang
    (package
      (name "go-github-com-prometheus-client-golang")
      (version "0.9.4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/prometheus/client_golang.git")
                       (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0s134fj4i7k6pxdmxwkdi7amb1882yq33spv15hg3pkpbd3h311p"))))
      (build-system go-build-system)
      (arguments
       '(#:tests? #f
         #:import-path "github.com/prometheus/client_golang"
         #:phases
         (modify-phases %standard-phases
           ;; Source-only package
           (delete 'build))))
      (propagated-inputs
       `(("go-github-com-beorn7-perks-quantile"
          ,go-github-com-beorn7-perks-quantile)
         ("go-github-com-golang-protobuf-proto"
          ,go-github-com-golang-protobuf-proto)
         ("go-github-com-prometheus-client-model"
          ,go-github-com-prometheus-client-model)
         ("go-github-com-prometheus-common"
          ,go-github-com-prometheus-common)
         ("go-github-com-prometheus-procfs" ,go-github-com-prometheus-procfs)))
      (synopsis "HTTP server and client tools for Prometheus")
      (description "This package @code{promhttp} provides HTTP client and
server tools for Prometheus metrics.")
      (home-page "https://github.com/prometheus/client_golang")
      (license asl2.0)))

(define-public go-gopkg.in-asn1-ber.v1
  (package
    (name "go-gopkg.in-asn1-ber.v1")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gopkg.in/asn1-ber.v1")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y8bvzbxpw0lfnn7pbcdwzqj4l90qj6xf88dvv9pxd9yl5g6cskx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/asn1-ber.v1"
       ;; Tests don't pass "vet" on Go since 1.11.  See
       ;; https://github.com/go-asn1-ber/asn1-ber/issues/20.
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key import-path #:allow-other-keys)
             (invoke "go" "test"
                     "-vet=off"
                     import-path))))))
    (synopsis "ASN.1 BER encoding and decoding in Go")
    (description "This package provides ASN.1 BER encoding and decoding in the
Go language.")
    (home-page "https://gopkg.in/asn1-ber.v1")
    (license expat)))

(define-public go-gopkg.in-ldap.v2
  (package
    (name "go-gopkg.in-ldap.v2")
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gopkg.in/ldap.v2")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wf81wy04nhkqs0dg5zkivr4sh37r83bxrfwjz9vr4jq6vmljr3h"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/ldap.v2"
       #:tests? #f)) ; the test suite requires network access
    (propagated-inputs
     `(("go-gopkg.in-asn1-ber.v1" ,go-gopkg.in-asn1-ber.v1)))
    (synopsis "LDAP v3 functionality for Go")
    (description "This package provides basic LDAP v3 functionality in the Go
language.")
    (home-page "https://gopkg.in/ldap.v2")
    (license expat)))

(define-public go-github-com-flynn-archive-go-shlex
  (let ((commit "3f9db97f856818214da2e1057f8ad84803971cff")
        (revision "0"))
    (package
      (name "go-github-com-flynn-archive-go-shlex")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/flynn-archive/go-shlex.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1j743lysygkpa2s2gii2xr32j7bxgc15zv4113b0q9jhn676ysia"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/flynn-archive/go-shlex"))
      (synopsis "Go lexer")
      (description "Shlex is a simple lexer for go that supports shell-style
quoting, commenting, and escaping.")
      (home-page "https://github.com/flynn-archive/go-shlex")
      (license asl2.0))))

(define-public go-github-com-audriusbutkevicius-pfilter
  (let ((commit "c55ef6137fc6f075801eac099cc2687ede0f101d")
        (revision "3"))
    (package
      (name "go-github-com-audriusbutkevicius-pfilter")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/AudriusButkevicius/pfilter.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0xzhwyd0w21bhvzl5pinn22hp0y6h44rh3s2ppql69rafc6zd3c6"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/AudriusButkevicius/pfilter"))
      (synopsis "Filter packets into multiple virtual connections")
      (description "Pfilter is a Go package for filtering packets into multiple
virtual connections from a single physical connection.")
      (home-page "https://github.com/AudriusButkevicius/pfilter")
      (license expat))))

(define-public go-github-com-ccding-go-stun
  (let ((commit "be486d185f3dfcb2dbf8429332da50a0da7f95a6")
        (revision "2"))
    (package
      (name "go-github-com-ccding-go-stun")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ccding/go-stun.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1gr0rw1c1y7wh6913lyn5k4ig023by27i36bly6am8dwgrgp34ww"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ccding/go-stun"))
      (synopsis "STUN client implementation")
      (description "Go-stun is a go implementation of the STUN client (RFC 3489
and RFC 5389).")
      (home-page "https://github.com/ccding/go-stun")
      (license asl2.0))))
