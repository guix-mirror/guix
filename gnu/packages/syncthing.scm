;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
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
  #:use-module (guix licenses))

(define-public syncthing
  (package
    (name "syncthing")
    (version "0.14.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/syncthing/syncthing"
                                  "/releases/download/v" version
                                  "/syncthing-source-v" version ".tar.gz"))
              (sha256
               (base32
                "0qqcn8j2hng4jl6ndbrjmbiwbl2f305qx5yw7swbvj7s3l7k756i"))))
    (build-system go-build-system)
    ;; The primary Syncthing executable goes to "out", while the auxiliary
    ;; server programs and utility tools go to "utils".  This reduces the size
    ;; of "out" by ~80 MiB.
    (outputs '("out" "utils"))
    (arguments
     `(#:import-path "github.com/syncthing/syncthing"
       #:unpack-path "github.com/syncthing"
       ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bundled-source-code
           (lambda _
             ;; Keep the bundled cznic libraries. There are some "internal"
             ;; cznic libraries that complicate the use of non-bundled copies.
             (rename-file "src/github.com/syncthing/syncthing/vendor/github.com/cznic"
                          "cznic")
             (delete-file-recursively "src/github.com/syncthing/syncthing/vendor")
             (mkdir-p "src/github.com/syncthing/syncthing/vendor/github.com/")
             (rename-file "cznic"
                          "src/github.com/syncthing/syncthing/vendor/github.com/cznic")
             #t))

         (add-before 'build 'increase-test-timeout
           (lambda _
             (substitute* "src/github.com/syncthing/syncthing/build.go"
               (("60s") "999s"))
             #t))

         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (zero? (system* "go" "run" "build.go" "-no-upgrade")))))

         (replace 'check
           (lambda _
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (zero? (system* "go" "run" "build.go" "test")))))

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
    ;; When updating Syncthing, check 'vendor/manifest' in the source
    ;; distribution to ensure we are using the correct versions of these
    ;; dependencies.
    (inputs
     `(("go-github-com-audriusbutkevicius-cli"
        ,go-github-com-audriusbutkevicius-cli)
       ("go-github-com-audriusbutkevicius-kcp-go"
        ,go-github-com-audriusbutkevicius-kcp-go)
       ("go-github-com-audriusbutkevicius-go-nat-pmp"
        ,go-github-com-audriusbutkevicius-go-nat-pmp)
       ("go-github-com-audriusbutkevicius-pfilter"
        ,go-github-com-audriusbutkevicius-pfilter)
       ("go-github-com-bkaradzic-go-lz4" ,go-github-com-bkaradzic-go-lz4)
       ("go-github-com-calmh-du" ,go-github-com-calmh-du)
       ("go-github-com-calmh-xdr" ,go-github-com-calmh-xdr)
       ("go-github-com-ccding-go-stun"
        ,go-github-com-ccding-go-stun)
       ("go-github-com-chmduquesne-rollinghash-adler32"
        ,go-github-com-chmduquesne-rollinghash-adler32)
;       ("go-github-com-cznic-ql" ,go-github-com-cznic-ql) ; bundled
       ; Used by bundled ql
       ("go-github-com-edsrzf-mmap-go" ,go-github-com-edsrzf-mmap-go)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ("go-github-com-gogo-protobuf-union"
        ,(go-github-com-gogo-protobuf-union))
       ("go-github-com-golang-groupcache-lru"
        ,go-github-com-golang-groupcache-lru)
       ("go-github-com-jackpal-gateway" ,go-github-com-jackpal-gateway)
       ("go-github-com-kardianos-osext" ,go-github-com-kardianos-osext)
       ("go-github-com-kballard-go-shellquote"
        ,go-github-com-kballard-go-shellquote)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
       ("go-github-com-oschwald-geoip2-golang"
        ,go-github-com-oschwald-geoip2-golang)
       ("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
       ("go-github-com-sasha-s-go-deadlock" ,go-github-com-sasha-s-go-deadlock)
       ("go-github-com-syndtr-goleveldb" ,go-github-com-syndtr-goleveldb)
       ("go-github-com-thejerf-suture" ,go-github-com-thejerf-suture)
       ("go-github-com-vitrun-qart" ,(go-github-com-vitrun-qart-union))
       ("go-github-com-xtaci-smux" ,go-github-com-xtaci-smux)
       ("go-golang-org-x-crypto" ,(go-golang-org-x-crypto-union))
       ("go-golang-org-x-net-union" ,(go-golang-org-x-net-union))
       ("go-golang-org-x-text" ,(go-golang-org-x-text-union))
       ("go-golang-org-x-time-rate" ,go-golang-org-x-time-rate)
       ("go-github-com-d4l3k-messagediff"
        ,go-github-com-d4l3k-messagediff)
       ("go-github-com-zillode-notify" ,go-github-com-zillode-notify)))
    (synopsis "Decentralized continuous filesystem synchronization")
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
      (description "This packages provides a Go client for the NAT-PMP internet
protocol for port mapping and discovering the external IP address of a
firewall.")
      (home-page "https://github.com/AudriusButkevicius/go-nat-pmp")
      (license asl2.0))))

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
  (let ((commit "08e072f9cb164f943a92eb59f90f3abc64ac6e8f")
        (revision "0"))
    (package
      (name "go-github-com-calmh-xdr")
      (version (git-version "2.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/calmh/xdr")
                      (commit commit)))
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
      (license expat))))

(define-public go-github-com-d4l3k-messagediff
  (let ((commit "29f32d820d112dbd66e58492a6ffb7cc3106312b")
        (revision "0"))
    (package
      (name "go-github-com-d4l3k-messagediff")
      (version (git-version "1.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/d4l3k/messagediff")
                       (commit commit)))
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
      (license expat))))

(define-public go-github-com-edsrzf-mmap-go
  (let ((commit "0bce6a6887123b67a60366d2c9fe2dfb74289d2e")
        (revision "0"))
    (package
      (name "go-github-com-edsrzf-mmap-go")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/edsrzf/mmap-go")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1am4m2k451bksnbiqj6lxknk4lsgmrhv0q3ajqac818vj0cpfgs9"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/edsrzf/mmap-go"))
      (synopsis "Go implementation of mmap")
      (description "This packages provides a Go implementation of mmap.")
      (home-page "https://github.com/edsrzf/mmap-go")
      (license bsd-3))))

(define-public go-github-com-gobwas-glob
  (let ((commit "51eb1ee00b6d931c66d229ceeb7c31b985563420")
        (revision "0"))
    (package
      (name "go-github-com-gobwas-glob")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/gobwas/glob")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "090wzpwsjana1qas8ipwh1pj959gvc4b7vwybzi01f3bmd79jwlp"))))
      (build-system go-build-system)
      (arguments
        `(#:import-path "github.com/gobwas/glob"))
      (synopsis "Go globbing library")
      (description "This packages provides a Go implementation of globs.")
      (home-page "https://github.com/gobwas/glob")
      (license expat))))

(define* (go-github-com-gogo-protobuf-union
           #:optional (packages (list go-github-com-gogo-protobuf
                                      go-github-com-gogo-protobuf-protoc-gen-gogo)))
  (package
    (name "go-github-com-gogo-protobuf-union")
    (version (package-version go-github-com-gogo-protobuf))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 packages))
    (synopsis "Union of Go protobuf libraries")
    (description "This is a union of Go protobuf libraries")
    (home-page (package-home-page go-github-com-gogo-protobuf))
    (license (package-license go-github-com-gogo-protobuf))))

(define-public go-github-com-gogo-protobuf
  (let ((commit "efccd33a0c20aa078705571d5ddbfa14c8395a63")
        (revision "0"))
    (package
      (name "go-github-com-gogo-protobuf")
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gogo/protobuf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09kfa3aqmhh7p0rc6wd4fw5cjccidsk9vgcy13albv0g8vnbmmgw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gogo/protobuf/proto"
         #:unpack-path "github.com/gogo/protobuf"))
      (propagated-inputs
       `(("go-github-com-gogo-protobuf-protoc-gen-gogo"
          ,go-github-com-gogo-protobuf-protoc-gen-gogo)))
      (synopsis "Protocol Buffers for Go with Gadgets")
      (description "Gogoprotobuf is a fork of golang/protobuf with extra code
generation features.  This code generation is used to achieve:
@itemize
@item fast marshalling and unmarshalling
@item more canonical Go structures
@item goprotobuf compatibility
@item less typing by optionally generating extra helper code
@item peace of mind by optionally generating test and benchmark code
@item other serialization formats
@end itemize")
      (home-page "https://github.com/gogo/protobuf")
      (license bsd-3))))

(define-public go-github-com-gogo-protobuf-protoc-gen-gogo
  (let ((commit "efccd33a0c20aa078705571d5ddbfa14c8395a63")
        (revision "0"))
    (package
      (name "go-github-com-gogo-protobuf-protoc-gen-gogo")
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/gogo/protobuf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09kfa3aqmhh7p0rc6wd4fw5cjccidsk9vgcy13albv0g8vnbmmgw"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gogo/protobuf/protoc-gen-gogo"
         #:unpack-path "github.com/gogo/protobuf"))
      (synopsis "Protocol Buffers for Go with Gadgets")
      (description "Gogoprotobuf is a fork of golang/protobuf with extra code
generation features.  This code generation is used to achieve:
@itemize
@item fast marshalling and unmarshalling
@item more canonical Go structures
@item goprotobuf compatibility
@item less typing by optionally generating extra helper code
@item peace of mind by optionally generating test and benchmark code
@item other serialization formats
@end itemize")
      (home-page "https://github.com/gogo/protobuf")
      (license bsd-3))))

(define-public go-github-com-golang-groupcache-lru
  (let ((commit "72d04f9fcdec7d3821820cc4a6f150eae553639a")
        (revision "0"))
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
                  "1l3ryh7bq1f2mhr3sd3x1wav99pd27r8l3ydgqh375wn4x7v5qd6"))))
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
  (let ((commit "5795ac81146e01d3fab7bcf21c043c3d6a32b006")
        (revision "0"))
    (package
      (name "go-github-com-jackpal-gateway")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jackpal/gateway")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0fkwkwmhfadwk3cha8616bhqxfkr9gjjnynhhxyldlphixgs3f25"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/jackpal/gateway"))
      (synopsis "Discover the address of a LAN gateway")
      (description "@code{gateway} is a Go library for discovering the IP
address of the default LAN gateway.")
      (home-page "https://github.com/jackpal/gateway")
      (license bsd-3))))

(define-public go-github-com-kardianos-osext
  (let ((commit "9d302b58e975387d0b4d9be876622c86cefe64be")
        (revision "0"))
    (package
      (name "go-github-com-kardianos-osext")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kardianos/osext")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r6f727s16g4f66k8c2z1xh8ga1p53hg9g2v95pmhd1i60fhy47a"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/kardianos/osext"))
      (synopsis "Find the running executable")
      (description "Osext provides a method for finding the current executable
file that is running.  This can be used for upgrading the current executable or
finding resources located relative to the executable file.")
      (home-page "https://github.com/kardianos/osext")
      (license bsd-3))))

(define-public go-github-com-lib-pq
  (let ((commit "2704adc878c21e1329f46f6e56a1c387d788ff94")
        (revision "0"))
    (package
      (name "go-github-com-lib-pq")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lib/pq")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "160fmvi7bczxw3i3h5s821hv029ph5ld8x3c36b4cz2sr30wp110"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/lib/pq"
         ;; The tests seem to fail without access to the network or a running
         ;; Postgres instance.
         #:tests? #f))
      (synopsis "Golang Postgres driver for Go's database/sql")
      (description "This packages provides a pure Go Postgres driver for Go's
database/sql package.")
      (home-page "https://github.com/lib/pq")
      (license expat))))

(define-public go-github-com-minio-sha256-simd
  (let ((commit "6124d070eb4e7001c244b6ccc282620a5dce44a0")
        (revision "0"))
    (package
      (name "go-github-com-minio-sha256-simd")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/minio/sha256-simd")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1azrdp7x7vl9ngkxs890blspz0345xhadvssdlb0435hdqa0gkll"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/minio/sha256-simd"))
      (synopsis "Hardware-accelerated SHA256 in Go using SIMD")
      (description "This packages provides a pure Go implementation of SHA256
using SIMD (Single instruction, multiple data) instructions for Intel and ARM
architectures.")
      (home-page "https://github.com/minio/sha256-simd")
      (license asl2.0))))

(define-public go-github-com-oschwald-geoip2-golang
  (let ((commit "0fd242da7906550802871efe101abfdb1cc550a8")
        (revision "0"))
    (package
      (name "go-github-com-oschwald-geoip2-golang")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/oschwald/geoip2-golang")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kglnix0r5sjkk346ip30l7dwq1gv2g4wjy2cjmgjvb8x778hnww"))))
      (build-system go-build-system)
      (propagated-inputs
       `(("go-github-com-oschwald-maxminddb-golang"
          ,go-github-com-oschwald-maxminddb-golang)
         ("go-golang-org-x-sys-unix" ,go-golang-org-x-sys-unix)))
      (arguments
       `(#:import-path "github.com/oschwald/geoip2-golang"
         #:tests? #f)) ; Requires some unpackaged software and test data
      (synopsis "MaxMind GeoIP2 reader")
      (description "This packages provides a library for reading MaxMind
GeoLite2 and GeoIP2 databases in Go.")
      (home-page "https://github.com/oschwald/geoip2-golang")
      (license isc))))

(define-public go-github-com-oschwald-maxminddb-golang
  (let ((commit "697da8075d2061aa8ed639346443f5d3e8c80b30")
        (revision "0"))
    (package
      (name "go-github-com-oschwald-maxminddb-golang")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/oschwald/maxminddb-golang")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "00kkxzlvra0kcbkl56wp0dp1yw3cmfjqqlwbqy7bq5r34s7iavq0"))))
      (build-system go-build-system)
      (propagated-inputs
       `(("go-golang-org-x-sys-unix" ,go-golang-org-x-sys-unix)))
      (arguments
       `(#:import-path "github.com/oschwald/maxminddb-golang"
         #:tests? #f)) ; Requires some unpackaged software and test data
      (synopsis "MaxMind DB Reader for Go")
      (description "This is a Go reader for the MaxMind DB format.  Although
this can be used to read GeoLite2 and GeoIP2 databases, @code{geoip2} provides a
higher-level API for doing so.")
      (home-page "https://github.com/oschwald/maxminddb-golang")
      (license isc))))

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
  (let ((commit "1f30fe9094a513ce4c700b9a54458bbb0c96996c")
        (revision "0"))
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
                  "1hvbiaq4b6dqgjz6jkkxglfh9gf71zin6qsg508sh0r0ixfavrzj"))))
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
  (let ((commit "341000892f3dd25f440e6231e8533eb3688ed7ec")
        (revision "0"))
    (package
      (name "go-github-com-sasha-s-go-deadlock")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sasha-s/go-deadlock")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bcdyxwm5qpqynxahwaahbqi7ghgdajmg7b4276pdalkxkxkhsv8"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/sasha-s/go-deadlock"))
      (propagated-inputs
       `(("go-github-com-petermattis-goid" ,go-github-com-petermattis-goid)))
      (synopsis "Deadlock detection in go")
      (description "This package provides tools for detecting deadlocks at
run-time in Go.")
      (home-page "https://github.com/sasha-s/go-deadlock")
      (license asl2.0))))

(define-public go-github-com-syndtr-goleveldb
  (let ((commit "549b6d6b1c0419617182954dd77770f2e2685ed5")
        (revision "1"))
    (package
      (name "go-github-com-syndtr-goleveldb")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/syndtr/goleveldb")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hs8bsxyjfq9d7000i1jk8bq7p2ab8snz23air13aw5ra2ri36bq"))))
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
  (let ((commit "0ac47afae95ad5bc5184ed346bc945168e883f5d")
        (revision "0"))
    (package
      (name "go-github-com-thejerf-suture")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thejerf/suture")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0f860fkaibnnkmh4q6q9yn3r26sraaj8wx9irwm76cmsp48zcxfy"))))
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
      (license expat))))

(define* (go-github-com-vitrun-qart-union
           #:optional (packages (list go-github-com-vitrun-qart-coding
                                      go-github-com-vitrun-qart-gf256
                                      go-github-com-vitrun-qart-qr)))
  (package
    (name "go-github-com-vitrun-qart")
    (version (package-version go-github-com-vitrun-qart-qr))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 packages))
    (synopsis "Union of qart libraries")
    (description "This is a union of qart libraries.")
    (home-page (package-home-page go-github-com-vitrun-qart-qr))
    (license (package-license go-github-com-vitrun-qart-qr))))

(define-public go-github-com-vitrun-qart-coding
  (let ((commit "bf64b92db6b05651d6c25a3dabf2d543b360c0aa")
        (revision "0"))
    (package
      (name "go-github-com-vitrun-qart-coding")
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
       `(#:import-path "github.com/vitrun/qart/coding"
         #:unpack-path "github.com/vitrun/qart"))
      (synopsis "Low-level QR coding library")
      (description "This package provides a library for embedding
human-meaningful graphics in QR codes.  However, instead of scribbling on
redundant pieces and relying on error correction to preserve the meaning,
@code{qart} engineers the encoded values to create the picture in a code with no
inherent errors.  This @code{qart} component, @code{coding}, implements
low-level QR coding details.")
      (home-page "https://github.com/vitrun/qart/")
      (license bsd-3))))

(define-public go-github-com-vitrun-qart-gf256
  (let ((commit "bf64b92db6b05651d6c25a3dabf2d543b360c0aa")
        (revision "0"))
    (package
      (name "go-github-com-vitrun-qart-gf256")
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
       `(#:import-path "github.com/vitrun/qart/gf256"
         #:unpack-path "github.com/vitrun/qart"))
      (synopsis "Qart library for Galois Field GF(256) math")
      (description "This package, a component of @code{qart}, provides @code{gf256},
implements arithmetic over the Galois Field GF(256).")
      (home-page "https://github.com/vitrun/qart")
      (license bsd-3))))

(define-public go-github-com-vitrun-qart-qr
  (let ((commit "bf64b92db6b05651d6c25a3dabf2d543b360c0aa")
        (revision "0"))
    (package
      (name "go-github-com-vitrun-qart-qr")
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
       `(#:import-path "github.com/vitrun/qart/qr"
         #:unpack-path "github.com/vitrun/qart"))
      (synopsis "Qart component for generating QR codes")
      (description "This package, a component of @code{qart}, provides
@code{qr}, for QR code generation.")
      (description "This package provides a library for embedding
human-meaningful graphics in QR codes.  However, instead of scribbling on
redundant pieces and relying on error correction to preserve the meaning,
@code{qart} engineers the encoded values to create the picture in a code with no
inherent errors.  This @code{qart} component, @code{qr}, provides QR code
generation.")
      (home-page "https://github.com/vitrun/qart")
      (license bsd-3))))

;; Go searches for library modules by looking in the GOPATH environment
;; variable.  This variable is a list of paths.  However, Go does not
;; keep searching on GOPATH if it tries and fails to import a module.
;; So, we use a union for packages sharing a namespace.
(define* (go-golang-org-x-crypto-union #:optional
                                    (packages (list go-golang-org-x-crypto-blowfish
                                                    go-golang-org-x-crypto-bcrypt
                                                    go-golang-org-x-crypto-tea
                                                    go-golang-org-x-crypto-xtea
                                                    go-golang-org-x-crypto-pbkdf2
                                                    go-golang-org-x-crypto-twofish
                                                    go-golang-org-x-crypto-cast5
                                                    go-golang-org-x-crypto-salsa20)))
  (package
    (name "go-golang-org-x-crypto")
    (version (package-version go-golang-org-x-crypto-bcrypt))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 packages))
    (synopsis "Union of the Go x crypto libraries")
    (description "A union of the Golang cryptographic libraries.  A
union is required because `go build` assumes that all of the headers and
libraries are in the same directory.")
    (home-page (package-home-page go-golang-org-x-crypto-bcrypt))
    (license (package-license go-golang-org-x-crypto-bcrypt))))

(define-public go-golang-org-x-crypto-bcrypt
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-bcrypt")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/bcrypt"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Bcrypt in Go")
      (description "This package provides a Go implementation of the bcrypt
password hashing function.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-blowfish
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-blowfish")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/blowfish"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Blowfish in Go")
      (description "This package provides a Go implementation of the Blowfish
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-pbkdf2
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-pbkdf2")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/pbkdf2"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "PBKDF2 in Go")
      (description "This package provides a Go implementation of the PBKDF2 key
derivation function.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-tea
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-tea")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/tea"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Tiny Encryption Algorithm (TEA) in Go")
      (description "This packages a Go implementation of the Tiny Encryption
Algorithm (TEA) block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-salsa20
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-salsa20")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/salsa20"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Salsa20 in Go")
      (description "This packages provides a Go implementation of the Salsa20
stream cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-cast5
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-cast5")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/cast5"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Cast5 in Go")
      (description "This packages provides a Go implementation of the Cast5
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-twofish
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-twofish")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/twofish"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "Twofish in Go")
      (description "This packages provides a Go implementation of the Twofish
symmetric-key block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define-public go-golang-org-x-crypto-xtea
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang-org-x-crypto-xtea")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-crypto-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/crypto/xtea"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                      (string-append (assoc-ref outputs "out")
                                     "/src/golang.org/x/crypto/ed25519/testdata")
                      ".*\\.gz$"))
               #t)))))
      (synopsis "eXtended Tiny Encryption Algorithm (XTEA) in Go")
      (description "This package provides a Go implementation of the eXtended
Tiny Encryption Algorithm (XTEA) block cipher.")
      (home-page "https://go.googlesource.com/crypto/")
      (license bsd-3))))

(define* (go-golang-org-x-net-union #:optional
                                 (packages (list go-golang-org-x-net-ipv4
                                                 go-golang-org-x-net-bpf
                                                 go-golang-org-x-net-context
                                                 go-golang-org-x-net-ipv6
                                                 go-golang-org-x-net-proxy
                                                 go-golang-org-x-net-internal-iana)))
  (package
    (name "go-golang-org-x-net")
    (version (package-version go-golang-org-x-net-ipv4))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 packages))
    (synopsis "Union of the Go net libraries")
    (description "A union of the Golang net libraries.")
    (home-page (package-home-page go-golang-org-x-net-ipv4))
    (license (package-license go-golang-org-x-net-ipv4))))

(define-public go-golang-org-x-net-ipv4
  (let ((commit "ffcf1bedda3b04ebb15a168a59800a73d6dc0f4d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net-ipv4")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ifqw09pj9q23mza1d0im99yy3jp72dvq9dcx2bs1n1m11cjdjzp"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/ipv4"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Go IPv4 support")
      (description "This package provides @code{ipv4}, which implements IP-level
socket options for the Internet Protocol version 4.")
      (home-page "https://go.googlesource.com/net")
      (license bsd-3))))

(define-public go-golang-org-x-net-bpf
  (let ((commit "ffcf1bedda3b04ebb15a168a59800a73d6dc0f4d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net-bpf")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ifqw09pj9q23mza1d0im99yy3jp72dvq9dcx2bs1n1m11cjdjzp"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/bpf"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Berkeley Packet Filters (BPF) in Go")
      (description "This packages provides a Go implementation of the Berkeley
Packet Filter (BPF) virtual machine.")
      (home-page "https://go.googlesource.com/net/")
      (license bsd-3))))

(define-public go-golang-org-x-net-context
  (let ((commit "ffcf1bedda3b04ebb15a168a59800a73d6dc0f4d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net-context")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ifqw09pj9q23mza1d0im99yy3jp72dvq9dcx2bs1n1m11cjdjzp"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/context"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Golang Context type")
      (description "This packages provides @code{context}, which defines the
Context type, which carries deadlines, cancelation signals, and other
request-scoped values across API boundaries and between processes.")
      (home-page "https://go.googlesource.com/net/")
      (license bsd-3))))

(define-public go-golang-org-x-net-internal-iana
  (let ((commit "ffcf1bedda3b04ebb15a168a59800a73d6dc0f4d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net-internal-iana")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ifqw09pj9q23mza1d0im99yy3jp72dvq9dcx2bs1n1m11cjdjzp"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/internal/iana"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Go support for assigned numbers (IANA)")
      (description "This packages provides @code{iana}, which provides protocol
number resources managed by the Internet Assigned Numbers Authority (IANA).")
      (home-page "https://go.googlesource.com/net/")
      (license bsd-3))))

(define-public go-golang-org-x-net-ipv6
  (let ((commit "ffcf1bedda3b04ebb15a168a59800a73d6dc0f4d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net-ipv6")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ifqw09pj9q23mza1d0im99yy3jp72dvq9dcx2bs1n1m11cjdjzp"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/ipv6"
         #:unpack-path "golang.org/x/net"))
      (synopsis "Go IPv6 support")
      (description "This packages provides @code{ipv6}, which implements
IP-level socket options for the Internet Protocol version 6.")
      (home-page "https://go.googlesource.com/net")
      (license bsd-3))))

(define-public go-golang-org-x-net-proxy
  (let ((commit "ffcf1bedda3b04ebb15a168a59800a73d6dc0f4d")
        (revision "0"))
    (package
      (name "go-golang-org-x-net-proxy")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/net")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-net-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1ifqw09pj9q23mza1d0im99yy3jp72dvq9dcx2bs1n1m11cjdjzp"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/net/proxy"
         #:unpack-path "golang.org/x/net/"))
      (synopsis "Go support for network proxies")
      (description "This packages provides @code{proxy}, which provides support
for a variety of protocols to proxy network data.")
      (home-page "https://go.googlesource.com/net")
      (license bsd-3))))

(define-public go-golang-org-x-sys-unix
  (let ((commit "f3918c30c5c2cb527c0b071a27c35120a6c0719a")
        (revision "0"))
    (package
      (name "go-golang-org-x-sys-unix")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/sys")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02967mw0nq7hp39bcf8rdbid4jgz2fn6hd1x03mmavvca03scxbh"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/sys/unix"
         #:unpack-path "golang.org/x/sys"))
      (synopsis "Go support for low-level system interaction")
      (description "This package provides @code{unix}, which offers Go support
for low-level interaction with the operating system.")
      (home-page "https://go.googlesource.com/sys")
      (license bsd-3))))

(define* (go-golang-org-x-text-union #:optional
                                  (packages (list go-golang-org-x-text-transform
                                                  go-golang-org-x-text-unicode-norm)))
  (package
    (name "go-golang-org-x-text")
    (version (package-version go-golang-org-x-text-transform))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories))))))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 packages))
    (synopsis "Union of the Go text libraries")
    (description "A union of the Golang text libraries.")
    (home-page (package-home-page go-golang-org-x-text-transform))
    (license (package-license go-golang-org-x-text-transform))))

(define-public go-golang-org-x-text-transform
  (let ((commit "f4b4367115ec2de254587813edaa901bc1c723a8")
        (revision "0"))
    (package
      (name "go-golang-org-x-text-transform")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/text")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-text-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1a5m97y7sdxks02p4swg8ffp8bgr95aaf5fhfw511p7h3xg1dm0d"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/text/transform"
         #:unpack-path "golang.org/x/text"))
      (synopsis "Go text transformation")
      (description "This package provides @code{transform}, which provides
reader and writer wrappers that transform the bytes passing through.  Example
transformations provided by other packages include normalization and conversion
between character sets.")
      (home-page "https://go.googlesource.com/text")
      (license bsd-3))))

(define-public go-golang-org-x-text-unicode-norm
  (let ((commit "f4b4367115ec2de254587813edaa901bc1c723a8")
        (revision "0"))
    (package
      (name "go-golang-org-x-text-unicode-norm")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/text")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-text-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1a5m97y7sdxks02p4swg8ffp8bgr95aaf5fhfw511p7h3xg1dm0d"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/text/unicode/norm"
         #:unpack-path "golang.org/x/text"))
      (synopsis "Unicode normalization in Go")
      (description "This package provides @code{norm}, which contains types and
functions for normalizing Unicode strings.")
      (home-page "https://go.googlesource.com/text")
      (license bsd-3))))

(define-public go-github-com-audriusbutkevicius-pfilter
  (let ((commit "9dca34a5b530bfc9843fa8aa2ff08ff9821032cb")
        (revision "2"))
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
            "0i4qbnwba49db27fb1y792gcvhb0m744i9q4zgwjbypqmy3bj2a5"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/AudriusButkevicius/pfilter"))
      (synopsis "Filter packets into mulitple virtual connections")
      (description "Pfilter is a Go package for filtering packets into multiple
virtual connections from a single physical connection.")
      (home-page "https://github.com/AudriusButkevicius/pfilter")
      (license expat))))

(define-public go-github-com-ccding-go-stun
  (let ((commit "d9bbe8f8fa7bf7ed03e6cfc6a2796bb36139e1f4")
        (revision "1"))
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
            "04a4q69cmw6snlx54wikyj1y6gk94qzm9xv9als17inmj8z60xv7"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ccding/go-stun"))
      (synopsis "STUN client implementation")
      (description "Go-stun is a go implementation of the STUN client (RFC 3489
and RFC 5389).")
      (home-page "https://github.com/ccding/go-stun")
      (license asl2.0))))

(define-public go-github-com-chmduquesne-rollinghash-adler32
  (let ((commit "043b8fdecc9816f0011a056f6d92f9a091ab63dd")
        (revision "0"))
    (package
      (name "go-github-com-chmduquesne-rollinghash-adler32")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/chmduquesne/rollinghash.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0pc87laxgydqv03bdirfv32y9k0bdk2cwjxn28yh42nvay9p6y0k"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/chmduquesne/rollinghash/adler32"
         #:unpack-path "github.com/chmduquesne/rollinghash"))
      (synopsis "Adler-32 rolling hash in Go")
      (description "This package provides a Go implementation of the Adler-32
rolling hash.")
      (home-page "https://github.com/chmduquesne/rollinghash")
      (license expat))))

(define-public go-github-com-audriusbutkevicius-kcp-go
  (let ((commit "8ae5f528469c6ab76110f41eb7a51341b7efb946")
        (revision "1"))
    (package
      (name "go-github-com-audriusbutkevicius-kcp-go")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/AudriusButkevicius/kcp-go")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1bhg7bfc0r4a7i516xasq3i5ln15lsalm7i53b4nchba6db7mq14"))))
      (build-system go-build-system)
      (propagated-inputs
       `(("go-golang-org-x-net-ipv4" ,go-golang-org-x-net-ipv4)
         ("go-github-com-templexxx-reedsolomon"
          ,go-github-com-templexxx-reedsolomon)
         ("go-github-com-tjfoc-gmsm-sm4" ,go-github-com-tjfoc-gmsm-sm4)
         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
         ("go-golang-org-x-crypto" ,(go-golang-org-x-crypto-union))
         ("go-github-com-templexxx-xor" ,go-github-com-templexxx-xor)))
      (arguments
       '(#:import-path "github.com/AudriusButkevicius/kcp-go"
         ;; 'TestListenerClose' is known to fail. It seems that the test suite
         ;; is not being used upstream:
         ;; https://github.com/AudriusButkevicius/kcp-go/pull/1
         #:tests? #f))
      (synopsis "Reliable UDP connections in Go")
      (description "This package provides @code{kcp-go}, a reliable UDP library
written in Go.  It offers fast, ordered and error-checked delivery of streams
over UDP packets.")
      (home-page "https://github.com/xtaci/kcp-go")
      (license expat))))

(define-public go-github-com-templexxx-xor
  (let ((commit "42f9c041c330b560afb991153bf183c25444bcdc")
        (revision "0"))
    (package
      (name "go-github-com-templexxx-xor")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/templexxx/xor.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0ixzk64nyyzas4lyqxdih824xg5f5vph18vyiibmnflwd61m0i78"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/templexxx/xor"))
      (synopsis "XOR in Go")
      (description "This packages provides a Go implementation of XOR.")
      (home-page "https://github.com/templexxx/xor")
      (license expat))))

(define-public go-github-com-xtaci-smux
  (let ((commit "0f6b9aaecaaf354357adc7def9239011ad276776")
        (revision "0"))
    (package
      (name "go-github-com-xtaci-smux")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/xtaci/smux.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0wx9j0id2f5iqvyalhm4i80fr9k25klr7qqj8sd9acwp5vfl5fas"))))
      (build-system go-build-system)
      (propagated-inputs
       `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
      (arguments
       '(#:import-path "github.com/xtaci/smux"))
      (synopsis "Network multiplexing in Go")
      (description "Smux ( Simple MUltipleXing) is a multiplexing library for
Golang.  It relies on an underlying connection to provide reliability and
ordering, such as TCP or KCP, and provides stream-oriented multiplexing.")
      (home-page "https://github.com/xtaci/smux")
      (license expat))))

(define-public go-github-com-pkg-errors
  (let ((commit "ff09b135c25aae272398c51a07235b90a75aa4f0")
        (revision "0"))
    (package
      (name "go-github-com-pkg-errors")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pkg/errors.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pwl6v3hmc22zp32gkyqykl4kg69xk1mlp0vmhgd1f44difd5fvz"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/pkg/errors"))
      (synopsis "Go error handling primitives")
      (description "This packages provides @code{error}, which offers simple
error handling primitives in Go.")
      (home-page "https://github.com/pkg/errors")
      (license bsd-2))))

(define-public go-golang-org-x-time-rate
  (let ((commit "f51c12702a4d776e4c1fa9b0fabab841babae631")
        (revision "0"))
    (package
      (name "go-golang-org-x-time-rate")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/time")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07wc6g2fvafkr6djsscm0jpbpl4135khhb6kpyx1953hi5d1jvyy"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/time/rate"
         #:unpack-path "golang.org/x/time"))
      (propagated-inputs
       `(("go-golang-org-x-net-context" ,go-golang-org-x-net-context)))
      (synopsis "Rate limiting in Go")
      (description "This package provides @{rate}, which implements rate
limiting in Go.")
      (home-page "https://godoc.org/golang.org/x/time/rate")
      (license bsd-3))))

(define-public go-github-com-petermattis-goid
  (let ((commit "3db12ebb2a599ba4a96bea1c17b61c2f78a40e02")
        (revision "0"))
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
                  "0z18a3mr72c52g7g94n08gxw0ksnaafbfwdl5p5jav2sffirb0kd"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/petermattis/goid"))
      (synopsis "Identify the running goroutine")
      (description "This package offers a method of programatically retrieving
the current goroutine's ID.")
      (home-page "https://github.com/petermattis/goid")
      (license asl2.0))))

(define-public go-github-com-audriusbutkevicius-cli
  (let ((commit "7f561c78b5a4aad858d9fd550c92b5da6d55efbb")
        (revision "0"))
    (package
      (name "go-github-com-audriusbutkevicius-cli")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/AudriusButkevicius/cli.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0bg26pfg25vr16jmczig2m493mja2nxjxyswz3hha7avxw20rpi5"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/AudriusButkevicius/cli"))
      (synopsis "Library for building command-line interfaces in Go")
      (description "This package provides a library for building command-line
interfaces in Go.")
      (home-page "https://github.com/AudriusButkevicius/cli")
      (license expat))))

(define-public go-github-com-kballard-go-shellquote
  (let ((commit "cd60e84ee657ff3dc51de0b4f55dd299a3e136f2")
        (revision "0"))
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
                  "1xjpin4jq1zl84dcn96xhjmn9bsfyszf6g9aqyj2dc0xfi6c88y0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/kballard/go-shellquote"))
      (synopsis "Shell-style string joins and splits")
      (description "Shellquote provides utilities for joining/splitting strings
using sh's word-splitting rules.")
      (home-page "https://github.com/kballard/go-shellquote")
      (license expat))))

(define-public go-github-com-templexxx-reedsolomon
  (package
    (name "go-github-com-templexxx-reedsolomon")
    (version "0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/templexxx/reedsolomon.git")
                     (commit version)))
              (sha256
               (base32
                "05yfi6dq2mwaw6jf9vq2qhpw8vb9y94b3zi6mpfcpma262rxdkg4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/templexxx/reedsolomon"))
    (propagated-inputs
     `(("go-github-com-templexxx-cpufeat"
        ,go-github-com-templexxx-cpufeat)))
    (synopsis "Reed-Solomon Erasure Coding in Go")
    (description "This package provides and implemenation of Reed-Solomon
Erasure Coding in Go.")
    (home-page "https://github.com/templexxx/reedsolomon")
    (license expat)))

(define-public go-github-com-templexxx-cpufeat
  (let ((commit "3794dfbfb04749f896b521032f69383f24c3687e")
        (revision "0"))
    (package
      (name "go-github-com-templexxx-cpufeat")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/templexxx/cpufeat.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0b9f5p6nsiv907rz5d66qzgxnsz4k68r2f45vxi2hwdbnkjfxz8j"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/templexxx/cpufeat"))
      (synopsis "CPU feature identification for Go")
      (description "This package provides @code{cpu}, which implements
processor feature detection used by the Go standard libary.")
      (home-page "https://github.com/templexxx/cpufeat")
      (license bsd-3))))

(define-public go-github-com-tjfoc-gmsm-sm4
  (let ((commit "0f4904804c0f24f1784e10195a4144fcffa86a85")
        (revision "0"))
    (package
      (name "go-github-com-tjfoc-gmsm-sm4")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/tjfoc/gmsm")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1k56gx2ky0c5kf4icafd7zr809lliqzx2mn88lb6d52ljfpf77q5"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/tjfoc/gmsm/sm4"
         #:unpack-path "github.com/tjfoc/gmsm"))
      (synopsis "SM4 block cipher")
      (description "This package provides a Go implementation of SM4, a block
cipher used in the Chinese National Standard for Wireless LAN WAPI (Wired
Authentication and Privacy Infrastructure).")
      (home-page "https://github.com/tjfoc/gmsm")
      (license asl2.0))))

(define-public go-github-com-zillode-notify
  (let ((commit "54e3093eb7377fd139c4605f475cc78e83610b9d")
        (revision "0"))
    (package
      (name "go-github-com-zillode-notify")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/zillode/notify")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xmj0bh15hqbzq5qsbi2nb2lihl1pqdh1vwalwmxywnda3w68xj6"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/zillode/notify"))
      (propagated-inputs
       `(("go-golang-org-x-sys-unix" ,go-golang-org-x-sys-unix)))
      (synopsis "Filesystem event notification library")
      (description "This package provides @code{notify}, a filesystem event
notification library in Go.")
      (home-page "https://github.com/zillode/notify")
      (license expat))))
