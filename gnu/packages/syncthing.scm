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
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix licenses))

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
  (let ((commit "3c5717caf1475fd25964109a0fc640bd150fce43")
        (revision "0"))
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
                  "0wng25bw885ppiny9rz42kq0a7ddkym5zl0glb8rfk0m8dpvi1dd"))))
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
