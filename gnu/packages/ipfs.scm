;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages ipfs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang))

(define-public go-github-com-ipfs-go-ipfs-cmdkit-files
  (let ((commit
          "386fcf8f18a185ec121676665fe2d9574496048d")
        (revision "0"))
    (package
      (name "go-github-com-ipfs-go-ipfs-cmdkit-files")
      (version (git-version "1.1.3" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ipfs/go-ipfs-cmdkit.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "0qk6fshgdmhp8dip2ksm13j6nywi41m9mn0czkvmw6b697z85l2r"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/ipfs/go-ipfs-cmdkit"
         #:import-path "github.com/ipfs/go-ipfs-cmdkit/files"))
      (home-page "https://github.com/ipfs/go-ipfs-cmdkit")
      (synopsis "Shared types, functions and values for go-ipfs")
      (description "@command{cmdkit} offers some types, functions and values
that are shared between @command{go-ipfs/commands} and its rewrite
@command{go-ipfs-cmds}.")
      (license license:expat))))

(define-public go-github-com-ipfs-go-ipfs-api
  (let ((commit
          "dafc2a13a4389ac1a6c2786e34ab70a4f26d3a3f")
        (revision "0"))
    (package
      (name "go-github-com-ipfs-go-ipfs-api")
      (version (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ipfs/go-ipfs-api.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06kibnwb037sqynk99j07wm8alvxwx3bari9gdax4jv93396kycj"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ipfs/go-ipfs-api"
         ;; TODO: Tests fail, might need network access.
         #:tests? #f))
      (native-inputs
       `(("go-github-com-ipfs-go-ipfs-cmdkit-files" ,go-github-com-ipfs-go-ipfs-cmdkit-files)
         ("go-github-com-libp2p-go-libp2p-metrics" ,go-github-com-libp2p-go-libp2p-metrics)
         ("go-github-com-libp2p-go-flow-metrics" ,go-github-com-libp2p-go-flow-metrics)
         ("go-github-com-libp2p-go-libp2p-peer" ,go-github-com-libp2p-go-libp2p-peer)
         ("go-github-com-libp2p-go-libp2p-protocol" ,go-github-com-libp2p-go-libp2p-protocol)
         ("go-github-com-libp2p-go-libp2p-crypto" ,go-github-com-libp2p-go-libp2p-crypto)
         ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
         ("go-github-com-multiformats-go-multiaddr" ,go-github-com-multiformats-go-multiaddr)
         ("go-github-com-multiformats-go-multiaddr-net" ,go-github-com-multiformats-go-multiaddr-net)
         ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
         ("go-github-com-gogo-protobuf-proto" ,go-github-com-gogo-protobuf-proto)
         ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
         ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
         ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
         ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
         ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)
         ("go-golang-org-x-crypto-ed25519" ,go-golang-org-x-crypto-ed25519)
         ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)
         ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
         ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
         ("go-github-com-whyrusleeping-tar-utils" ,go-github-com-whyrusleeping-tar-utils)
         ("go-github-com-cheekybits-is" ,go-github-com-cheekybits-is)))
      (home-page "https://github.com/ipfs/go-ipfs-api")
      (synopsis "Unofficial Go interface to IPFS's HTTP API")
      (description "An unofficial Go interface to IPFS's HTTP API")
      (license license:expat))))

(define-public gx
  (package
    (name "gx")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/gx.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0pfx2p59xdbmqzfbgaf8xvlnzh8m05hkg596glq5kvl8ib65i4ha"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/whyrusleeping/gx"))
    (native-inputs
     `(("go-github-com-blang-semver" ,go-github-com-blang-semver)
       ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
       ("go-github-com-ipfs-go-ipfs-api" ,go-github-com-ipfs-go-ipfs-api)
       ("go-github-com-ipfs-go-ipfs-cmdkit-files" ,go-github-com-ipfs-go-ipfs-cmdkit-files)
       ("go-github-com-libp2p-go-flow-metrics" ,go-github-com-libp2p-go-flow-metrics)
       ("go-github-com-libp2p-go-libp2p-crypto" ,go-github-com-libp2p-go-libp2p-crypto)
       ("go-github-com-libp2p-go-libp2p-metrics" ,go-github-com-libp2p-go-libp2p-metrics)
       ("go-github-com-libp2p-go-libp2p-peer" ,go-github-com-libp2p-go-libp2p-peer)
       ("go-github-com-libp2p-go-libp2p-protocol" ,go-github-com-libp2p-go-libp2p-protocol)
       ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
       ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
       ("go-github-com-multiformats-go-multiaddr" ,go-github-com-multiformats-go-multiaddr)
       ("go-github-com-multiformats-go-multiaddr-net" ,go-github-com-multiformats-go-multiaddr-net)
       ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
       ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
       ("go-github-com-whyrusleeping-tar-utils" ,go-github-com-whyrusleeping-tar-utils)
       ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
       ("go-github-com-gogo-protobuf-proto" ,go-github-com-gogo-protobuf-proto)
       ("go-github-com-sabhiram-go-gitignore" ,go-github-com-sabhiram-go-gitignore)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
       ("go-github-com-whyrusleeping-json-filter" ,go-github-com-whyrusleeping-json-filter)
       ("go-github-com-whyrusleeping-progmeter" ,go-github-com-whyrusleeping-progmeter)
       ("go-github-com-whyrusleeping-stump" ,go-github-com-whyrusleeping-stump)
       ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)
       ("go-golang-org-x-crypto-ed25519" ,go-golang-org-x-crypto-ed25519)
       ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)))
    (home-page "https://github.com/whyrusleeping/gx")
    (synopsis "Package management tool using IPFS")
    (description "@command{gx} is a packaging tool built around the
distributed, content addressed filesystem IPFS.  It aims to be flexible,
powerful and simple.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-gx-util
  (package
    (inherit gx)
    (name "go-github-com-whyrusleeping-gx-util")
    (arguments
     '(#:unpack-path "github.com/whyrusleeping/gx"
       #:import-path "github.com/whyrusleeping/gx/gxutil"))))

(define-public gx-go
  (package
    (name "gx-go")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/gx-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fdy4b3ymqw6hzvvjwq37mfrdmizc8lxm53axw93n3x6118na9jc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/whyrusleeping/gx-go"))
    (native-inputs
     `(("go-github-com-whyrusleeping-gx-util" ,go-github-com-whyrusleeping-gx-util)
       ("go-github-com-kr-fs" ,go-github-com-kr-fs)
       ("go-github-com-gxed-hashland-keccakpg" ,go-github-com-gxed-hashland-keccakpg)
       ("go-github-com-ipfs-go-ipfs-api" ,go-github-com-ipfs-go-ipfs-api)
       ("go-github-com-ipfs-go-ipfs-cmdkit-files" ,go-github-com-ipfs-go-ipfs-cmdkit-files)
       ("go-github-com-libp2p-go-flow-metrics" ,go-github-com-libp2p-go-flow-metrics)
       ("go-github-com-libp2p-go-libp2p-crypto" ,go-github-com-libp2p-go-libp2p-crypto)
       ("go-github-com-libp2p-go-libp2p-metrics" ,go-github-com-libp2p-go-libp2p-metrics)
       ("go-github-com-libp2p-go-libp2p-peer" ,go-github-com-libp2p-go-libp2p-peer)
       ("go-github-com-libp2p-go-libp2p-protocol" ,go-github-com-libp2p-go-libp2p-protocol)
       ("go-github-com-minio-blake2b-simd" ,go-github-com-minio-blake2b-simd)
       ("go-github-com-minio-sha256-simd" ,go-github-com-minio-sha256-simd)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-mr-tron-base58" ,go-github-com-mr-tron-base58)
       ("go-github-com-multiformats-go-multiaddr" ,go-github-com-multiformats-go-multiaddr)
       ("go-github-com-multiformats-go-multiaddr-net" ,go-github-com-multiformats-go-multiaddr-net)
       ("go-github-com-multiformats-go-multihash" ,go-github-com-multiformats-go-multihash)
       ("go-github-com-spaolacci-murmur3" ,go-github-com-spaolacci-murmur3)
       ("go-github-com-whyrusleeping-tar-utils" ,go-github-com-whyrusleeping-tar-utils)
       ("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
       ("go-github-com-gogo-protobuf-proto" ,go-github-com-gogo-protobuf-proto)
       ("go-github-com-sabhiram-go-gitignore" ,go-github-com-sabhiram-go-gitignore)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
       ("go-github-com-whyrusleeping-progmeter" ,go-github-com-whyrusleeping-progmeter)
       ("go-github-com-whyrusleeping-stump" ,go-github-com-whyrusleeping-stump)
       ("go-golang-org-x-crypto-blake2s" ,go-golang-org-x-crypto-blake2s)
       ("go-golang-org-x-crypto-ed25519" ,go-golang-org-x-crypto-ed25519)
       ("go-golang-org-x-crypto-sha3" ,go-golang-org-x-crypto-sha3)))
    (home-page "https://github.com/whyrusleeping/gx-go")
    (synopsis "Golang subtool for the @command{gx} package manager")
    (description "A subtool for the @command{gx} package manager for packages
written in Go.")
    (license license:expat)))

(define-public go-ipfs
  (package
    (name "go-ipfs")
    (version "0.4.19")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append
             "https://dist.ipfs.io/go-ipfs/v" version
             "/go-ipfs-source.tar.gz"))
       (sha256
        (base32 "0s04ap14p6hnipjm27nm5k8s28zv9k5g9mziyh3ibgwn7dzb1kpx"))
       (file-name (string-append name "-" version "-source"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/ipfs/go-ipfs"
       #:import-path "github.com/ipfs/go-ipfs/cmd/ipfs"
       #:phases (modify-phases %standard-phases
                  (add-before 'reset-gzip-timestamps 'make-files-writable
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Make sure .gz files are writable so that the
                      ;; 'reset-gzip-timestamps' phase can do its work.
                      (let ((out (assoc-ref outputs "out")))
                        (for-each make-file-writable
                                  (find-files out "\\.gz$"))
                        #t))))))
    (home-page "https://ipfs.io")
    (synopsis "Go implementation of IPFS, a peer-to-peer hypermedia protocol")
    (description "IPFS is a global, versioned, peer-to-peer filesystem.  It
combines good ideas from Git, BitTorrent, Kademlia, SFS, and the Web.  It is
like a single bittorrent swarm, exchanging git objects.  IPFS provides an
interface as simple as the HTTP web, but with permanence built in.  You can
also mount the world at @code{/ipfs}.")
    (license license:expat)))
