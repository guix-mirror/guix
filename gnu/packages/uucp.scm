;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages uucp)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages texinfo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go))

(define-public uucp
  (package
    (name "uucp")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/uucp/uucp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0b5nhl9vvif1w3wdipjsk8ckw49jj1w85xw1mmqi3zbcpazia306"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The old 'configure' script doesn't support the arguments
             ;; that we pass by default.
             (setenv "CONFIG_SHELL" (which "sh"))
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--infodir=" out
                                      "/share/info"))))))))
    (home-page "https://www.gnu.org/software/uucp/uucp.html")
    (synopsis "UUCP protocol implementation")
    (description
     "Taylor UUCP is the GNU implementation of UUCP (Unix-to-Unix Copy), a
set of utilities for remotely transferring files, email and net news
between computers.")
    (license gpl2+)))

(define-public nncp
  (package
    (name "nncp")
    (version "7.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.nncpgo.org/download/nncp-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1r1zgj7gpkdmdm3wf31m0xi8y313kzd4dbyp4r4y8khnp32jvn8l"))
       (modules '((ice-9 ftw)
                  (guix build utils)))
       (snippet
        '(begin
           ;; Unbundle dependencies.
           ;; TODO: go.cypherpunks.ru was down at the time of
           ;; packaging. Unbundle go.cypherpunks dependencies as well once it
           ;; comes back online.
           (for-each (lambda (file)
                       (unless (member file (list "." ".." "go.cypherpunks.ru"))
                         (delete-file-recursively (string-append "src/vendor/" file))))
                     (scandir "src/vendor"))
           ;; Delete built documentation.
           (delete-file "doc/nncp.info")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build go-build-system) #:prefix go:)
                  (guix build utils))
       #:imported-modules ,%go-build-system-modules
       #:phases
       (modify-phases %standard-phases
         (add-before 'unpack 'setup-go-environment
           (assoc-ref go:%standard-phases 'setup-go-environment))
         (add-after 'unpack 'go-unpack
           (lambda* (#:key source #:allow-other-keys)
             ;; Copy source to GOPATH.
             (copy-recursively "src" "../src/go.cypherpunks.ru/nncp/v7")
             ;; Move bundled dependencies to GOPATH.
             (for-each (lambda (dependency)
                         (rename-file (string-append "src/vendor/go.cypherpunks.ru/"
                                                     dependency)
                                      (string-append "../src/go.cypherpunks.ru/"
                                                     dependency)))
                       (list "balloon" "recfile"))
             ;; Delete empty bundled dependencies directory.
             (delete-file-recursively "src/vendor")))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Set configuration path.
               (setenv "CFGPATH" "/etc/nncp.hjson")
               ;; Set output directories.
               (setenv "BINDIR" (string-append out "/bin"))
               (setenv "INFODIR" (string-append out "/share/info"))
               (setenv "DOCDIR" (string-append out "/share/doc/nncp")))
             ;; Set absolute store paths to sh and cat.
             (substitute* (list "src/pipe.go" "src/toss_test.go")
               (("/bin/sh") (which "sh")))
             (substitute* "src/toss_test.go"
               (("; cat") (string-append "; " (which "cat"))))
             ;; Remove module flags.
             (substitute* (list "bin/default.do" "bin/hjson-cli.do" "test.do")
               ((" -mod=vendor") "")
               ((" -m") ""))
             ;; Use the correct module path. `go list` does not report the
             ;; correct module path since we have moved the source files.
             (substitute* "bin/default.do"
               (("^mod=[^\n]*" all) "mod=go.cypherpunks.ru/nncp/v7"))
             ;; Disable timeout in tests. Tests can take longer than the
             ;; default timeout on spinning disks.
             (substitute* "test.do"
               (("test") "test -timeout 0"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "contrib/do" "-c" "test"))))
         (add-after 'install 'remove-go-references
           (assoc-ref go:%standard-phases 'remove-go-references)))))
    (inputs
     (list go-github-com-davecgh-go-xdr
           go-github-com-dustin-go-humanize
           go-github-com-flynn-noise
           go-github-com-gorhill-cronexpr
           go-github-com-hjson-hjson-go
           go-github-com-klauspost-compress
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-term
           go-lukechampine-com-blake3))
    (native-inputs
     (list go texinfo))
    (home-page "http://www.nncpgo.org/")
    (synopsis "Store and forward utilities")
    (description "NNCP (Node to Node copy) is a collection of utilities
simplifying secure store-and-forward files, mail and command exchanging.
These utilities are intended to help build up small size (dozens of nodes)
ad-hoc friend-to-friend (F2F) statically routed darknet delay-tolerant
networks for fire-and-forget secure reliable files, file requests, Internet
mail and commands transmission.  All packets are integrity checked, end-to-end
encrypted, explicitly authenticated by known participants public keys.  Onion
encryption is applied to relayed packets.  Each node acts both as a client and
server, can use push and poll behaviour model.  Multicasting areas, offline
sneakernet/floppynet, dead drops, sequential and append-only CD-ROM/tape
storages, air-gapped computers and online TCP daemon with full-duplex
resumable data transmission exists are all supported.")
    (license gpl3)))
