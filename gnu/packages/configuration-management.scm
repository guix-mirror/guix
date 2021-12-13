;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu packages configuration-management)
  #:use-module (gnu packages)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public chezmoi
  (package
    (name "chezmoi")
    ;; XXX: Make sure 7f238faa61e46d79b54d4d0ea8f0b5fc27db84b2 applied before
    ;; version update, which should fix @code{password-store} integration.
    (version "1.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/twpayne/chezmoi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b8y0wq3myhvjdnwl0i4x85iil7i7kmsjajvbw1a47afm83jkbaw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/twpayne/chezmoi"
       ;; We don't need to install the source code for end-user applications.
       #:install-source? #f))
    (native-inputs
     (list go-github-com-masterminds-sprig
           go-github-com-masterminds-goutils
           go-github-com-masterminds-semver
           go-github-com-google-uuid
           go-github-com-huandu-xstrings
           go-github-com-imdario-mergo
           go-github-com-mitchellh-reflectwalk
           go-github-com-mitchellh-copystructure
           go-github-com-bmatcuk-doublestar
           go-github-com-charmbracelet-glamour
           go-github-com-alecthomas-chroma
           go-github-com-coreos-go-semver
           go-github-com-danwakefield-fnmatch
           go-github-com-dlclark-regexp2
           go-github-go-git
           go-github-com-google-go-github
           go-github-com-google-go-querystring
           go-github-com-google-renameio
           go-github-com-microcosm-cc-bluemonday
           go-github-com-aymerick-douceur
           go-github-com-chris-ramon-douceur
           go-github-com-gorilla-css
           go-github-com-muesli-reflow-ansi
           go-github-com-muesli-reflow-wordwrap
           go-github-com-muesli-reflow-indent
           go-github-com-muesli-reflow-padding
           go-github-com-muesli-termenv
           go-github-com-google-goterm
           go-golang-org-colorful
           go-github-com-mattn-go-isatty
           go-github.com-mattn-go-runewidth
           go-github-com-olekukonko-tablewriter
           go-github-com-pelletier-go-toml
           go-github-com-pkg-diff
           go-github-com-sergi-go-diff
           go-github-com-spf13-cobra
           go-github-com-spf13-viper
           go-github-com-twpayne-go-shell
           go-github-com-twpayne-go-vfs
           go-github-com-twpayne-go-vfsafero
           go-github-com-twpayne-go-xdg
           go-github-com-yuin-goldmark
           go-github-com-zalando-go-keyring
           go-github-com-godbus-dbus
           go-etcd-io-bbolt
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-github-com-rogpeppe-go-internal
           gopkg-in-errgo-fmt-errors))
    (home-page "https://www.chezmoi.io/")
    (synopsis "Personal configuration files manager")
    (description "This package helps to manage personal configuration files
across multiple machines.")
    (license license:expat)))
