;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020 Helio Machado <0x2b3bfa0+guix@googlemail.com>
;;; Copyright © 2021 François Joulaud <francois.joulaud@radiofrance.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (guix import go)
  #:use-module (guix build-system go)
  #:use-module (guix git)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (string-replace-substring))
  #:use-module (guix http-client)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (htmlprag)               ;from Guile-Lib
  #:autoload   (guix git) (update-cached-checkout)
  #:autoload   (gcrypt hash) (open-hash-port hash-algorithm sha256)
  #:autoload   (guix serialization) (write-file)
  #:autoload   (guix base32) (bytevector->nix-base32-string)
  #:autoload   (guix build utils) (mkdir-p)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module ((rnrs io ports) #:select (call-with-port))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (sxml match)
  #:use-module ((sxml xpath) #:renamer (lambda (s)
                                         (if (eq? 'filter s)
                                             'xfilter
                                             s)))
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)

  #:export (go-module->guix-package
            go-module-recursive-import))

;;; Parameterize htmlprag to parse valid HTML more reliably.
(%strict-tokenizer? #t)

;;; Commentary:
;;;
;;; (guix import go) attempts to make it easier to create Guix package
;;; declarations for Go modules.
;;;
;;; Modules in Go are a "collection of related Go packages" which are "the
;;; unit of source code interchange and versioning".  Modules are generally
;;; hosted in a repository.
;;;
;;; At this point it should handle correctly modules which have only Go
;;; dependencies and are accessible from proxy.golang.org (or configured via
;;; GOPROXY).
;;;
;;; We want it to work more or less this way:
;;; - get latest version for the module from GOPROXY
;;; - infer VCS root repo from which we will check-out source by
;;;   + recognising known patterns (like github.com)
;;;   + or recognizing .vcs suffix
;;;   + or parsing meta tag in HTML served at the URL
;;;   + or (TODO) if nothing else works by using zip file served by GOPROXY
;;; - get go.mod from GOPROXY (which is able to synthetize one if needed)
;;; - extract list of dependencies from this go.mod
;;;
;;; The Go module paths are translated to a Guix package name under the
;;; assumption that there will be no collision.

;;; TODO list
;;; - get correct hash in vcs->origin
;;; - print partial result during recursive imports (need to catch
;;;   exceptions)

;;; Code:

(define http-fetch*
  ;; Like http-fetch, but memoized and returning the body as a string.
  (memoize (lambda args
             (call-with-port (apply http-fetch args) get-string-all))))

(define json-fetch*
  (memoize json-fetch))

(define (go-path-escape path)
  "Escape a module path by replacing every uppercase letter with an
exclamation mark followed with its lowercase equivalent, as per the module
Escaped Paths specification (see:
https://godoc.org/golang.org/x/mod/module#hdr-Escaped_Paths)."
  (define (escape occurrence)
    (string-append "!" (string-downcase (match:substring occurrence))))
  (regexp-substitute/global #f "[A-Z]" path 'pre escape 'post))

;; Prevent inlining of this procedure, which is accessed by unit tests.
(set! go-path-escape go-path-escape)

(define (go.pkg.dev-info name)
  (http-fetch* (string-append "https://pkg.go.dev/" name)))

(define (go-module-latest-version goproxy-url module-path)
  "Fetch the version number of the latest version for MODULE-PATH from the
given GOPROXY-URL server."
  (assoc-ref (json-fetch* (format #f "~a/~a/@latest" goproxy-url
                                  (go-path-escape module-path)))
             "Version"))

(define (go-package-licenses name)
  "Retrieve the list of licenses that apply to NAME, a Go package or module
name (e.g. \"github.com/golang/protobuf/proto\")."
  (let* ((body (go.pkg.dev-info (string-append name "?tab=licenses")))
         ;; Extract the text contained in a h2 child node of any
         ;; element marked with a "License" class attribute.
         (select (sxpath `(// (* (@ (equal? (class "License"))))
                              h2 // *text*))))
    (select (html->sxml body))))

(define (sxml->texi sxml-node)
  "A very basic SXML to Texinfo converter which attempts to preserve HTML
formatting and links as text."
  (sxml-match sxml-node
    ((strong ,text)
     (format #f "@strong{~a}" text))
    ((a (@ (href ,url)) ,text)
     (format #f "@url{~a,~a}" url text))
    ((code ,text)
     (format #f "@code{~a}" text))
    (,something-else something-else)))

(define (go-package-description name)
  "Retrieve a short description for NAME, a Go package name,
e.g. \"google.golang.org/protobuf/proto\"."
  (let* ((body (go.pkg.dev-info name))
         (sxml (html->sxml body))
         (overview ((sxpath
                     `(//
                       (* (@ (equal? (class "Documentation-overview"))))
                       (p 1))) sxml))
         ;; Sometimes, the first paragraph just contains images/links that
         ;; has only "\n" for text.  The following filter is designed to
         ;; omit it.
         (contains-text? (lambda (node)
                           (remove string-null?
                                   (map string-trim-both
                                        (filter (node-typeof? '*text*)
                                                (cdr node))))))
         (select-content (sxpath
                          `(//
                            (* (@ (equal? (class "UnitReadme-content"))))
                            div // p ,(xfilter contains-text?))))
         ;; Fall-back to use content; this is less desirable as it is more
         ;; verbose, but not every page has an overview.
         (description (if (not (null? overview))
                          overview
                          (select-content sxml)))
         (description* (and (not (null? description))
                            (first description))))
    (match description*
      (() #f)                           ;nothing selected
      ((p elements ...)
       (apply string-append (filter string? (map sxml->texi elements)))))))

(define (go-package-synopsis module-name)
  "Retrieve a short synopsis for a Go module named MODULE-NAME,
e.g. \"google.golang.org/protobuf\".  The data is scraped from
the https://pkg.go.dev/ web site."
  ;; Note: Only the *module* (rather than package) page has the README title
  ;; used as a synopsis on the https://pkg.go.dev web site.
  (let* ((url (string-append "https://pkg.go.dev/" module-name))
         (body (http-fetch* url))
         ;; Extract the text contained in a h2 child node of any
         ;; element marked with a "License" class attribute.
         (select-title (sxpath
                        `(// (div (@ (equal? (class "UnitReadme-content"))))
                             // h3 *text*))))
    (match (select-title (html->sxml body))
      (() #f)                           ;nothing selected
      ((title more ...)                 ;title is the first string of the list
       (string-trim-both title)))))

(define (list->licenses licenses)
  "Given a list of LICENSES mostly following the SPDX conventions, return the
corresponding Guix license or 'unknown-license!"
  (filter-map (lambda (license)
                (and (not (string-null? license))
                     (not (any (cut string=? <> license)
                               '("AND" "OR" "WITH")))
                     ;; Adjust the license names scraped from
                     ;; https://pkg.go.dev to an equivalent SPDX identifier,
                     ;; if they differ (see: https://github.com/golang/pkgsite
                     ;; /internal/licenses/licenses.go#L174).
                     (or (spdx-string->license
                          (match license
                            ("BlueOak-1.0" "BlueOak-1.0.0")
                            ("BSD-0-Clause" "0BSD")
                            ("BSD-2-Clause" "BSD-2-Clause-FreeBSD")
                            ("GPL2" "GPL-2.0")
                            ("GPL3" "GPL-3.0")
                            ("NIST" "NIST-PD")
                            (_ license)))
                         'unknown-license!)))
              licenses))

(define (fetch-go.mod goproxy module-path version)
  "Fetch go.mod from the given GOPROXY server for the given MODULE-PATH
and VERSION and return an input port."
  (let ((url (format #f "~a/~a/@v/~a.mod" goproxy
                     (go-path-escape module-path)
                     (go-path-escape version))))
    (http-fetch* url)))

(define %go.mod-require-directive-rx
  ;; A line in a require directive is composed of a module path and
  ;; a version separated by whitespace and an optionnal '//' comment at
  ;; the end.
  (make-regexp
   (string-append
    "^[[:blank:]]*"
    "([^[:blank:]]+)[[:blank:]]+([^[:blank:]]+)"
    "([[:blank:]]+//.*)?")))

(define %go.mod-replace-directive-rx
  ;; ReplaceSpec = ModulePath [ Version ] "=>" FilePath newline
  ;;             | ModulePath [ Version ] "=>" ModulePath Version newline .
  (make-regexp
   (string-append
    "([^[:blank:]]+)([[:blank:]]+([^[:blank:]]+))?"
    "[[:blank:]]+" "=>" "[[:blank:]]+"
    "([^[:blank:]]+)([[:blank:]]+([^[:blank:]]+))?")))

(define (parse-go.mod content)
  "Parse the go.mod file CONTENT, returning a list of requirements."
  (define-record-type <results>
    (make-results requirements replacements)
    results?
    (requirements results-requirements)
    (replacements results-replacements))
  ;; We parse only a subset of https://golang.org/ref/mod#go-mod-file-grammar
  ;; which we think necessary for our use case.
  (define (toplevel results)
    "Main parser, RESULTS is a pair of alist serving as accumulator for
     all encountered requirements and replacements."
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        ;; parsing ended, give back the result
        results)
       ((string=? line "require (")
        ;; a require block begins, delegate parsing to IN-REQUIRE
        (in-require results))
       ((string=? line "replace (")
        ;; a replace block begins, delegate parsing to IN-REPLACE
        (in-replace results))
       ((string-prefix? "require " line)
        ;; a standalone require directive
        (let* ((stripped-line (string-drop line 8))
               (new-results (require-directive results stripped-line)))
          (toplevel new-results)))
       ((string-prefix? "replace " line)
        ;; a standalone replace directive
        (let* ((stripped-line (string-drop line 8))
               (new-results (replace-directive results stripped-line)))
          (toplevel new-results)))
       (#t
        ;; unrecognised line, ignore silently
        (toplevel results)))))

  (define (in-require results)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        ;; this should never happen here but we ignore silently
        results)
       ((string=? line ")")
        ;; end of block, coming back to toplevel
        (toplevel results))
       (#t
        (in-require (require-directive results line))))))

  (define (in-replace results)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        ;; this should never happen here but we ignore silently
        results)
       ((string=? line ")")
        ;; end of block, coming back to toplevel
        (toplevel results))
       (#t
        (in-replace (replace-directive results line))))))

  (define (replace-directive results line)
    "Extract replaced modules and new requirements from replace directive
    in LINE and add to RESULTS."
    (match results
      (($ <results> requirements replaced)
       (let* ((rx-match (regexp-exec %go.mod-replace-directive-rx line))
              (module-path (match:substring rx-match 1))
              (version (match:substring rx-match 3))
              (new-module-path (match:substring rx-match 4))
              (new-version (match:substring rx-match 6))
              (new-replaced (alist-cons module-path version replaced))
              (new-requirements
               (if (string-match "^\\.?\\./" new-module-path)
                   requirements
                   (alist-cons new-module-path new-version requirements))))
         (make-results new-requirements new-replaced)))))
  (define (require-directive results line)
    "Extract requirement from LINE and add it to RESULTS."
    (let* ((rx-match (regexp-exec %go.mod-require-directive-rx line))
           (module-path (match:substring rx-match 1))
           ;; we saw double-quoted string in the wild without escape
           ;; sequences so we just trim the quotes
           (module-path (string-trim-both module-path #\"))
           (version (match:substring rx-match 2)))
      (match results
        (($ <results> requirements replaced)
         (make-results (alist-cons module-path version requirements) replaced)))))

  (let ((results (with-input-from-string content
                   (lambda _
                     (toplevel (make-results '() '()))))))
    (match results
      (($ <results> requirements replaced)
       ;; At last we remove replaced modules from the requirements list
       (fold
        (lambda (replacedelem requirements)
          (alist-delete! (car replacedelem) requirements))
        requirements
        replaced)))))

;; Prevent inlining of this procedure, which is accessed by unit tests.
(set! parse-go.mod parse-go.mod)

(define-record-type <vcs>
  (%make-vcs url-prefix root-regex type)
  vcs?
  (url-prefix vcs-url-prefix)
  (root-regex vcs-root-regex)
  (type vcs-type))

(define (make-vcs prefix regexp type)
  (%make-vcs prefix (make-regexp regexp) type))

(define known-vcs
  ;; See the following URL for the official Go equivalent:
  ;; https://github.com/golang/go/blob/846dce9d05f19a1f53465e62a304dea21b99f910/src/cmd/go/internal/vcs/vcs.go#L1026-L1087
    (list
     (make-vcs
      "github.com"
      "^(github\\.com/[A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+)(/[A-Za-z0-9_.\\-]+)*$"
      'git)
     (make-vcs
      "bitbucket.org"
      "^(bitbucket\\.org/([A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+))(/[A-Za-z0-9_.\\-]+)*$"
      'unknown)
     (make-vcs
      "hub.jazz.net/git/"
      "^(hub\\.jazz\\.net/git/[a-z0-9]+/[A-Za-z0-9_.\\-]+)(/[A-Za-z0-9_.\\-]+)*$"
      'git)
     (make-vcs
      "git.apache.org"
      "^(git\\.apache\\.org/[a-z0-9_.\\-]+\\.git)(/[A-Za-z0-9_.\\-]+)*$"
      'git)
     (make-vcs
      "git.openstack.org"
      "^(git\\.openstack\\.org/[A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+)(\\.git)?\
(/[A-Za-z0-9_.\\-]+)*$"
      'git)))

(define (module-path->repository-root module-path)
  "Infer the repository root from a module path.  Go modules can be
defined at any level of a repository tree, but querying for the meta tag
usually can only be done from the web page at the root of the repository,
hence the need to derive this information."

  ;; For reference, see: https://golang.org/ref/mod#vcs-find.
  (define vcs-qualifiers '(".bzr" ".fossil" ".git" ".hg" ".svn"))

  (define (vcs-qualified-module-path->root-repo-url module-path)
    (let* ((vcs-qualifiers-group (string-join vcs-qualifiers "|"))
           (pattern (format #f "^(.*(~a))(/|$)" vcs-qualifiers-group))
           (m (string-match pattern module-path)))
      (and=> m (cut match:substring <> 1))))

  (or (and=> (find (lambda (vcs)
                     (string-prefix? (vcs-url-prefix vcs) module-path))
                   known-vcs)
             (lambda (vcs)
               (match:substring (regexp-exec (vcs-root-regex vcs)
                                             module-path) 1)))
      (vcs-qualified-module-path->root-repo-url module-path)
      module-path))

(define (go-module->guix-package-name module-path)
  "Converts a module's path to the canonical Guix format for Go packages."
  (string-downcase (string-append "go-" (string-replace-substring
                                         (string-replace-substring
                                          (string-replace-substring
                                           module-path
                                           "." "-")
                                          "/" "-")
                                         "_" "-"))))

(define (strip-.git-suffix/maybe repo-url)
  "Strip a repository URL '.git' suffix from REPO-URL if hosted at GitHub."
  (match repo-url
    ((and (? (cut string-prefix? "https://github.com" <>))
          (? (cut string-suffix? ".git" <>)))
     (string-drop-right repo-url 4))
    (_ repo-url)))

(define-record-type <module-meta>
  (make-module-meta import-prefix vcs repo-root)
  module-meta?
  (import-prefix module-meta-import-prefix)
  (vcs module-meta-vcs)                 ;a symbol
  (repo-root module-meta-repo-root))

(define (fetch-module-meta-data module-path)
  "Retrieve the module meta-data from its landing page.  This is necessary
because goproxy servers don't currently provide all the information needed to
build a package."
  ;; <meta name="go-import" content="import-prefix vcs repo-root">
  (let* ((meta-data (http-fetch* (format #f "https://~a?go-get=1" module-path)))
         (select (sxpath `(// head (meta (@ (equal? (name "go-import"))))
                              // content))))
    (match (select (html->sxml meta-data))
      (() #f)                           ;nothing selected
      (((content content-text))
       (match (string-split content-text #\space)
         ((root-path vcs repo-url)
          (make-module-meta root-path (string->symbol vcs)
                            (strip-.git-suffix/maybe repo-url))))))))

(define (module-meta-data-repo-url meta-data goproxy)
  "Return the URL where the fetcher which will be used can download the
source."
  (if (member (module-meta-vcs meta-data) '(fossil mod))
      goproxy
      (module-meta-repo-root meta-data)))

;; XXX: Copied from (guix scripts hash).
(define (vcs-file? file stat)
  (case (stat:type stat)
    ((directory)
     (member (basename file) '(".bzr" ".git" ".hg" ".svn" "CVS")))
    ((regular)
     ;; Git sub-modules have a '.git' file that is a regular text file.
     (string=? (basename file) ".git"))
    (else
     #f)))

;; XXX: Adapted from 'file-hash' in (guix scripts hash).
(define* (file-hash file #:optional (algorithm (hash-algorithm sha256)))
  ;; Compute the hash of FILE.
  (let-values (((port get-hash) (open-hash-port algorithm)))
    (write-file file port #:select? (negate vcs-file?))
    (force-output port)
    (get-hash)))

(define* (git-checkout-hash url reference algorithm)
  "Return the ALGORITHM hash of the checkout of URL at REFERENCE, a commit or
tag."
  (define cache
    (string-append (or (getenv "TMPDIR") "/tmp")
                   "/guix-import-go-"
                   (passwd:name (getpwuid (getuid)))))

  ;; Use a custom cache to avoid cluttering the default one under
  ;; ~/.cache/guix, but choose one under /tmp so that it's persistent across
  ;; subsequent "guix import" invocations.
  (mkdir-p cache)
  (chmod cache #o700)
  (let-values (((checkout commit _)
                (parameterize ((%repository-cache-directory cache))
                  (update-cached-checkout url
                                          #:ref
                                          `(tag-or-commit . ,reference)))))
    (file-hash checkout algorithm)))

(define (vcs->origin vcs-type vcs-repo-url version)
  "Generate the `origin' block of a package depending on what type of source
control system is being used."
  (case vcs-type
    ((git)
     (let ((plain-version? (string=? version (go-version->git-ref version)))
           (v-prefixed?    (string-prefix? "v" version)))
       `(origin
          (method git-fetch)
          (uri (git-reference
                (url ,vcs-repo-url)
                ;; This is done because the version field of the package,
                ;; which the generated quoted expression refers to, has been
                ;; stripped of any 'v' prefixed.
                (commit ,(if (and plain-version? v-prefixed?)
                             '(string-append "v" version)
                             '(go-version->git-ref version)))))
          (file-name (git-file-name name version))
          (sha256
           (base32
            ,(bytevector->nix-base32-string
              (git-checkout-hash vcs-repo-url (go-version->git-ref version)
                                 (hash-algorithm sha256))))))))
    ((hg)
     `(origin
        (method hg-fetch)
        (uri (hg-reference
              (url ,vcs-repo-url)
              (changeset ,version)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          ;; FIXME: populate hash for hg repo checkout
          "0000000000000000000000000000000000000000000000000000"))))
    ((svn)
     `(origin
        (method svn-fetch)
        (uri (svn-reference
              (url ,vcs-repo-url)
              (revision (string->number version))))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          ;; FIXME: populate hash for svn repo checkout
          "0000000000000000000000000000000000000000000000000000"))))
    (else
     (raise
      (formatted-message (G_ "unsupported vcs type '~a' for package '~a'")
                         vcs-type vcs-repo-url)))))

(define* (go-module->guix-package module-path #:key
                                  (goproxy-url "https://proxy.golang.org"))
  (let* ((latest-version (go-module-latest-version goproxy-url module-path))
         (content (fetch-go.mod goproxy-url module-path latest-version))
         (dependencies (map car (parse-go.mod content)))
         (guix-name (go-module->guix-package-name module-path))
         (root-module-path (module-path->repository-root module-path))
         ;; The VCS type and URL are not included in goproxy information. For
         ;; this we need to fetch it from the official module page.
         (meta-data (fetch-module-meta-data root-module-path))
         (vcs-type (module-meta-vcs meta-data))
         (vcs-repo-url (module-meta-data-repo-url meta-data goproxy-url))
         (synopsis (go-package-synopsis root-module-path))
         (description (go-package-description module-path))
         (licenses (go-package-licenses module-path)))
    (values
     `(package
        (name ,guix-name)
        ;; Elide the "v" prefix Go uses
        (version ,(string-trim latest-version #\v))
        (source
         ,(vcs->origin vcs-type vcs-repo-url latest-version))
        (build-system go-build-system)
        (arguments
         '(#:import-path ,root-module-path))
        ,@(maybe-propagated-inputs
           (map go-module->guix-package-name dependencies))
        (home-page ,(format #f "https://~a" root-module-path))
        (synopsis ,synopsis)
        (description ,(and=> description beautify-description))
        (license ,(match (list->licenses licenses)
                    (() #f)             ;unknown license
                    ((license)          ;a single license
                     license)
                    ((license ...)      ;a list of licenses
                     `(list ,@license)))))
     dependencies)))

(define go-module->guix-package* (memoize go-module->guix-package))

(define* (go-module-recursive-import package-name
                                     #:key (goproxy-url "https://proxy.golang.org"))
  (recursive-import
   package-name
   #:repo->guix-package (lambda* (name . _)
                          (go-module->guix-package*
                           name
                           #:goproxy-url goproxy-url))
   #:guix-name go-module->guix-package-name))
