;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 François Joulaud <francois.joulaud@radiofrance.com>
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

;;; Summary
;; Tests for guix/import/go.scm

(define-module (test-import-go)
  #:use-module (guix base32)
  #:use-module (guix build-system go)
  #:use-module (guix import go)
  #:use-module (guix base32)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (guix tests)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (web response))

(define parse-go.mod
  (@@ (guix import go) parse-go.mod))

(define fixture-go-mod-simple
  "module my/thing
go 1.12
require other/thing v1.0.2
require new/thing/v2 v2.3.4
exclude old/thing v1.2.3
replace bad/thing v1.4.5 => good/thing v1.4.5
")

(define fixture-go-mod-with-block
  "module M

require (
         A v1
         B v1.0.0
         C v1.0.0
         D v1.2.3
         E dev
)

exclude D v1.2.3
")


(define fixture-go-mod-complete
  "module M

go 1.13

replace github.com/myname/myproject/myapi => ./api

replace github.com/mymname/myproject/thissdk => ../sdk

replace launchpad.net/gocheck => github.com/go-check/check v0.0.0-20140225173054-eb6ee6f84d0a

require (
	github.com/user/project v1.1.11
	github.com/user/project/sub/directory v1.1.12
	bitbucket.org/user/project v1.11.20
	bitbucket.org/user/project/sub/directory v1.11.21
	launchpad.net/project v1.1.13
	launchpad.net/project/series v1.1.14
	launchpad.net/project/series/sub/directory v1.1.15
	launchpad.net/~user/project/branch v1.1.16
	launchpad.net/~user/project/branch/sub/directory v1.1.17
	hub.jazz.net/git/user/project v1.1.18
	hub.jazz.net/git/user/project/sub/directory v1.1.19
	k8s.io/kubernetes/subproject v1.1.101
	one.example.com/abitrary/repo v1.1.111
	two.example.com/abitrary/repo v0.0.2
	\"quoted.example.com/abitrary/repo\" v0.0.2
)

replace two.example.com/abitrary/repo => github.com/corp/arbitrary-repo v0.0.2

replace (
	golang.org/x/sys => golang.org/x/sys v0.0.0-20190813064441-fde4db37ae7a // pinned to release-branch.go1.13
	golang.org/x/tools => golang.org/x/tools v0.0.0-20190821162956-65e3620a7ae7 // pinned to release-branch.go1.13
)

")



(define fixture-latest-for-go-check
  "{\"Version\":\"v0.0.0-20201130134442-10cb98267c6c\",\"Time\":\"2020-11-30T13:44:42Z\"}")


(define fixtures-go-check-test
  (let ((version
           "{\"Version\":\"v0.0.0-20201130134442-10cb98267c6c\",\"Time\":\"2020-11-30T13:44:42Z\"}")
        (go.mod
          "module gopkg.in/check.v1

go 1.11

require github.com/kr/pretty v0.2.1
")
        (go-get
           "<!DOCTYPE html>
<html lang=\"en\" >
  <head>
    <meta charset=\"utf-8\">
  <link rel=\"dns-prefetch\" href=\"https://github.githubassets.com\">
    <script crossorigin=\"anonymous\" defer=\"defer\" integrity=\"sha512-aw5tciVT0IsECUmMuwp9ez60QReE2/yFNL1diLgZnOom6RhU8+0lG3RlAKto4JwbCoEP15E41Pksd7rK5BKfCQ==\" type=\"application/javascript\" src=\"https://github.githubassets.com/assets/topic-suggestions-6b0e6d72.js\"></script>
      <meta name=\"viewport\" content=\"width=device-width\">

   <title>GitHub - go-check/check: Rich testing for the Go language</title>
   <meta name=\"description\" content=\"Rich testing for the Go language. Contribute to go-check/check development by creating an account on GitHub.\">
   <link rel=\"search\" type=\"application/opensearchdescription+xml\" href=\"/opensearch.xml\" title=\"GitHub\">
   <link rel=\"fluid-icon\" href=\"https://github.com/fluidicon.png\" title=\"GitHub\">
   <!-- To prevent page flashing, the optimizely JS needs to be loaded in the
                     <head> tag before the DOM renders -->
   <meta name=\"hostname\" content=\"github.com\">
   <meta name=\"user-login\" content=\"\">
   <link href=\"https://github.com/go-check/check/commits/v1.atom\" rel=\"alternate\" title=\"Recent Commits to check:v1\" type=\"application/atom+xml\">
   <meta name=\"go-import\" content=\"github.com/go-check/check git https://github.com/go-check/check.git\">
  </head>
  <body class=\"logged-out env-production page-responsive\" style=\"word-wrap: break-word;\">
  </body>
</html>
")
        (pkg.go.dev "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"utf-8\">\n</head>\n<body class=\"Site Site--wide Site--redesign\">\n  <div class=\"Site-content\">\n    <div class=\"Container\">\n      <div class=\"UnitDetails\" data-test-id=\"UnitDetails\">\n        <div class=\"UnitDetails-content js-unitDetailsContent\" role=\"main\" data-test-id=\"UnitDetails-content\">\n          <div class=\"UnitReadme js-readme\">\n            <h2 class=\"UnitReadme-title\" id=\"section-readme\"><img height=\"25px\" width=\"20px\" src=\"/static/img/pkg-icon-readme_20x16.svg\" alt=\"\">README</h2>\n            <div class=\"UnitReadme-content\" data-test-id=\"Unit-readmeContent\">\n              <div class=\"Overview-readmeContent js-readmeContent\">\n                <h3 class=\"h1\" id=\"readme-instructions\">Instructions</h3>\n                <p>Install the package with:</p>\n                <pre><code>go get gopkg.in/check.v1\n</code></pre>\n              </div>\n              <div class=\"UnitReadme-fadeOut\"></div>\n            </div>\n          </div>\n          <div class=\"UnitDoc\">\n            <h2 class=\"UnitDoc-title\" id=\"section-documentation\"><img height=\"25px\" width=\"20px\" src=\"/static/img/pkg-icon-doc_20x12.svg\" alt=\"\">Documentation</h2>\n            <div class=\"Documentation js-documentation\">\n              <div class=\"Documentation-content js-docContent\">\n                <section class=\"Documentation-overview\">\n                  <h3 tabindex=\"-1\" id=\"pkg-overview\" class=\"Documentation-overviewHeader\">Overview <a href=\"#pkg-overview\">¶</a></h3>\n                  <div role=\"navigation\" aria-label=\"Table of Contents\">\n                    <ul class=\"Documentation-toc\"></ul>\n                  </div>\n                  <p>Package check is a rich testing extension for Go's testing package.</p>\n                  <p>For details about the project, see:</p>\n                  <pre><a href=\"http://labix.org/gocheck\">http://labix.org/gocheck</a>\n</pre>\n                </section>\n                <h3 tabindex=\"-1\" id=\"pkg-constants\" class=\"Documentation-constantsHeader\">Constants <a href=\"#pkg-constants\">¶</a></h3>\n              </div>\n            </div>\n          </div>\n        </div>\n      </div>\n    </div>\n  </div>\n</body>\n</html>\n")
        (pkg.go.dev-licence "<!DOCTYPE html>\n<html lang=\"en\">\n<meta charset=\"utf-8\">\n<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">\n<body class=\"Site Site--wide Site--redesign\">\n  <div class=\"Unit-content\" role=\"main\">\n    <section class=\"License\" id=\"lic-0\">\n      <h2><div id=\"#lic-0\">BSD-2-Clause</div></h2>\n      <p>This is not legal advice. <a href=\"/license-policy\">Read disclaimer.</a></p>\n      <pre class=\"License-contents\">Gocheck - A rich testing framework for Go\n \nCopyright line\n\nAll rights reserved.\n\nRedistribution and use in source and binary forms, with or without\nmodification, are permitted provided that the following conditions are met: \n\n1. Redistributions of source code must retain the above copyright notice, this\n   list of conditions and the following disclaimer. \n2. Redistributions in binary form must reproduce the above copyright notice,\n   this list of conditions and the following disclaimer in the documentation\n   and/or other materials provided with the distribution. \n\nTHIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS &#34;AS IS&#34; AND\nANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\nWARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\nDISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR\nANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\nLOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\nON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\nSOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n</pre>\n    </section>\n    <div class=\"License-source\">Source: github.com/go-check/check@v0.0.0-20201128035030-22ab2dfb190c/LICENSE</div>\n  </div>\n  </div>\n"))
    `(("https://proxy.golang.org/github.com/go-check/check/@v/v0.0.0-20201130134442-10cb98267c6c.mod"
       . ,go.mod)
      ("https://proxy.golang.org/github.com/go-check/check/@latest"
       . ,version)
      ("https://github.com/go-check/check?go-get=1"
       . ,go-get)
      ("https://pkg.go.dev/github.com/go-check/check"
       . ,pkg.go.dev)
      ("https://pkg.go.dev/github.com/go-check/check?tab=licenses"
       . ,pkg.go.dev-licence))))

(test-begin "go")

;;; Unit tests for go build-system

(test-equal "go-version basic"
  "v1.0.2"
  (go-version->git-ref "v1.0.2"))

(test-equal "go-version omited 'v' character"
  "v1.0.2"
  (go-version->git-ref "v1.0.2"))

(test-equal "go-version with embeded git-ref"
  "65e3620a7ae7"
  (go-version->git-ref "v0.0.0-20190821162956-65e3620a7ae7"))

(test-equal "go-version with complex embeded git-ref"
  "daa7c04131f5"
  (go-version->git-ref "v1.2.4-0.20191109021931-daa7c04131f5"))

;;; Unit tests for (guix import go)

(test-equal "go-path-escape"
  "github.com/!azure/!avere"
  ((@@ (guix import go) go-path-escape) "github.com/Azure/Avere"))


;; We define a function for all similar tests with different go.mod files
(define (testing-parse-mod name expected input)
  (define (inf? p1 p2)
    (string<? (car p1) (car p2)))
  (let ((input-port (open-input-string input)))
    (test-equal name
      (sort expected inf?)
      (sort
       ( (@@ (guix import go) parse-go.mod)
         input-port)
       inf?))))

(testing-parse-mod "parse-go.mod-simple"
                   '(("good/thing" . "v1.4.5")
                     ("new/thing/v2" . "v2.3.4")
                     ("other/thing" . "v1.0.2"))
                   fixture-go-mod-simple)

(testing-parse-mod "parse-go.mod-with-block"
                   '(("A" . "v1")
                     ("B" . "v1.0.0")
                     ("C" . "v1.0.0")
                     ("D" . "v1.2.3")
                     ("E" . "dev"))
                   fixture-go-mod-with-block)

(testing-parse-mod "parse-go.mod-complete"
                   '(("github.com/corp/arbitrary-repo" . "v0.0.2")
                     ("quoted.example.com/abitrary/repo" . "v0.0.2")
                     ("one.example.com/abitrary/repo" . "v1.1.111")
                     ("hub.jazz.net/git/user/project/sub/directory" . "v1.1.19")
                     ("hub.jazz.net/git/user/project" . "v1.1.18")
                     ("launchpad.net/~user/project/branch/sub/directory" . "v1.1.17")
                     ("launchpad.net/~user/project/branch" . "v1.1.16")
                     ("launchpad.net/project/series/sub/directory" . "v1.1.15")
                     ("launchpad.net/project/series" . "v1.1.14")
                     ("launchpad.net/project" . "v1.1.13")
                     ("bitbucket.org/user/project/sub/directory" . "v1.11.21")
                     ("bitbucket.org/user/project" . "v1.11.20")
                     ("k8s.io/kubernetes/subproject" . "v1.1.101")
                     ("github.com/user/project/sub/directory" . "v1.1.12")
                     ("github.com/user/project" . "v1.1.11")
                     ("github.com/go-check/check" . "v0.0.0-20140225173054-eb6ee6f84d0a"))
                   fixture-go-mod-complete)

;;; End-to-end tests for (guix import go)
(define (mock-http-fetch testcase)
  (lambda (url . rest)
    (let ((body (assoc-ref testcase url)))
      (if body
          (open-input-string body)
          (error "mocked http-fetch Unexpected URL: " url)))))

(define (mock-http-get testcase)
  (lambda (url . rest)
    (let ((body (assoc-ref testcase url))
          (response-header
             (build-response
                #:version '(1 . 1)
                #:code 200
                #:reason-phrase "Ok"
                #:headers `(
                            (content-type text/html (charset . "utf-8"))
                            (date . ,(make-date 0 10 58 12 6 3 2021 0))
                            (transfer-encoding (chunked)))
                #:port #f
                #:validate-headers? #t)))
      (if body
          (values response-header body)
          (error "mocked http-get Unexpected URL: " url)))))

(test-equal "go-module->guix-package"
  '(package
    (name "go-github-com-go-check-check")
    (version "0.0.0-20201130134442-10cb98267c6c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-check/check.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5"))))
    (build-system go-build-system)
    (arguments
     (quote (#:import-path "github.com/go-check/check")))
    (inputs
     (quasiquote (("go-github-com-kr-pretty"
                   (unquote go-github-com-kr-pretty)))))
    (home-page "https://github.com/go-check/check")
    (synopsis "Instructions")
    (description #f)
    (license license:bsd-2))

  ;; Replace network resources with sample data.
  (call-with-temporary-directory
   (lambda (checkout)
     (mock ((web client) http-get
            (mock-http-get fixtures-go-check-test))
           (mock ((guix http-client) http-fetch
                  (mock-http-fetch fixtures-go-check-test))
                 (mock ((guix git) update-cached-checkout
                        (lambda* (url #:key ref)
                          ;; Return an empty directory and its hash.
                          (values checkout
                                  (nix-base32-string->bytevector
                                   "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5")
                                  #f)))
                       (go-module->guix-package "github.com/go-check/check")))))))

(test-end "go")

