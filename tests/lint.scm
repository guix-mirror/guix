;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

;; Avoid interference.
(unsetenv "http_proxy")

(define-module (test-lint)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix lint)
  #:use-module (guix ui)
  #:use-module (guix swh)
  #:use-module ((guix gexp) #:select (local-file))
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module ((guix import hackage) #:select (%hackage-url))
  #:use-module ((guix import stackage) #:select (%stackage-url))
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (web uri)
  #:use-module (web server)
  #:use-module (web server http)
  #:use-module (web response)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

;; Test the linter.

(define %null-sha256
  ;; SHA256 of the empty string.
  (base32
   "0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"))

(define %long-string
  (make-string 2000 #\a))

(define (string-match-or-error pattern str)
  (or (string-match pattern str)
      (error str "did not match" pattern)))

(define single-lint-warning-message
  (match-lambda
    (((and (? lint-warning?) warning))
     (lint-warning-message warning))))

(define (warning-contains? str warnings)
  "Return true if WARNINGS is a singleton with a warning that contains STR."
  (match warnings
    (((? lint-warning? warning))
     (string-contains (lint-warning-message warning) str))))


(test-begin "lint")

(test-equal "description: not a string"
  "invalid description: foobar"
  (single-lint-warning-message
   (check-description-style
    (dummy-package "x" (description 'foobar)))))

(test-equal "description: not empty"
  "description should not be empty"
  (single-lint-warning-message
   (check-description-style
    (dummy-package "x" (description "")))))

(test-equal "description: invalid Texinfo markup"
  "Texinfo markup in description is invalid"
  (single-lint-warning-message
   (check-description-style
    (dummy-package "x" (description "f{oo}b@r")))))

(test-equal "description: does not start with an upper-case letter"
  "description should start with an upper-case letter or digit"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (description "bad description."))))
     (check-description-style pkg))))

(test-equal "description: may start with a digit"
  '()
  (let ((pkg (dummy-package "x"
                            (description "2-component library."))))
    (check-description-style pkg)))

(test-equal "description: may start with lower-case package name"
  '()
  (let ((pkg (dummy-package "x"
                            (description "x is a dummy package."))))
    (check-description-style pkg)))

(test-equal "description: two spaces after end of sentence"
  "sentences in description should be followed by two spaces; possible infraction at 3"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (description "Bad. Quite bad."))))
     (check-description-style pkg))))

(test-equal "description: end-of-sentence detection with abbreviations"
  '()
  (let ((pkg (dummy-package "x"
                            (description
                             "E.g. Foo, i.e. Bar resp. Baz (a.k.a. DVD)."))))
    (check-description-style pkg)))

(test-equal "description: may not contain trademark signs: ™"
  "description should not contain trademark sign '™' at 20"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (description "Does The Right Thing™"))))
     (check-description-style pkg))))

(test-equal "description: may not contain trademark signs: ®"
  "description should not contain trademark sign '®' at 17"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (description "Works with Format®"))))
     (check-description-style pkg))))

(test-equal "description: suggest ornament instead of quotes"
  "use @code or similar ornament instead of quotes"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (description "This is a 'quoted' thing."))))
     (check-description-style pkg))))

(test-equal "synopsis: not a string"
  "invalid synopsis: #f"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis #f))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: not empty"
  "synopsis should not be empty"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis ""))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: valid Texinfo markup"
  "Texinfo markup in synopsis is invalid"
  (single-lint-warning-message
   (check-synopsis-style
    (dummy-package "x" (synopsis "Bad $@ texinfo")))))

(test-equal "synopsis: does not start with an upper-case letter"
  "synopsis should start with an upper-case letter or digit"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis "bad synopsis"))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: may start with a digit"
  '()
  (let ((pkg (dummy-package "x"
                            (synopsis "5-dimensional frobnicator"))))
    (check-synopsis-style pkg)))

(test-equal "synopsis: ends with a period"
  "no period allowed at the end of the synopsis"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis "Bad synopsis."))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: ends with 'etc.'"
  '()
  (let ((pkg (dummy-package "x"
                            (synopsis "Foo, bar, etc."))))
    (check-synopsis-style pkg)))

(test-equal "synopsis: starts with 'A'"
  "no article allowed at the beginning of the synopsis"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis "A bad synopŝis"))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: starts with 'An'"
  "no article allowed at the beginning of the synopsis"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis "An awful synopsis"))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: starts with 'a'"
  '("no article allowed at the beginning of the synopsis"
    "synopsis should start with an upper-case letter or digit")
  (sort
   (map
    lint-warning-message
    (let ((pkg (dummy-package "x"
                              (synopsis "a bad synopsis"))))
      (check-synopsis-style pkg)))
   string<?))

(test-equal "synopsis: starts with 'an'"
  '("no article allowed at the beginning of the synopsis"
    "synopsis should start with an upper-case letter or digit")
  (sort
   (map
    lint-warning-message
    (let ((pkg (dummy-package "x"
                              (synopsis "an awful synopsis"))))
      (check-synopsis-style pkg)))
   string<?))

(test-equal "synopsis: too long"
  "synopsis should be less than 80 characters long"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (synopsis (make-string 80 #\X)))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: start with package name"
  "synopsis should not start with the package name"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (name "Foo")
                             (synopsis "Foo, a nice package"))))
     (check-synopsis-style pkg))))

(test-equal "synopsis: start with package name prefix"
  '()
  (let ((pkg (dummy-package "arb"
                            (synopsis "Arbitrary precision"))))
    (check-synopsis-style pkg)))

(test-equal "synopsis: start with abbreviation"
  '()
  (let ((pkg (dummy-package "uucp"
                            ;; Same problem with "APL interpreter", etc.
                            (synopsis "UUCP implementation")
                            (description "Imagine this is Taylor UUCP."))))
    (check-synopsis-style pkg)))

(test-equal "inputs: pkg-config is probably a native input"
  "'pkg-config' should probably be a native input"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (inputs `(("pkg-config" ,pkg-config))))))
     (check-inputs-should-be-native pkg))))

(test-equal "inputs: glib:bin is probably a native input"
  "'glib:bin' should probably be a native input"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (inputs `(("glib" ,glib "bin"))))))
     (check-inputs-should-be-native pkg))))

(test-equal
    "inputs: python-setuptools should not be an input at all (input)"
  "'python-setuptools' should probably not be an input at all"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (inputs `(("python-setuptools"
                                        ,python-setuptools))))))
     (check-inputs-should-not-be-an-input-at-all pkg))))

(test-equal
    "inputs: python-setuptools should not be an input at all (native-input)"
  "'python-setuptools' should probably not be an input at all"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (native-inputs
                              `(("python-setuptools"
                                 ,python-setuptools))))))
     (check-inputs-should-not-be-an-input-at-all pkg))))

(test-equal
    "inputs: python-setuptools should not be an input at all (propagated-input)"
  "'python-setuptools' should probably not be an input at all"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (propagated-inputs
                              `(("python-setuptools" ,python-setuptools))))))
     (check-inputs-should-not-be-an-input-at-all pkg))))

(test-equal "file patches: different file name -> warning"
  "file names of patches should start with the package name"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (source
                              (dummy-origin
                               (patches (list "/path/to/y.patch")))))))
     (check-patch-file-names pkg))))

(test-equal "file patches: same file name -> no warnings"
  '()
  (let ((pkg (dummy-package "x"
                            (source
                             (dummy-origin
                              (patches (list "/path/to/x.patch")))))))
    (check-patch-file-names pkg)))

(test-equal "<origin> patches: different file name -> warning"
  "file names of patches should start with the package name"
  (single-lint-warning-message
   (let ((pkg (dummy-package "x"
                             (source
                              (dummy-origin
                               (patches
                                (list
                                 (dummy-origin
                                  (file-name "y.patch")))))))))
     (check-patch-file-names pkg))))

(test-equal "<origin> patches: same file name -> no warnings"
  '()
  (let ((pkg (dummy-package "x"
                            (source
                             (dummy-origin
                              (patches
                               (list
                                (dummy-origin
                                 (file-name "x.patch")))))))))
    (check-patch-file-names pkg)))

(test-equal "patches: file name too long"
  (string-append "x-"
                 (make-string 100 #\a)
                 ".patch: file name is too long")
  (single-lint-warning-message
   (let ((pkg (dummy-package
               "x"
               (source
                (dummy-origin
                 (patches (list (string-append "x-"
                                               (make-string 100 #\a)
                                               ".patch"))))))))
     (check-patch-file-names pkg))))

(test-equal "patches: not found"
  "this-patch-does-not-exist!: patch not found\n"
  (single-lint-warning-message
   (let ((pkg (dummy-package
               "x"
               (source
                (dummy-origin
                 (patches
                  (list (search-patch "this-patch-does-not-exist!"))))))))
     (check-patch-file-names pkg))))

(test-assert "patch headers: no warnings"
  (call-with-temporary-directory
   (lambda (directory)
     (call-with-output-file (string-append directory "/t.patch")
       (lambda (port)
         (display "This is a patch.\n\n--- a\n+++ b\n"
                  port)))

     (parameterize ((%patch-path (list directory)))
       (let ((pkg (dummy-package "x"
                    (source (dummy-origin
                             (patches (search-patches "t.patch")))))))
         (null? (check-patch-headers pkg)))))))

(test-equal "patch headers: missing comment"
  "t.patch: patch lacks comment and upstream status"
  (call-with-temporary-directory
   (lambda (directory)
     (call-with-output-file (string-append directory "/t.patch")
       (lambda (port)
         (display "\n--- a\n+++ b\n"
                  port)))

     (parameterize ((%patch-path (list directory)))
       (let ((pkg (dummy-package "x"
                    (source (dummy-origin
                             (patches (search-patches "t.patch")))))))
         (single-lint-warning-message (check-patch-headers pkg)))))))

(test-equal "patch headers: empty"
  "t.patch: empty patch"
  (call-with-temporary-directory
   (lambda (directory)
     (call-with-output-file (string-append directory "/t.patch")
       (const #t))

     (parameterize ((%patch-path '()))
       (let ((pkg (dummy-package "x"
                    (source (dummy-origin
                             (patches
                              (list (local-file
                                     (string-append directory
                                                    "/t.patch")))))))))
         (single-lint-warning-message (check-patch-headers pkg)))))))

(test-equal "patch headers: patch not found"
  "does-not-exist.patch: patch not found\n"
  (parameterize ((%patch-path '()))
    (let ((pkg (dummy-package "x"
                 (source (dummy-origin
                          (patches
                           (search-patches "does-not-exist.patch")))))))
      (single-lint-warning-message (check-patch-headers pkg)))))

(test-equal "derivation: invalid arguments"
  "failed to create x86_64-linux derivation: (wrong-type-arg \"map\" \"Wrong type argument: ~S\" (invalid-module) ())"
  (match (let ((pkg (dummy-package "x"
                                   (arguments
                                    '(#:imported-modules (invalid-module))))))
           (check-derivation pkg))
    (((and (? lint-warning?) first-warning) others ...)
     (lint-warning-message first-warning))))

(test-equal "profile-collisions: no warnings"
  '()
  (check-profile-collisions (dummy-package "x")))

(test-equal "profile-collisions: propagated inputs collide"
  "propagated inputs p0@1 and p0@2 collide"
  (let* ((p0  (dummy-package "p0" (version "1")))
         (p0* (dummy-package "p0" (version "2")))
         (p1  (dummy-package "p1" (propagated-inputs `(("p0" ,p0)))))
         (p2  (dummy-package "p2" (propagated-inputs `(("p1" ,p1)))))
         (p3  (dummy-package "p3" (propagated-inputs `(("p0" ,p0*)))))
         (p4  (dummy-package "p4" (propagated-inputs
                                   `(("p2" ,p2) ("p3", p3))))))
    (single-lint-warning-message
     (check-profile-collisions p4))))

(test-assert "profile-collisions: propagated inputs collide, store items"
  (string-match-or-error
   "propagated inputs /[[:graph:]]+-p0-1 and /[[:graph:]]+-p0-1 collide"
   (let* ((p0  (dummy-package "p0" (version "1")))
          (p0* (dummy-package "p0" (version "1")
                              (inputs `(("x" ,(dummy-package "x"))))))
          (p1  (dummy-package "p1" (propagated-inputs `(("p0" ,p0)))))
          (p2  (dummy-package "p2" (propagated-inputs `(("p1" ,p1)))))
          (p3  (dummy-package "p3" (propagated-inputs `(("p0" ,p0*)))))
          (p4  (dummy-package "p4" (propagated-inputs
                                    `(("p2" ,p2) ("p3", p3))))))
     (single-lint-warning-message
      (check-profile-collisions p4)))))

(test-equal "license: invalid license"
  "invalid license field"
  (single-lint-warning-message
   (check-license (dummy-package "x" (license #f)))))

(test-equal "home-page: wrong home-page"
  "invalid value for home page"
  (let ((pkg (package
               (inherit (dummy-package "x"))
               (home-page #f))))
    (single-lint-warning-message
     (check-home-page pkg))))

(test-equal "home-page: invalid URI"
  "invalid home page URL: \"foobar\""
  (let ((pkg (package
               (inherit (dummy-package "x"))
               (home-page "foobar"))))
    (single-lint-warning-message
     (check-home-page pkg))))

(test-assert "home-page: host not found"
  (let ((pkg (package
               (inherit (dummy-package "x"))
               (home-page "http://does-not-exist"))))
    (warning-contains? "domain not found" (check-home-page pkg))))

(parameterize ((%http-server-port 9999))
  ;; TODO skip this test if some process is currently listening at 9999
  (test-equal "home-page: Connection refused"
    "URI http://localhost:9999/foo/bar unreachable: Connection refused"
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (home-page (%local-url)))))
      (single-lint-warning-message
       (check-home-page pkg)))))

(test-equal "home-page: 200"
  '()
  (with-http-server `((200 ,%long-string))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (home-page (%local-url)))))
      (check-home-page pkg))))

(with-http-server `((200 "This is too small."))
  (test-equal "home-page: 200 but short length"
    (format #f "URI ~a returned suspiciously small file (18 bytes)"
            (%local-url))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (home-page (%local-url)))))

      (single-lint-warning-message
       (check-home-page pkg)))))

(with-http-server `((404 ,%long-string))
  (test-equal "home-page: 404"
    (format #f "URI ~a not reachable: 404 (\"Such is life\")" (%local-url))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (home-page (%local-url)))))
      (single-lint-warning-message
       (check-home-page pkg)))))

(with-http-server `((301 ,%long-string))
  (test-equal "home-page: 301, invalid"
    (format #f "invalid permanent redirect from ~a" (%local-url))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (home-page (%local-url)))))
      (single-lint-warning-message
       (check-home-page pkg)))))

(with-http-server `((200 ,%long-string))
  (let* ((initial-url (%local-url))
         (redirect (build-response #:code 301
                                   #:headers
                                   `((location
                                      . ,(string->uri initial-url))))))
    (parameterize ((%http-server-port 0))
      (with-http-server `((,redirect ""))
        (test-equal "home-page: 301 -> 200"
          (format #f "permanent redirect from ~a to ~a"
                  (%local-url) initial-url)
          (let ((pkg (package
                       (inherit (dummy-package "x"))
                       (home-page (%local-url)))))
            (single-lint-warning-message
             (check-home-page pkg))))))))

(with-http-server `((404 "booh!"))
  (let* ((initial-url (%local-url))
         (redirect    (build-response #:code 301
                                      #:headers
                                      `((location
                                         . ,(string->uri initial-url))))))
    (parameterize ((%http-server-port 0))
      (with-http-server `((,redirect ""))
        (test-equal "home-page: 301 -> 404"
          (format #f "URI ~a not reachable: 404 (\"Such is life\")" (%local-url))
          (let ((pkg (package
                       (inherit (dummy-package "x"))
                       (home-page (%local-url)))))
            (single-lint-warning-message
             (check-home-page pkg))))))))


(test-equal "source-file-name"
  "the source file name should contain the package name"
  (let ((pkg (dummy-package "x"
                            (version "3.2.1")
                            (source
                             (origin
                               (method url-fetch)
                               (uri "http://www.example.com/3.2.1.tar.gz")
                               (sha256 %null-sha256))))))
    (single-lint-warning-message
     (check-source-file-name pkg))))

(test-equal "source-file-name: v prefix"
  "the source file name should contain the package name"
  (let ((pkg (dummy-package "x"
                            (version "3.2.1")
                            (source
                             (origin
                               (method url-fetch)
                               (uri "http://www.example.com/v3.2.1.tar.gz")
                               (sha256 %null-sha256))))))
    (single-lint-warning-message
     (check-source-file-name pkg))))

(test-equal "source-file-name: bad checkout"
  "the source file name should contain the package name"
  (let ((pkg (dummy-package "x"
                            (version "3.2.1")
                            (source
                             (origin
                               (method git-fetch)
                               (uri (git-reference
                                     (url "http://www.example.com/x.git")
                                     (commit "0")))
                               (sha256 %null-sha256))))))
    (single-lint-warning-message
     (check-source-file-name pkg))))

(test-equal "source-file-name: good checkout"
  '()
  (let ((pkg (dummy-package "x"
                            (version "3.2.1")
                            (source
                             (origin
                               (method git-fetch)
                               (uri (git-reference
                                     (url "http://git.example.com/x.git")
                                     (commit "0")))
                               (file-name (string-append "x-" version))
                               (sha256 %null-sha256))))))
    (check-source-file-name pkg)))

(test-equal "source-file-name: valid"
  '()
  (let ((pkg (dummy-package "x"
                            (version "3.2.1")
                            (source
                             (origin
                               (method url-fetch)
                               (uri "http://www.example.com/x-3.2.1.tar.gz")
                               (sha256 %null-sha256))))))
    (check-source-file-name pkg)))

(test-equal "source-unstable-tarball"
  "the source URI should not be an autogenerated tarball"
  (let ((pkg (dummy-package "x"
                            (source
                             (origin
                               (method url-fetch)
                               (uri "https://github.com/example/example/archive/v0.0.tar.gz")
                               (sha256 %null-sha256))))))
    (single-lint-warning-message
     (check-source-unstable-tarball pkg))))

(test-equal "source-unstable-tarball: source #f"
  '()
  (let ((pkg (dummy-package "x"
                            (source #f))))
    (check-source-unstable-tarball pkg)))

(test-equal "source-unstable-tarball: valid"
  '()
  (let ((pkg (dummy-package "x"
                            (source
                             (origin
                               (method url-fetch)
                               (uri "https://github.com/example/example/releases/download/x-0.0/x-0.0.tar.gz")
                               (sha256 %null-sha256))))))
    (check-source-unstable-tarball pkg)))

(test-equal "source-unstable-tarball: package named archive"
  '()
  (let ((pkg (dummy-package "x"
                            (source
                             (origin
                               (method url-fetch)
                               (uri "https://github.com/example/archive/releases/download/x-0.0/x-0.0.tar.gz")
                               (sha256 %null-sha256))))))
    (check-source-unstable-tarball pkg)))

(test-equal "source-unstable-tarball: not-github"
  '()
  (let ((pkg (dummy-package "x"
                            (source
                             (origin
                               (method url-fetch)
                               (uri "https://bitbucket.org/archive/example/download/x-0.0.tar.gz")
                               (sha256 %null-sha256))))))
    (check-source-unstable-tarball pkg)))

(test-equal "source-unstable-tarball: git-fetch"
  '()
  (let ((pkg (dummy-package "x"
                            (source
                             (origin
                               (method git-fetch)
                               (uri (git-reference
                                     (url "https://github.com/archive/example")
                                     (commit "0")))
                               (sha256 %null-sha256))))))
    (check-source-unstable-tarball pkg)))

(test-equal "source: 200"
  '()
  (with-http-server `((200 ,%long-string))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (source (origin
                           (method url-fetch)
                           (uri (%local-url))
                           (sha256 %null-sha256))))))
      (check-source pkg))))

(with-http-server '((200 "This is too small."))
  (test-equal "source: 200 but short length"
    (format #f "URI ~a returned suspiciously small file (18 bytes)"
            (%local-url))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (source (origin
                           (method url-fetch)
                           (uri (%local-url))
                           (sha256 %null-sha256))))))
      (match (check-source pkg)
        ((first-warning ; All source URIs are unreachable
          (and (? lint-warning?) second-warning))
         (lint-warning-message second-warning))))))

(with-http-server `((404 ,%long-string))
  (test-equal "source: 404"
    (format #f "URI ~a not reachable: 404 (\"Such is life\")"
            (%local-url))
    (let ((pkg (package
                 (inherit (dummy-package "x"))
                 (source (origin
                           (method url-fetch)
                           (uri (%local-url))
                           (sha256 %null-sha256))))))
      (match (check-source pkg)
        ((first-warning ; All source URIs are unreachable
          (and (? lint-warning?) second-warning))
         (lint-warning-message second-warning))))))

(test-equal "source: 404 and 200"
  '()
  (with-http-server `((404 ,%long-string))
    (let ((bad-url (%local-url)))
      (parameterize ((%http-server-port (+ 1 (%http-server-port))))
        (with-http-server `((200 ,%long-string))
          (let ((pkg (package
                       (inherit (dummy-package "x"))
                       (source (origin
                                 (method url-fetch)
                                 (uri (list bad-url (%local-url)))
                                 (sha256 %null-sha256))))))
            ;; Since one of the two URLs is good, this should return the empty
            ;; list.
            (check-source pkg)))))))

(with-http-server `((200 ,%long-string))
  (let* ((initial-url (%local-url))
         (redirect    (build-response #:code 301
                                      #:headers
                                      `((location
                                         . ,(string->uri initial-url))))))
    (parameterize ((%http-server-port 0))
      (with-http-server `((,redirect ""))
        (test-equal "source: 301 -> 200"
          (format #f "permanent redirect from ~a to ~a"
                  (%local-url) initial-url)
          (let ((pkg (package
                       (inherit (dummy-package "x"))
                       (source (origin
                                 (method url-fetch)
                                 (uri (%local-url))
                                 (sha256 %null-sha256))))))
            (match (check-source pkg)
              ((first-warning ; All source URIs are unreachable
                (and (? lint-warning?) second-warning))
               (lint-warning-message second-warning)))))))))

(with-http-server `((200 ,%long-string))
  (let* ((initial-url (%local-url))
         (redirect    (build-response #:code 301
                                      #:headers
                                      `((location
                                         . ,(string->uri initial-url))))))
    (parameterize ((%http-server-port 0))
      (with-http-server `((,redirect ""))
        (test-equal "source, git-reference: 301 -> 200"
          (format #f "permanent redirect from ~a to ~a"
                  (%local-url) initial-url)
          (let ((pkg (dummy-package
                      "x"
                      (source (origin
                                (method git-fetch)
                                (uri (git-reference (url (%local-url))
                                                    (commit "v1.0.0")))
                                (sha256 %null-sha256))))))
            (single-lint-warning-message (check-source pkg))))))))

(with-http-server '((404 "booh!"))
  (let* ((initial-url (%local-url))
         (redirect    (build-response #:code 301
                                      #:headers
                                      `((location
                                         . ,(string->uri initial-url))))))
    (parameterize ((%http-server-port 0))
      (with-http-server `((,redirect ""))
        (test-equal "source: 301 -> 404"
          (format #f "URI ~a not reachable: 404 (\"Such is life\")"
                  (%local-url))
          (let ((pkg (package
                       (inherit (dummy-package "x"))
                       (source (origin
                                 (method url-fetch)
                                 (uri (%local-url))
                                 (sha256 %null-sha256))))))
            (match (check-source pkg)
              ((first-warning ; The first warning says that all URI's are
                              ; unreachable
                (and (? lint-warning?) second-warning))
               (lint-warning-message second-warning)))))))))

(test-equal "mirror-url"
  '()
  (let ((source (origin
                  (method url-fetch)
                  (uri "http://example.org/foo/bar.tar.gz")
                  (sha256 %null-sha256))))
    (check-mirror-url (dummy-package "x" (source source)))))

(test-equal "mirror-url: one suggestion"
  "URL should be 'mirror://gnu/foo/foo.tar.gz'"
  (let ((source (origin
                  (method url-fetch)
                  (uri "http://ftp.gnu.org/pub/gnu/foo/foo.tar.gz")
                  (sha256 %null-sha256))))
    (single-lint-warning-message
     (check-mirror-url (dummy-package "x" (source source))))))

(test-equal "github-url"
  '()
  (with-http-server `((200 ,%long-string))
    (check-github-url
     (dummy-package "x" (source
                         (origin
                           (method url-fetch)
                           (uri (%local-url))
                           (sha256 %null-sha256)))))))

(let ((github-url "https://github.com/foo/bar/bar-1.0.tar.gz"))
  (test-equal "github-url: one suggestion"
    (string-append
     "URL should be '" github-url "'")
    (let ((redirect (build-response #:code 301
                                    #:headers
                                    `((location
                                       . ,(string->uri github-url))))))
      (with-http-server `((,redirect ""))
        (let* ((initial-url (%local-url))
               (redirect    (build-response #:code 302
                                            #:headers
                                            `((location
                                               . ,(string->uri initial-url))))))
          (parameterize ((%http-server-port 0))
            (with-http-server `((,redirect ""))
              (single-lint-warning-message
               (check-github-url
                (dummy-package "x" (source
                                    (origin
                                      (method url-fetch)
                                      (uri (%local-url))
                                      (sha256 %null-sha256))))))))))))

  (test-equal "github-url: already the correct github url"
    '()
    (check-github-url
     (dummy-package "x" (source
                         (origin
                           (method url-fetch)
                           (uri github-url)
                           (sha256 %null-sha256)))))))

(test-equal "cve"
  '()
  (mock ((guix lint) package-vulnerabilities (const '()))
        (check-vulnerabilities (dummy-package "x"))))

(test-equal "cve: one vulnerability"
  "probably vulnerable to CVE-2015-1234"
  (let ((dummy-vulnerabilities
         (lambda (package)
           (list (make-struct/no-tail
                  (@@ (guix cve) <vulnerability>)
                  "CVE-2015-1234"
                  (list (cons (package-name package)
                              (package-version package))))))))
    (single-lint-warning-message
     (check-vulnerabilities (dummy-package "pi" (version "3.14"))
                            dummy-vulnerabilities))))

(test-equal "cve: one patched vulnerability"
  '()
  (mock ((guix lint) package-vulnerabilities
         (lambda (package)
           (list (make-struct/no-tail (@@ (guix cve) <vulnerability>)
                                      "CVE-2015-1234"
                                      (list (cons (package-name package)
                                                  (package-version package)))))))
        (check-vulnerabilities
         (dummy-package "pi"
                        (version "3.14")
                        (source
                         (dummy-origin
                          (patches
                           (list "/a/b/pi-CVE-2015-1234.patch"))))))))

(test-equal "cve: known safe from vulnerability"
  '()
  (mock ((guix lint) package-vulnerabilities
         (lambda (package)
           (list (make-struct/no-tail (@@ (guix cve) <vulnerability>)
                                      "CVE-2015-1234"
                                      (list (cons (package-name package)
                                                  (package-version package)))))))
        (check-vulnerabilities
         (dummy-package "pi"
                        (version "3.14")
                        (properties `((lint-hidden-cve . ("CVE-2015-1234"))))))))

(test-equal "cve: vulnerability fixed in replacement version"
  '()
  (mock ((guix lint) package-vulnerabilities
         (lambda (package)
           (match (package-version package)
             ("0"
              (list (make-struct/no-tail (@@ (guix cve) <vulnerability>)
                                         "CVE-2015-1234"
                                         (list (cons (package-name package)
                                                     (package-version package))))))
             ("1"
              '()))))
        (check-vulnerabilities
         (dummy-package
          "foo" (version "0")
          (replacement (dummy-package "foo" (version "1")))))))

(test-equal "cve: patched vulnerability in replacement"
  '()
  (mock ((guix lint) package-vulnerabilities
         (lambda (package)
           (list (make-struct/no-tail (@@ (guix cve) <vulnerability>)
                                      "CVE-2015-1234"
                                      (list (cons (package-name package)
                                                  (package-version package)))))))
        (check-vulnerabilities
         (dummy-package
          "pi" (version "3.14") (source (dummy-origin))
          (replacement (dummy-package
                        "pi" (version "3.14")
                        (source
                         (dummy-origin
                          (patches
                           (list "/a/b/pi-CVE-2015-1234.patch"))))))))))

(test-equal "formatting: lonely parentheses"
  "parentheses feel lonely, move to the previous or next line"
  (single-lint-warning-message
   (check-formatting
    (dummy-package "ugly as hell!"
                   )
    )))

(test-assert "formatting: tabulation"
  (string-match-or-error
   "tabulation on line [0-9]+, column [0-9]+"
   (single-lint-warning-message
    (check-formatting (dummy-package "leave the tab here:	")))))

(test-assert "formatting: trailing white space"
  (string-match-or-error
   "trailing white space .*"
   ;; Leave the trailing white space on the next line!
   (single-lint-warning-message
    (check-formatting (dummy-package "x")))))            

(test-assert "formatting: long line"
  (string-match-or-error
   "line [0-9]+ is way too long \\([0-9]+ characters\\)"
   (single-lint-warning-message (check-formatting
           (dummy-package "x"))                                     ;here is a stupid comment just to make a long line
     )))

(test-equal "formatting: alright"
  '()
  (check-formatting (dummy-package "x")))

(test-assert "archival: missing content"
  (let* ((origin   (origin
                     (method url-fetch)
                     (uri "http://example.org/foo.tgz")
                     (sha256 (make-bytevector 32))))
         (warnings (with-http-server '((404 "Not archived."))
                     (parameterize ((%swh-base-url (%local-url)))
                       (check-archival (dummy-package "x"
                                                      (source origin)))))))
    (warning-contains? "not archived" warnings)))

(test-equal "archival: content available"
  '()
  (let* ((origin   (origin
                     (method url-fetch)
                     (uri "http://example.org/foo.tgz")
                     (sha256 (make-bytevector 32))))
         ;; https://archive.softwareheritage.org/api/1/content/
         (content  "{ \"checksums\": {}, \"data_url\": \"xyz\",
                      \"length\": 42 }"))
    (with-http-server `((200 ,content))
      (parameterize ((%swh-base-url (%local-url)))
        (check-archival (dummy-package "x" (source origin)))))))

(test-assert "archival: missing revision"
  (let* ((origin   (origin
                     (method git-fetch)
                     (uri (git-reference
                           (url "http://example.org/foo.git")
                           (commit "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
                     (sha256 (make-bytevector 32))))
         ;; https://archive.softwareheritage.org/api/1/origin/save/
         (save     "{ \"origin_url\": \"http://example.org/foo.git\",
                      \"save_request_date\": \"2014-11-17T22:09:38+01:00\",
                      \"save_request_status\": \"accepted\",
                      \"save_task_status\": \"scheduled\" }")
         (warnings (with-http-server `((404 "No revision.") ;lookup-revision
                                       (404 "No origin.")   ;lookup-origin
                                       (200 ,save))         ;save-origin
                     (parameterize ((%swh-base-url (%local-url)))
                       (check-archival (dummy-package "x" (source origin)))))))
    (warning-contains? "scheduled" warnings)))

(test-equal "archival: revision available"
  '()
  (let* ((origin   (origin
                     (method git-fetch)
                     (uri (git-reference
                           (url "http://example.org/foo.git")
                           (commit "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
                     (sha256 (make-bytevector 32))))
         ;; https://archive.softwareheritage.org/api/1/revision/
         (revision "{ \"author\": {}, \"parents\": [],
                      \"date\": \"2014-11-17T22:09:38+01:00\" }"))
    (with-http-server `((200 ,revision))
      (parameterize ((%swh-base-url (%local-url)))
        (check-archival (dummy-package "x" (source origin)))))))

(test-assert "archival: rate limit reached"
  ;; We should get a single warning stating that the rate limit was reached,
  ;; and nothing more, in particular no other HTTP requests.
  (let* ((origin   (origin
                     (method url-fetch)
                     (uri "http://example.org/foo.tgz")
                     (sha256 (make-bytevector 32))))
         (too-many (build-response
                    #:code 429
                    #:reason-phrase "Too many requests"
                    #:headers '((x-ratelimit-remaining . "0")
                                (x-ratelimit-reset . "3000000000"))))
         (warnings (with-http-server `((,too-many "Rate limit reached."))
                     (parameterize ((%swh-base-url (%local-url)))
                       (append-map (lambda (name)
                                     (check-archival
                                      (dummy-package name (source origin))))
                                   '("x" "y" "z"))))))
    (string-contains (single-lint-warning-message warnings)
                     "rate limit reached")))

(test-assert "haskell-stackage"
  (let* ((stackage (string-append "{ \"packages\": [{"
                                  "    \"name\":\"x\","
                                  "    \"version\":\"1.0\" }]}"))
         (packages (map (lambda (version)
                          (dummy-package
                           (string-append "ghc-x")
                           (version version)
                           (source
                            (dummy-origin
                             (method url-fetch)
                             (uri (string-append
                                   "https://hackage.haskell.org/package/"
                                   "x-" version "/x-" version ".tar.gz"))))))
                        '("0.9" "1.0" "2.0")))
         (warnings (pk (with-http-server `((200 ,stackage) ; memoized
                                           (200 "name: x\nversion: 1.0\n")
                                           (200 "name: x\nversion: 1.0\n")
                                           (200 "name: x\nversion: 1.0\n"))
                         (parameterize ((%hackage-url (%local-url))
                                        (%stackage-url (%local-url)))
                           (append-map check-haskell-stackage packages))))))
    (match warnings
      (((? lint-warning? warning))
       (and (string=? (package-version (lint-warning-package warning)) "2.0")
            (string-contains (lint-warning-message warning)
                             "ahead of Stackage LTS version"))))))

(test-end "lint")

;; Local Variables:
;; eval: (put 'with-http-server 'scheme-indent-function 1)
;; eval: (put 'with-warnings 'scheme-indent-function 0)
;; End:
