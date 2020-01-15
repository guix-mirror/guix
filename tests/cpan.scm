;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-cpan)
  #:use-module (guix import cpan)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (guix tests http)
  #:use-module (guix grafts)
  #:use-module (srfi srfi-64)
  #:use-module (web client)
  #:use-module (ice-9 match))

;; Globally disable grafts because they can trigger early builds.
(%graft? #f)

(define test-json
  "{
  \"metadata\" : {
    \"name\" : \"Foo-Bar\",
    \"version\" : \"0.1\"
  }
  \"name\" : \"Foo-Bar-0.1\",
  \"distribution\" : \"Foo-Bar\",
  \"license\" : [
    \"perl_5\"
  ],
  \"dependency\": [
     { \"relationship\": \"requires\",
       \"phase\": \"runtime\",
       \"version\": \"1.05\",
       \"module\": \"Test::Script\"
     }
  ],
  \"abstract\" : \"Fizzle Fuzz\",
  \"download_url\" : \"http://example.com/Foo-Bar-0.1.tar.gz\",
  \"author\" : \"Guix\",
  \"version\" : \"0.1\"
}")

(define test-source
  "foobar")

;; Avoid collisions with other tests.
(%http-server-port 10400)

(test-begin "cpan")

(test-assert "cpan->guix-package"
  ;; Replace network resources with sample data.
  (with-http-server `((200 ,test-json)
                      (200 ,test-source)
                      (200 "{ \"distribution\" : \"Test-Script\" }"))
    (parameterize ((%metacpan-base-url (%local-url))
                   (current-http-proxy (%local-url)))
      (match (cpan->guix-package "Foo::Bar")
        (('package
           ('name "perl-foo-bar")
           ('version "0.1")
           ('source ('origin
                      ('method 'url-fetch)
                      ('uri ('string-append "http://example.com/Foo-Bar-"
                                            'version ".tar.gz"))
                      ('sha256
                       ('base32
                        (? string? hash)))))
           ('build-system 'perl-build-system)
           ('propagated-inputs
            ('quasiquote
             (("perl-test-script" ('unquote 'perl-test-script)))))
           ('home-page "https://metacpan.org/release/Foo-Bar")
           ('synopsis "Fizzle Fuzz")
           ('description 'fill-in-yourself!)
           ('license 'perl-license))
         (string=? (bytevector->nix-base32-string
                    (call-with-input-string test-source port-sha256))
                   hash))
        (x
         (pk 'fail x #f))))))

(test-equal "metacpan-url->mirror-url, http"
  "mirror://cpan/authors/id/T/TE/TEST/Foo-Bar-0.1.tar.gz"
  (metacpan-url->mirror-url
   "http://cpan.metacpan.org/authors/id/T/TE/TEST/Foo-Bar-0.1.tar.gz"))

(test-equal "metacpan-url->mirror-url, https"
  "mirror://cpan/authors/id/T/TE/TEST/Foo-Bar-0.1.tar.gz"
  (metacpan-url->mirror-url
   "https://cpan.metacpan.org/authors/id/T/TE/TEST/Foo-Bar-0.1.tar.gz"))

(test-end "cpan")
