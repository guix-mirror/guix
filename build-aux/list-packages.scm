#!/bin/sh
exec guile -l "$0"                              \
  -c '(apply (@ (list-packages) list-packages)
             (cdr (command-line)))'
!#
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (list-packages)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gnu-maintenance)
  #:use-module (gnu packages)
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (list-packages))

;;; Commentary:
;;;
;;; Emit an HTML representation of the packages available in GNU Guix.
;;;
;;; Code:

(define lookup-gnu-package
  (let ((gnu (official-gnu-packages)))
    (lambda (name)
      "Return the package description for GNU package NAME, or #f."
      (find (lambda (package)
              (equal? (gnu-package-name package) name))
            gnu))))

(define (package->sxml package)
  "Return HTML-as-SXML representing PACKAGE."
  (define (source-url package)
    (let ((loc (package-location package)))
      (and loc
           (string-append "http://git.savannah.gnu.org/cgit/guix.git/tree/"
                          (location-file loc) "#n"
                          (number->string (location-line loc))))))

  (define (license package)
    (define ->sxml
      (match-lambda
       ((lst ...)
        `(div ,(map ->sxml lst)))
       ((? license? license)
        (let ((uri (license-uri license)))
          (case (and=> (and uri (string->uri uri)) uri-scheme)
            ((http https)
             `(div (a (@ (href ,uri))
                      ,(license-name license))))
            (else
             `(div ,(license-name license) " ("
                   ,(license-comment license) ")")))))
       (#f "")))

    (->sxml (package-license package)))

  (define (package-logo name)
    (and=> (lookup-gnu-package name)
           gnu-package-logo))

  (let ((description-id (symbol->string
                         (gensym (package-name package)))))
   `(tr (td ,(if (gnu-package? package)
                 `(img (@ (src "/graphics/gnu-head-mini.png")))
                 ""))
        (td (a (@ (href ,(source-url package)))
               ,(package-name package) " "
               ,(package-version package)))
        (td (@ (colspan "2") (height "0"))
            (a (@ (href "javascript:void(0)")
                  (title "show/hide package description")
                  (onClick ,(format #f "javascript:show_hide('~a')"
                                    description-id)))
               ,(package-synopsis package))
            (div (@ (id ,description-id)
                    (style "position: relative; display: none;"))
                 ,(match (package-logo (package-name package))
                    ((? string? url)
                     `(img (@ (src ,url)
                              (height "35em")
                              (style "float: left; padding-right: 1em;"))))
                    (_ #f))
                 (p ,(package-description package))
                 ,(license package)
                 (a (@ (href ,(package-home-page package)))
                    ,(package-home-page package)))))))

(define (packages->sxml packages)
  "Return an HTML page as SXML describing PACKAGES."
  `(div
    (h2 "GNU Guix Package List")
    (div (@ (style "margin-bottom: 5em;"))
         (div
          (img (@ (src "graphics/guix-logo.small.png")
                  (alt "GNU Guix and the GNU System")
                  (height "83em"))))
         "This web page lists the packages currently provided by the "
         (a (@ (href "manual/guix.html#GNU-Distribution"))
            "GNU system distribution")
         " of "
         (a (@ (href "/software/guix/guix.html")) "GNU Guix") ".")
    (table (@ (style "border: none;"))
           ,@(map package->sxml packages))))


(define (list-packages . args)
  "Return an HTML page listing all the packages found in the GNU distribution,
with gnu.org server-side include and all that."
  ;; Don't attempt to translate descriptions.
  (setlocale LC_ALL "C")

  ;; Output the page as UTF-8 since that's what the gnu.org server-side
  ;; headers claim.
  (set-port-encoding! (current-output-port) "UTF-8")

  (let ((packages (sort (fold-packages cons '())
                        (lambda (p1 p2)
                          (string<? (package-name p1) (package-name p2))))))
   (format #t "<!--#include virtual=\"/server/html5-header.html\" -->
<!-- Parent-Version: 1.70 $ -->

<title>GNU Guix - GNU Distribution - GNU Project</title>
<!--#include virtual=\"/server/banner.html\" -->

<script language=\"javascript\" type=\"text/javascript\">
// license: CC0
function show_hide(idThing)
{
  var thing = document.getElementById(idThing);
  if (thing) {
    if (thing.style.display == \"none\") {
      thing.style.display = \"\";
    } else {
      thing.style.display = \"none\";
    }
  }
}
</script>")
   (display (sxml->xml (packages->sxml packages)))
   (format #t "<!--#include virtual=\"/server/footer.html\" -->
<div id=\"footer\">

<p>Please send general FSF &amp; GNU inquiries to
<a href=\"mailto:gnu@gnu.org\">&lt;gnu@gnu.org&gt;</a>.
There are also <a href=\"/contact/\">other ways to contact</a>
the FSF.  Broken links and other corrections or suggestions can be sent
to <a href=\"mailto:bug-guix@gnu.org\">&lt;bug-guix@gnu.org&gt;</a>.</p>

<p>Copyright &copy; 2013 Free Software Foundation, Inc.</p>

<p>This page is licensed under a <a rel=\"license\"
href=\"http://creativecommons.org/licenses/by-nd/3.0/us/\">Creative
Commons Attribution-NoDerivs 3.0 United States License</a>.</p>

<p>Updated:
<!-- timestamp start -->
$Date$
<!-- timestamp end -->
</p>
</div>
</div>
</body>
</html>
"))
  )

;;; list-packages.scm ends here
