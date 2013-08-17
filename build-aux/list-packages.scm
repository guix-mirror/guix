#!/bin/sh
exec guile -l "$0"                              \
  -c '(apply (@ (list-packages) list-packages)
             (cdr (command-line)))'
!#
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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
             `(div (a (@ (href ,uri)
                         (title "Link to the full license"))
                      ,(license-name license))))
            (else
             `(div ,(license-name license) " ("
                   ,(license-comment license) ")")))))
       (#f "")))

    (->sxml (package-license package)))

  (define (status package)
    (define (url system)
      `(a (@ (href ,(string-append "http://hydra.gnu.org/job/gnu/master/"
                                   (package-full-name package) "."
                                   system))
             (title "View the status of this architecture's build at Hydra"))
          ,system))

    `(div "status: "
          ,(url "x86_64-linux") " "
          ,(url "i686-linux")))

  (define (package-logo name)
    (and=> (lookup-gnu-package name)
           gnu-package-logo))

  (let ((description-id (symbol->string
                         (gensym (package-name package)))))
   `(tr (td ,(if (gnu-package? package)
                 `(img (@ (src "/graphics/gnu-head-mini.png")
                          (alt "Part of GNU")
                          (title "Part of GNU")))
                 ""))
        (td (a (@ (href ,(source-url package))
                  (title "Link to the Guix package source code"))
               ,(package-name package) " "
               ,(package-version package)))
        (td (a (@ (href "javascript:void(0)")
                  (title "show/hide package description")
                  (onClick ,(format #f "javascript:show_hide('~a')"
                                    description-id)))
               ,(package-synopsis package))
            (div (@ (id ,description-id)
                    (style "display: none;"))
                 ,(match (package-logo (package-name package))
                    ((? string? url)
                     `(img (@ (src ,url)
                              (height "35em")
                              (class "package-logo")
                              (alt ("Logo of " ,(package-name package))))))
                    (_ #f))
                 (p ,(package-description package))
                 ,(license package)
                 (a (@ (href ,(package-home-page package))
                       (title "Link to the package's website"))
                    ,(package-home-page package))
                 ,(status package))))))

(define (packages->sxml packages)
  "Return an HTML page as SXML describing PACKAGES."
  `(div
    (h2 "GNU Guix Package List")
    (div (@ (id "intro"))
         (div
          (img (@ (src "graphics/guix-logo.small.png")
                  (alt "GNU Guix and the GNU System")
                  (height "83em"))))
         (p "This web page lists the packages currently provided by the "
            (a (@ (href "manual/guix.html#GNU-Distribution"))
               "GNU system distribution")
            " of "
            (a (@ (href "/software/guix/guix.html")) "GNU Guix") ".  "
            "Our " (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
                      "continuous integration system")
            " shows their current build status."))
    (table (@ (id "packages"))
           (tr (th "GNU?")
               (th "Package version")
               (th "Package details"))
           ,@(map package->sxml packages))
    (a (@ (href "#intro")
          (title "Back to top.")
          (id "top"))
       "^")))


(define (insert-css)
  "Return the CSS for the list-packages page."
  (format #t
"<style>
a {transition: all 0.3s}
div#intro {margin-bottom: 5em}
div#intro div, div#intro p {padding:0.5em}
div#intro div {float:left}
table#packages, table#packages tr, table#packages tbody, table#packages td,
table#packages th {border: 0px solid black}
div.package-description {position: relative}
table#packages tr:nth-child(even) {background-color: #FFF}
table#packages tr:nth-child(odd) {background-color: #EEE}
table#packages tr:hover, table#packages tr:focus, table#packages tr:active {background-color: #DDD}
table#packages tr:first-child, table#packages tr:first-child:hover, table#packages tr:first-child:focus, table#packages tr:first-child:active {
background-color: #333;
color: #fff;
}
table#packages td
{
margin:0px;
padding:0.2em 0.5em;
}
table#packages td:first-child {
width:10%;
text-align:center;
}
table#packages td:nth-child(2){width:30%;}
table#packages td:last-child {width:60%}
img.package-logo {
float: left;
padding-right: 1em;
}
table#packages span a {float: right}
a#top {
position:fixed;
right:2%;
bottom:2%;
font-size:150%;
background-color:#EEE;
padding:1.125% 0.75% 0% 0.75%;
text-decoration:none;
color:#000;
border-radius:5px;
}
a#top:hover, a#top:focus {
background-color:#333;
color:#fff;
}
</style>"))

(define (insert-js)
  "Return the JavaScript for the list-packages page."
  (format #t
"<script type=\"text/javascript\">
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
</script>"))


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
")
   (insert-css)
   (insert-js)
   (format #t "<!--#include virtual=\"/server/banner.html\" -->")

   (sxml->xml (packages->sxml packages))
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
