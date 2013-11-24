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
  #:use-module (sxml fold)
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

(define (list-join lst item)
  "Join the items in LST by inserting ITEM between each pair of elements."
  (let loop ((lst    lst)
             (result '()))
    (match lst
      (()
       (match (reverse result)
         (()
          '())
         ((_ rest ...)
          rest)))
      ((head tail ...)
       (loop tail
             (cons* head item result))))))

(define (package->sxml package previous description-ids remaining)
  "Return 3 values: the HTML-as-SXML for PACKAGE added to all previously
collected package output in PREVIOUS, a list of DESCRIPTION-IDS and the number
of packages still to be processed in REMAINING.  Also Introduces a call to the
JavaScript prep_pkg_descs function as part of the output of PACKAGE, every
time the length of DESCRIPTION-IDS, increasing, is 15 or when REMAINING,
decreasing, is 1."
  (define (location-url loc)
    (string-append "http://git.savannah.gnu.org/cgit/guix.git/tree/"
                   (location-file loc) "#n"
                   (number->string (location-line loc))))

  (define (source-url package)
    (let ((loc (package-location package)))
      (and loc (location-url loc))))

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

  (define (patches package)
    (define (patch-url patch)
      (string-append
       "http://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/patches/"
       (basename patch)))

    (define (snippet-link snippet)
      (let ((loc (or (package-field-location package 'source)
                     (package-location package))))
        `(a (@ (href ,(location-url loc))
               (title "Link to patch snippet"))
            "snippet")))

    (and (origin? (package-source package))
         (let ((patches (origin-patches (package-source package)))
               (snippet (origin-snippet (package-source package))))
           (and (or (pair? patches) snippet)
                `(div "patches: "
                      ,(let loop ((patches patches)
                                  (number  1)
                                  (links   '()))
                         (match patches
                           (()
                            (let* ((additional (and snippet
                                                    (snippet-link snippet)))
                                   (links      (if additional
                                                   (cons additional links)
                                                   links)))
                              (list-join (reverse links) ", ")))
                           ((patch rest ...)
                            (loop rest
                                  (+ 1 number)
                                  (cons `(a (@ (href ,(patch-url patch))
                                               (title ,(string-append
                                                        "Link to "
                                                        (basename patch))))
                                            ,(number->string number))
                                        links))))))))))

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

  (define (insert-tr description-id js?)
    (define (insert-js-call description-ids)
      "Return an sxml call to prep_pkg_descs, with up to 15 elements of
description-ids as formal parameters."
      `(script (@ (type "text/javascript"))
               ,(format #f "prep_pkg_descs(~a)"
                        (string-append "'"
                                       (string-join description-ids "', '")
                                       "'"))))

    (let ((description-ids (cons description-id description-ids)))
      `(tr (td ,(if (gnu-package? package)
                    `(img (@ (src "/graphics/gnu-head-mini.png")
                             (alt "Part of GNU")
                             (title "Part of GNU")))
                    ""))
           (td (a (@ (href ,(source-url package))
                     (title "Link to the Guix package source code"))
                  ,(package-name package) " "
                  ,(package-version package)))
           (td (span ,(package-synopsis package))
               (div (@ (id ,description-id))
                    ,(match (package-logo (package-name package))
                       ((? string? url)
                        `(img (@ (src ,url)
                                 (height "35")
                                 (class "package-logo")
                                 (alt ("Logo of " ,(package-name package))))))
                       (_ #f))
                    (p ,(package-description package))
                    ,(license package)
                    (a (@ (href ,(package-home-page package))
                          (title "Link to the package's website"))
                       ,(package-home-page package))
                    ,(status package)
                    ,(patches package)
                    ,(if js?
                         (insert-js-call description-ids)
                         ""))))))

  (let ((description-id (symbol->string
                         (gensym (package-name package)))))
    (cond ((= remaining 1)              ; Last package in packages
           (values
            (reverse                              ; Fold has reversed packages
             (cons (insert-tr description-id 'js) ; Prefix final sxml
                   previous))
            '()                            ; No more work to do
            0))                            ; End of the line
          ((= (length description-ids) 15) ; Time for a JS call
           (values
            (cons (insert-tr description-id 'js)
                  previous)    ; Prefix new sxml
            '()                ; Reset description-ids
            (1- remaining)))   ; Reduce remaining
          (else                ; Insert another row, and build description-ids
           (values
            (cons (insert-tr description-id #f)
                  previous)                       ; Prefix new sxml
            (cons description-id description-ids) ; Update description-ids
            (1- remaining))))))                   ; Reduce remaining

(define (packages->sxml packages)
  "Return an HTML page as SXML describing PACKAGES."
  `(div
    (h2 "GNU Guix Package List")
    (div (@ (id "intro"))
         (div
          (img (@ (src "graphics/guix-logo.small.png")
                  (alt "GNU Guix and the GNU System")
                  (height "83"))))
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
           ,@(fold-values package->sxml packages '() '() (length packages)))
    (a (@ (href "#intro")
          (title "Back to top.")
          (id "top"))
       "^")))


(define (insert-css)
  "Return the CSS for the list-packages page."
  (format #t
"<style>
/* license: CC0 */
a {
    transition: all 0.3s;
}
div#intro {
    margin-bottom: 2em;
}
div#intro div, div#intro p {
    padding:0.5em;
}
div#intro div {
    float:left;
}
div#intro img {
    float:left;
    padding:0.75em;
}
table#packages, table#packages tr, table#packages tbody, table#packages td, table#packages th {
    border: 0px solid black;
    clear: both;
}
table#packages tr:nth-child(even) {
    background-color: #FFF;
}
table#packages tr:nth-child(odd) {
    background-color: #EEE;
}
table#packages tr:hover, table#packages tr:focus, table#packages tr:active {
    background-color: #DDD;
}
table#packages tr:first-child, table#packages tr:first-child:hover, table#packages tr:first-child:focus, table#packages tr:first-child:active {
    background-color: #333;
    color: #fff;
}
table#packages td {
    margin:0px;
    padding:0.2em 0.5em;
}
table#packages td:first-child {
    width:10%;
    text-align:center;
}
table#packages td:nth-child(2) {
    width:30%;
}
table#packages td:last-child {
    width:60%;
}
img.package-logo {
    float: left;
    padding: 0.75em;
}
table#packages span {
    font-weight: 700;
}
table#packages span a {
    float: right;
    font-weight: 500;
}
a#top {
    position:fixed;
    right:10px;
    bottom:10px;
    font-size:150%;
    background-color:#EEE;
    padding:10px 7.5px 0 7.5px;
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
  if(document.getElementById && document.createTextNode) {
    var thing = document.getElementById(idThing);
    /* Used to change the link text, depending on whether description is
       collapsed or expanded */
    var thingLink = thing.previousSibling.lastChild.firstChild;
    if (thing) {
      if (thing.style.display == \"none\") {
        thing.style.display = \"\";
        thingLink.data = 'Collapse';
      } else {
        thing.style.display = \"none\";
        thingLink.data = 'Expand';
      }
    }
  }
}
/* Add controllers used for collapse/expansion of package descriptions */
function prep(idThing)
{
  var tdThing = document.getElementById(idThing).parentNode;
  if (tdThing) {
    var aThing = tdThing.firstChild.appendChild(document.createElement('a'));
    aThing.setAttribute('href', 'javascript:void(0)');
    aThing.setAttribute('title', 'show/hide package description');
    aThing.appendChild(document.createTextNode('Expand'));
    aThing.onclick=function(){show_hide(idThing);};
    /* aThing.onkeypress=function(){show_hide(idThing);}; */
  }
}
/* Take n element IDs, prepare them for javascript enhanced
   display and hide the IDs by default. */
function prep_pkg_descs()
{
  if(document.getElementById && document.createTextNode) {
    for(var i=0; i<arguments.length; i++) {
      prep(arguments[i])
      show_hide(arguments[i]);
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
   (format #t "</div>
<!--#include virtual=\"/server/footer.html\" -->
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
