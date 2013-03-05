;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2010, 2011, 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix gnu-maintenance)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (guix ftp-client)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:export (official-gnu-packages
            gnu-package?
            releases
            latest-release
            gnu-package-name->name+version))

;;; Commentary:
;;;
;;; Code for dealing with the maintenance of GNU packages, such as
;;; auto-updates.
;;;
;;; Code:


;;;
;;; List of GNU packages.
;;;

(define (http-fetch uri)
  "Return a string containing the textual data at URI, a string."
  (let*-values (((resp data)
                (http-get (string->uri uri)))
               ((code)
                (response-code resp)))
    (case code
      ((200)
       data)
      (else
       (error "download failed:" uri code
              (response-reason-phrase resp))))))

(define %package-list-url
  (string-append "http://cvs.savannah.gnu.org/"
                 "viewvc/*checkout*/gnumaint/"
                 "gnupackages.txt?root=womb"))

(define (official-gnu-packages)
  "Return a list of GNU packages."
  (define %package-line-rx
    (make-regexp "^package: (.+)$"))

  (let ((lst (string-split (http-fetch %package-list-url) #\nl)))
    (filter-map (lambda (line)
                  (and=> (regexp-exec %package-line-rx line)
                         (cut match:substring <> 1)))
                lst)))

(define gnu-package?
  (memoize
   (lambda (package)
     "Return true if PACKAGE is a GNU package.  This procedure may access the
network to check in GNU's database."
     ;; TODO: Find a way to determine that a package is non-GNU without going
     ;; through the network.
     (let ((url (and=> (package-source package) origin-uri)))
       (or (and (string? url) (string-prefix? "mirror://gnu" url))
           (and (member (package-name package) (official-gnu-packages))
                #t))))))


;;;
;;; Latest release.
;;;

(define (ftp-server/directory project)
  "Return the FTP server and directory where PROJECT's tarball are
stored."
  (define quirks
    '(("commoncpp2"   "ftp.gnu.org"   "/gnu/commoncpp")
      ("ucommon"      "ftp.gnu.org"   "/gnu/commoncpp")
      ("libzrtpcpp"   "ftp.gnu.org"   "/gnu/ccrtp")
      ("libosip2"     "ftp.gnu.org"   "/gnu/osip")
      ("libgcrypt"    "ftp.gnupg.org" "/gcrypt/libgcrypt")
      ("libgpg-error" "ftp.gnupg.org" "/gcrypt/libgpg-error")
      ("libassuan"    "ftp.gnupg.org" "/gcrypt/libassuan")
      ("gnupg"        "ftp.gnupg.org" "/gcrypt/gnupg")
      ("freefont-ttf" "ftp.gnu.org"   "/gnu/freefont")
      ("gnu-ghostscript" "ftp.gnu.org"  "/gnu/ghostscript")
      ("mit-scheme"   "ftp.gnu.org" "/gnu/mit-scheme/stable.pkg")
      ("icecat"       "ftp.gnu.org" "/gnu/gnuzilla")
      ("source-highlight" "ftp.gnu.org" "/gnu/src-highlite")
      ("TeXmacs"      "ftp.texmacs.org" "/TeXmacs/targz")))

  (match (assoc project quirks)
    ((_ server directory)
     (values server directory))
    (_
     (values "ftp.gnu.org" (string-append "/gnu/" project)))))

(define (releases project)
  "Return the list of releases of PROJECT as a list of release name/directory
pairs.  Example: (\"mit-scheme-9.0.1\" . \"/gnu/mit-scheme/stable.pkg/9.0.1\"). "
  ;; TODO: Parse something like fencepost.gnu.org:/gd/gnuorg/packages-ftp.
  (define release-rx
    (make-regexp (string-append "^" project
                                "-([0-9]|[^-])*(-src)?\\.tar\\.")))

  (define alpha-rx
    (make-regexp "^.*-.*[0-9](-|~)?(alpha|beta|rc|cvs|svn|git)-?[0-9\\.]*\\.tar\\."))

  (define (sans-extension tarball)
    (let ((end (string-contains tarball ".tar")))
      (substring tarball 0 end)))

  (let-values (((server directory) (ftp-server/directory project)))
    (define conn (ftp-open server))

    (let loop ((directories (list directory))
               (result      '()))
      (if (null? directories)
          (begin
            (ftp-close conn)
            result)
          (let* ((directory (car directories))
                 (files     (ftp-list conn directory))
                 (subdirs   (filter-map (lambda (file)
                                          (match file
                                            ((name 'directory . _) name)
                                            (_ #f)))
                                        files)))
            (loop (append (map (cut string-append directory "/" <>)
                               subdirs)
                          (cdr directories))
                  (append
                   ;; Filter out signatures, deltas, and files which
                   ;; are potentially not releases of PROJECT--e.g.,
                   ;; in /gnu/guile, filter out guile-oops and
                   ;; guile-www; in mit-scheme, filter out binaries.
                   (filter-map (lambda (file)
                                 (match file
                                   ((file 'file . _)
                                    (and (not (string-suffix? ".sig" file))
                                         (regexp-exec release-rx file)
                                         (not (regexp-exec alpha-rx file))
                                         (let ((s (sans-extension file)))
                                           (and (regexp-exec
                                                 %package-name-rx s)
                                                (cons s directory)))))
                                   (_ #f)))
                               files)
                   result)))))))

(define (latest-release project)
  "Return (\"FOO-X.Y\" . \"/bar/foo\") or #f."
  (let ((releases (releases project)))
    (and (not (null? releases))
         (fold (lambda (release latest)
                 (if (version>? (car release) (car latest))
                     release
                     latest))
               '("" . "")
               releases))))

(define %package-name-rx
  ;; Regexp for a package name, e.g., "foo-X.Y".  Since TeXmacs uses
  ;; "TeXmacs-X.Y-src", the `-src' suffix is allowed.
  (make-regexp "^(.*)-(([0-9]|\\.)+)(-src)?"))

(define (gnu-package-name->name+version name+version)
  "Return the package name and version number extracted from NAME+VERSION."
  (let ((match (regexp-exec %package-name-rx name+version)))
    (if (not match)
        (values name+version #f)
        (values (match:substring match 1) (match:substring match 2)))))

;;; gnu-maintenance.scm ends here
