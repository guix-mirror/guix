;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012, 2013 Nikita Karetnikov <nikita@karetnikov.org>
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
  #:use-module (guix http-client)
  #:use-module (guix ftp-client)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:export (gnu-package-name
            gnu-package-mundane-name
            gnu-package-copyright-holder
            gnu-package-savannah
            gnu-package-fsd
            gnu-package-language
            gnu-package-logo
            gnu-package-doc-category
            gnu-package-doc-summary
            gnu-package-doc-description
            gnu-package-doc-urls
            gnu-package-download-url

            official-gnu-packages
            find-packages
            gnu-package?

            releases
            latest-release
            gnu-release-archive-types
            gnu-package-name->name+version

            %gnu-updater))

;;; Commentary:
;;;
;;; Code for dealing with the maintenance of GNU packages, such as
;;; auto-updates.
;;;
;;; Code:


;;;
;;; List of GNU packages.
;;;

(define %gnumaint-base-url
  "http://cvs.savannah.gnu.org/viewvc/*checkout*/gnumaint/")

(define %package-list-url
  (string->uri
   (string-append %gnumaint-base-url "gnupackages.txt?root=womb")))

(define %package-description-url
  ;; This file contains package descriptions in recutils format.
  ;; See <https://lists.gnu.org/archive/html/guix-devel/2013-10/msg00071.html>.
  (string->uri
   (string-append %gnumaint-base-url "pkgblurbs.txt?root=womb")))

(define-record-type* <gnu-package-descriptor>
  gnu-package-descriptor
  make-gnu-package-descriptor

  gnu-package-descriptor?

  (name             gnu-package-name)
  (mundane-name     gnu-package-mundane-name)
  (copyright-holder gnu-package-copyright-holder)
  (savannah         gnu-package-savannah)
  (fsd              gnu-package-fsd)
  (language         gnu-package-language)         ; list of strings
  (logo             gnu-package-logo)
  (doc-category     gnu-package-doc-category)
  (doc-summary      gnu-package-doc-summary)
  (doc-description  gnu-package-doc-description)  ; taken from 'pkgdescr.txt'
  (doc-urls         gnu-package-doc-urls)         ; list of strings
  (download-url     gnu-package-download-url))

(define* (official-gnu-packages
          #:optional (fetch http-fetch/cached))
  "Return a list of records, which are GNU packages.  Use FETCH,
to fetch the list of GNU packages over HTTP."
  (define (read-records port)
    ;; Return a list of alists.  Each alist contains fields of a GNU
    ;; package.
    (let loop ((alist  (recutils->alist port))
               (result '()))
      (if (null? alist)
          (reverse result)
          (loop (recutils->alist port)
                (cons alist result)))))

  (define official-description
    (let ((db (read-records (fetch %package-description-url #:text? #t))))
      (lambda (name)
        ;; Return the description found upstream for package NAME, or #f.
        (and=> (find (lambda (alist)
                       (equal? name (assoc-ref alist "package")))
                     db)
               (lambda (record)
                 (let ((field (assoc-ref record "blurb")))
                   ;; The upstream description file uses "redirect PACKAGE" as
                   ;; a blurb in cases where the description of the two
                   ;; packages should be considered the same (e.g., GTK+ has
                   ;; "redirect gnome".)  This is usually not acceptable for
                   ;; us because we prefer to have distinct descriptions in
                   ;; such cases.  Thus, ignore the 'blurb' field when that
                   ;; happens.
                   (and field
                        (not (string-prefix? "redirect " field))
                        field)))))))

  (map (lambda (alist)
         (let ((name (assoc-ref alist "package")))
           (alist->record `(("description" . ,(official-description name))
                            ,@alist)
                          make-gnu-package-descriptor
                          (list "package" "mundane-name" "copyright-holder"
                                "savannah" "fsd" "language" "logo"
                                "doc-category" "doc-summary" "description"
                                "doc-url"
                                "download-url")
                          '("doc-url" "language"))))
       (let* ((port (fetch %package-list-url #:text? #t))
              (lst  (read-records port)))
         (close-port port)
         lst)))

(define (find-packages regexp)
  "Find GNU packages which satisfy REGEXP."
  (let ((name-rx (make-regexp regexp)))
    (filter (lambda (package)
              (false-if-exception
               (regexp-exec name-rx (gnu-package-name package))))
            (official-gnu-packages))))

(define gnu-package?
  (memoize
   (let ((official-gnu-packages (memoize official-gnu-packages)))
     (lambda (package)
       "Return true if PACKAGE is a GNU package.  This procedure may access the
network to check in GNU's database."
       (define (mirror-type url)
         (let ((uri (string->uri url)))
           (and (eq? (uri-scheme uri) 'mirror)
                (cond
                 ((member (uri-host uri)
                          '("gnu" "gnupg" "gcc" "gnome"))
                  ;; Definitely GNU.
                  'gnu)
                 ((equal? (uri-host uri) "cran")
                  ;; Possibly GNU: mirror://cran could be either GNU R itself
                  ;; or a non-GNU package.
                  #f)
                 (else
                  ;; Definitely non-GNU.
                  'non-gnu)))))

       (define (gnu-home-page? package)
         (and=> (package-home-page package)
                (lambda (url)
                  (and=> (uri-host (string->uri url))
                         (lambda (host)
                           (member host '("www.gnu.org" "gnu.org")))))))

       (or (gnu-home-page? package)
           (let ((url  (and=> (package-source package) origin-uri))
                 (name (package-name package)))
             (case (and (string? url) (mirror-type url))
               ((gnu) #t)
               ((non-gnu) #f)
               (else
                (and (member name (map gnu-package-name (official-gnu-packages)))
                     #t)))))))))


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
      ("glib"         "ftp.gnome.org" "/pub/gnome/sources/glib")
      ("gnutls"       "ftp.gnutls.org" "/gcrypt/gnutls")

      ;; FIXME: ftp.texmacs.org is currently outdated; texmacs.org refers to
      ;; its own http URL instead.
      ("TeXmacs"      "ftp.texmacs.org" "/TeXmacs/targz")))

  (match (assoc project quirks)
    ((_ server directory)
     (values server directory))
    (_
     (values "ftp.gnu.org" (string-append "/gnu/" project)))))

(define (sans-extension tarball)
  "Return TARBALL without its .tar.* or .zip extension."
  (let ((end (or (string-contains tarball ".tar")
                 (string-contains tarball ".zip"))))
    (substring tarball 0 end)))

(define %tarball-rx
  ;; The .zip extensions is notably used for freefont-ttf.
  ;; The "-src" pattern is for "TeXmacs-1.0.7.9-src.tar.gz".
  ;; The "-gnu[0-9]" pattern is for "icecat-38.4.0-gnu1.tar.bz2".
  (make-regexp "^([^.]+)-([0-9]|[^-])+(-(src|gnu[0-9]))?\\.(tar\\.|zip$)"))

(define %alpha-tarball-rx
  (make-regexp "^.*-.*[0-9](-|~)?(alpha|beta|rc|cvs|svn|git)-?[0-9\\.]*\\.tar\\."))

(define (release-file? project file)
  "Return #f if FILE is not a release tarball of PROJECT, otherwise return
true."
  (and (not (string-suffix? ".sig" file))
       (and=> (regexp-exec %tarball-rx file)
              (lambda (match)
                ;; Filter out unrelated files, like `guile-www-1.1.1'.
                ;; Case-insensitive for things like "TeXmacs" vs. "texmacs".
                (and=> (match:substring match 1)
                       (lambda (name)
                         (string-ci=? name project)))))
       (not (regexp-exec %alpha-tarball-rx file))
       (let ((s (sans-extension file)))
         (regexp-exec %package-name-rx s))))

(define (tarball->version tarball)
  "Return the version TARBALL corresponds to.  TARBALL is a file name like
\"coreutils-8.23.tar.xz\"."
  (let-values (((name version)
                (gnu-package-name->name+version (sans-extension tarball))))
    version))

(define (releases project)
  "Return the list of releases of PROJECT as a list of release name/directory
pairs.  Example: (\"mit-scheme-9.0.1\" . \"/gnu/mit-scheme/stable.pkg/9.0.1\"). "
  ;; TODO: Parse something like fencepost.gnu.org:/gd/gnuorg/packages-ftp.
  (let-values (((server directory) (ftp-server/directory project)))
    (define conn (ftp-open server))

    (let loop ((directories (list directory))
               (result      '()))
      (match directories
        (()
         (ftp-close conn)
         (coalesce-sources result))
        ((directory rest ...)
         (let* ((files   (ftp-list conn directory))
                (subdirs (filter-map (match-lambda
                                       ((name 'directory . _) name)
                                       (_ #f))
                                     files)))
           (define (file->url file)
             (string-append "ftp://" server directory "/" file))

           (define (file->source file)
             (let ((url (file->url file)))
               (upstream-source
                (package project)
                (version (tarball->version file))
                (urls (list url))
                (signature-urls (list (string-append url ".sig"))))))

           (loop (append (map (cut string-append directory "/" <>)
                              subdirs)
                         rest)
                 (append
                  ;; Filter out signatures, deltas, and files which
                  ;; are potentially not releases of PROJECT--e.g.,
                  ;; in /gnu/guile, filter out guile-oops and
                  ;; guile-www; in mit-scheme, filter out binaries.
                  (filter-map (match-lambda
                                ((file 'file . _)
                                 (and (release-file? project file)
                                      (file->source file)))
                                (_ #f))
                              files)
                  result))))))))

(define* (latest-release project
                         #:key (ftp-open ftp-open) (ftp-close ftp-close))
  "Return (\"FOO-X.Y\" . \"/bar/foo\") or #f.  Use FTP-OPEN and FTP-CLOSE to
open (resp. close) FTP connections; this can be useful to reuse connections."
  (define (latest a b)
    (if (version>? a b) a b))

  (define (latest-release a b)
    (if (version>? (upstream-source-version a) (upstream-source-version b))
        a b))

  (define contains-digit?
    (cut string-any char-set:digit <>))

  (define patch-directory-name?
    ;; Return #t for patch directory names such as 'bash-4.2-patches'.
    (cut string-suffix? "patches" <>))

  (let-values (((server directory) (ftp-server/directory project)))
    (define conn (ftp-open server))

    (define (file->url directory file)
      (string-append "ftp://" server directory "/" file))

    (define (file->source directory file)
      (let ((url (file->url directory file)))
        (upstream-source
         (package project)
         (version (tarball->version file))
         (urls (list url))
         (signature-urls (list (string-append url ".sig"))))))

    (let loop ((directory directory)
               (result    #f))
      (let* ((entries (ftp-list conn directory))

             ;; Filter out sub-directories that do not contain digits---e.g.,
             ;; /gnuzilla/lang and /gnupg/patches.
             (subdirs (filter-map (match-lambda
                                    (((? patch-directory-name? dir)
                                      'directory . _)
                                     #f)
                                    (((? contains-digit? dir) 'directory . _)
                                     dir)
                                    (_ #f))
                                  entries))

             ;; Whether or not SUBDIRS is empty, compute the latest releases
             ;; for the current directory.  This is necessary for packages
             ;; such as 'sharutils' that have a sub-directory that contains
             ;; only an older release.
             (releases (filter-map (match-lambda
                                     ((file 'file . _)
                                      (and (release-file? project file)
                                           (file->source directory file)))
                                     (_ #f))
                                   entries)))

        ;; Assume that SUBDIRS correspond to versions, and jump into the
        ;; one with the highest version number.
        (let* ((release  (reduce latest-release #f
                                 (coalesce-sources releases)))
               (result   (if (and result release)
                             (latest-release release result)
                             (or release result)))
               (target   (reduce latest #f subdirs)))
          (if target
              (loop (string-append directory "/" target)
                    result)
              (begin
                (ftp-close conn)
                result)))))))

(define (latest-release* package)
  "Like 'latest-release', but ignore FTP errors that might occur when PACKAGE
is not actually a GNU package, or not hosted on ftp.gnu.org, or not under that
name (this is the case for \"emacs-auctex\", for instance.)"
  (catch 'ftp-error
    (lambda ()
      (latest-release package))
    (lambda (key port . rest)
      (if (ftp-connection? port)
          (ftp-close port)
          (close-port port))
      #f)))

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

(define (non-emacs-gnu-package? package)
  "Return true if PACKAGE is a non-Emacs GNU package.  This excludes AucTeX,
for instance, whose releases are now uploaded to elpa.gnu.org."
  (and (not (string-prefix? "emacs-" (package-name package)))
       (gnu-package? package)))

(define %gnu-updater
  (upstream-updater
   (name 'gnu)
   (description "Updater for GNU packages")
   (pred non-emacs-gnu-package?)
   (latest latest-release*)))

;;; gnu-maintenance.scm ends here
