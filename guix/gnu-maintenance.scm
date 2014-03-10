;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix gnupg)
  #:use-module (rnrs io ports)
  #:use-module (guix base32)
  #:use-module ((guix build utils)
                #:select (substitute))
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
            gnu-package-name->name+version
            package-update-path
            package-update
            update-package-source))

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

(define (official-gnu-packages)
  "Return a list of records, which are GNU packages."
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
    (let ((db (read-records (http-fetch %package-description-url
                                        #:text? #t))))
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
       (read-records (http-fetch %package-list-url #:text? #t))))

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
       ;; TODO: Find a way to determine that a package is non-GNU without going
       ;; through the network.
       (let ((url  (and=> (package-source package) origin-uri))
             (name (package-name package)))
         (or (and (string? url) (string-prefix? "mirror://gnu" url))
             (and (member name (map gnu-package-name (official-gnu-packages)))
                  #t)))))))


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
      ("TeXmacs"      "ftp.texmacs.org" "/TeXmacs/targz")))

  (match (assoc project quirks)
    ((_ server directory)
     (values server directory))
    (_
     (values "ftp.gnu.org" (string-append "/gnu/" project)))))

(define (sans-extension tarball)
  "Return TARBALL without its .tar.* extension."
  (let ((end (string-contains tarball ".tar")))
    (substring tarball 0 end)))

(define %tarball-rx
  (make-regexp "^(.+)-([0-9]|[^-])*(-src)?\\.tar\\."))

(define %alpha-tarball-rx
  (make-regexp "^.*-.*[0-9](-|~)?(alpha|beta|rc|cvs|svn|git)-?[0-9\\.]*\\.tar\\."))

(define (release-file project file)
  "Return #f if FILE is not a release tarball of PROJECT, otherwise return
PACKAGE-VERSION."
  (and (not (string-suffix? ".sig" file))
       (and=> (regexp-exec %tarball-rx file)
              (lambda (match)
                ;; Filter out unrelated files, like `guile-www-1.1.1'.
                (equal? project (match:substring match 1))))
       (not (regexp-exec %alpha-tarball-rx file))
       (let ((s (sans-extension file)))
         (and (regexp-exec %package-name-rx s) s))))

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
         result)
        ((directory rest ...)
         (let* ((files   (ftp-list conn directory))
                (subdirs (filter-map (match-lambda
                                      ((name 'directory . _) name)
                                      (_ #f))
                                     files)))
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
                                 (and=> (release-file project file)
                                        (cut cons <> directory)))
                                (_ #f))
                              files)
                  result))))))))

(define* (latest-release project
                         #:key (ftp-open ftp-open) (ftp-close ftp-close))
  "Return (\"FOO-X.Y\" . \"/bar/foo\") or #f.  Use FTP-OPEN and FTP-CLOSE to
open (resp. close) FTP connections; this can be useful to reuse connections."
  (define (latest a b)
    (if (version>? a b) a b))

  (define contains-digit?
    (cut string-any char-set:digit <>))

  (define patch-directory-name?
    ;; Return #t for patch directory names such as 'bash-4.2-patches'.
    (cut string-suffix? "patches" <>))

  (let-values (((server directory) (ftp-server/directory project)))
    (define conn (ftp-open server))

    (let loop ((directory directory))
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
                                  entries)))
        (match subdirs
          (()
           ;; No sub-directories, so assume that tarballs are here.
           (let ((files (filter-map (match-lambda
                                     ((file 'file . _)
                                      (release-file project file))
                                     (_ #f))
                                    entries)))
             (ftp-close conn)
             (and=> (reduce latest #f files)
                    (cut cons <> directory))))
          ((subdirs ...)
           ;; Assume that SUBDIRS correspond to versions, and jump into the
           ;; one with the highest version number.
           (let ((target (reduce latest #f subdirs)))
             (if target
                 (loop (string-append directory "/" target))
                 (begin
                   (ftp-close conn)
                   #f)))))))))

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


;;;
;;; Auto-update.
;;;

(define (package-update-path package)
  "Return an update path for PACKAGE, or #f if no update is needed."
  (and (gnu-package? package)
       (match (latest-release (package-name package))
         ((name+version . directory)
          (let-values (((_ new-version)
                        (package-name->name+version name+version)))
            (and (version>? name+version (package-full-name package))
                 `(,new-version . ,directory))))
         (_ #f))))

(define* (download-tarball store project directory version
                           #:key (archive-type "gz")
                                 (key-download 'interactive))
  "Download PROJECT's tarball over FTP and check its OpenPGP signature.  On
success, return the tarball file name.  KEY-DOWNLOAD specifies a download
policy for missing OpenPGP keys; allowed values: 'interactive' (default),
'always', and 'never'."
  (let* ((server  (ftp-server/directory project))
         (base    (string-append project "-" version ".tar." archive-type))
         (url     (string-append "ftp://" server "/" directory "/" base))
         (sig-url (string-append url ".sig"))
         (tarball (download-to-store store url))
         (sig     (download-to-store store sig-url)))
    (let ((ret (gnupg-verify* sig tarball #:key-download key-download)))
      (if ret
          tarball
          (begin
            (warning (_ "signature verification failed for `~a'~%")
                     base)
            (warning (_ "(could be because the public key is not in your keyring)~%"))
            #f)))))

(define* (package-update store package #:key (key-download 'interactive))
  "Return the new version and the file name of the new version tarball for
PACKAGE, or #f and #f when PACKAGE is up-to-date.  KEY-DOWNLOAD specifies a
download policy for missing OpenPGP keys; allowed values: 'always', 'never',
and 'interactive' (default)."
  (match (package-update-path package)
    ((version . directory)
     (let-values (((name)
                   (package-name package))
                  ((archive-type)
                   (let ((source (package-source package)))
                     (or (and (origin? source)
                              (file-extension (origin-uri source)))
                         "gz"))))
       (let ((tarball (download-tarball store name directory version
                                        #:archive-type archive-type
                                        #:key-download key-download)))
         (values version tarball))))
    (_
     (values #f #f))))

(define (update-package-source package version hash)
  "Modify the source file that defines PACKAGE to refer to VERSION,
whose tarball has SHA256 HASH (a bytevector).  Return the new version string
if an update was made, and #f otherwise."
  (define (new-line line matches replacement)
    ;; Iterate over MATCHES and return the modified line based on LINE.
    ;; Replace each match with REPLACEMENT.
    (let loop ((m* matches)                       ; matches
               (o  0)                             ; offset in L
               (r  '()))                          ; result
      (match m*
        (()
         (let ((r (cons (substring line o) r)))
           (string-concatenate-reverse r)))
        ((m . rest)
         (loop rest
               (match:end m)
               (cons* replacement
                      (substring line o (match:start m))
                      r))))))

  (define (update-source file old-version version
                         old-hash hash)
    ;; Update source file FILE, replacing occurrences OLD-VERSION by VERSION
    ;; and occurrences of OLD-HASH by HASH (base32 representation thereof).

    ;; TODO: Currently this is a bit of a sledgehammer: if VERSION occurs in
    ;; different unrelated places, we may modify it more than needed, for
    ;; instance.  We should try to make changes only within the sexp that
    ;; corresponds to the definition of PACKAGE.
    (let ((old-hash (bytevector->nix-base32-string old-hash))
          (hash     (bytevector->nix-base32-string hash)))
      (substitute file
                  `((,(regexp-quote old-version)
                     . ,(cut new-line <> <> version))
                    (,(regexp-quote old-hash)
                     . ,(cut new-line <> <> hash))))
      version))

  (let ((name (package-name package))
        (loc  (package-field-location package 'version)))
    (if loc
        (let ((old-version (package-version package))
              (old-hash    (origin-sha256 (package-source package)))
              (file        (and=> (location-file loc)
                                  (cut search-path %load-path <>))))
          (if file
              (update-source file
                             old-version version
                             old-hash hash)
              (begin
                (warning (_ "~a: could not locate source file")
                         (location-file loc))
                #f)))
        (begin
          (format (current-error-port)
                  (_ "~a: ~a: no `version' field in source; skipping~%")
                  (location->string (package-location package))
                  name)))))

;;; gnu-maintenance.scm ends here
