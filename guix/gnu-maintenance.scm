;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (guix ftp-client)
  #:use-module (guix utils)
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
            gnu-package-doc-urls
            gnu-package-download-url

            official-gnu-packages
            find-packages
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
  "Return an input port containing the textual data at URI, a string."
  (let*-values (((resp data)
                 (let ((uri (string->uri uri)))
                   ;; Try hard to use the API du jour to get an input port.
                   (if (version>? "2.0.7" (version))
                       (if (defined? 'http-get*)
                           (http-get* uri)
                           (http-get uri))       ; old Guile, returns a string
                       (http-get uri #:streaming? #t)))) ; 2.0.8 or later
               ((code)
                (response-code resp)))
    (case code
      ((200)
       (cond ((not data)
              (begin
                ;; XXX: Guile 2.0.5 and earlier did not support chunked transfer
                ;; encoding, which is required when fetching %PACKAGE-LIST-URL
                ;; (see <http://lists.gnu.org/archive/html/guile-devel/2011-09/msg00089.html>).
                ;; Since users may still be using these versions, warn them and
                ;; bail out.
                (format (current-error-port)
                        "warning: using Guile ~a, ~a ~s encoding~%"
                        (version)
                        "which does not support HTTP"
                        (response-transfer-encoding resp))
                (error "download failed; use a newer Guile"
                       uri resp)))
             ((string? data)                 ; old `http-get' returns a string
              (open-input-string data))
             (else                           ; input port
              data)))
      (else
       (error "download failed" uri code
              (response-reason-phrase resp))))))

(define %package-list-url
  (string-append "http://cvs.savannah.gnu.org/"
                 "viewvc/*checkout*/gnumaint/"
                 "gnupackages.txt?root=womb"))

(define-record-type* <gnu-package-descriptor>
  gnu-package-descriptor
  make-gnu-package-descriptor

  gnu-package-descriptor?

  (name             gnu-package-name)
  (mundane-name     gnu-package-mundane-name)
  (copyright-holder gnu-package-copyright-holder)
  (savannah         gnu-package-savannah)
  (fsd              gnu-package-fsd)
  (language         gnu-package-language)
  (logo             gnu-package-logo)
  (doc-category     gnu-package-doc-category)
  (doc-summary      gnu-package-doc-summary)
  (doc-urls         gnu-package-doc-urls)
  (download-url     gnu-package-download-url))

(define (official-gnu-packages)
  "Return a list of records, which are GNU packages."
  (define (group-package-fields port state)
    ;; Return a list of alists.  Each alist contains fields of a GNU
    ;; package.
    (let ((line        (read-line port))
          (field-rx    (make-regexp "^([[:graph:]]+): (.*)$"))
          (doc-urls-rx (make-regexp "^doc-url: (.*)$"))
          (end-rx      (make-regexp "^# End. .+Do not remove this line.+")))

      (define (match-field str)
        ;; Packages are separated by empty strings.  If STR is an
        ;; empty string, create a new list to store fields of a
        ;; different package.  Otherwise, match and create a key-value
        ;; pair.
        (match str
          (""
           (group-package-fields port (cons '() state)))
          (str
           (cond ((regexp-exec doc-urls-rx str)
                  =>
                  (lambda (match)
                    (if (equal? (assoc-ref (first state) "doc-urls") #f)
                        (group-package-fields
                         port (cons (cons (cons "doc-urls"
                                                (list
                                                 (match:substring match 1)))
                                          (first state))
                                    (drop state 1)))
                        (group-package-fields
                         port (cons (cons (cons "doc-urls"
                                                (cons (match:substring match 1)
                                                      (assoc-ref (first state)
                                                                 "doc-urls")))
                                          (assoc-remove! (first state)
                                                         "doc-urls"))
                                    (drop state 1))))))
                 ((regexp-exec field-rx str)
                  =>
                  (lambda (match)
                    (group-package-fields
                     port (cons (cons (cons (match:substring match 1)
                                            (match:substring match 2))
                                      (first state))
                                (drop state 1)))))
                 (else (group-package-fields port state))))))

      (if (or (eof-object? line)
              (regexp-exec end-rx line)) ; don't include dummy fields
          (remove null-list? state)
          (match-field line))))

  (define (alist->record alist make keys)
    ;; Apply MAKE, which should be a syntactic constructor, to the
    ;; values associated with KEYS in ALIST.
    (let ((args (map (cut assoc-ref alist <>) keys)))
      (apply make args)))

  (reverse
   (map (lambda (alist)
          (alist->record alist
                         make-gnu-package-descriptor
                         (list "package" "mundane-name" "copyright-holder"
                               "savannah" "fsd" "language" "logo"
                               "doc-category" "doc-summary" "doc-urls"
                               "download-url")))
        (group-package-fields (http-fetch %package-list-url)
                              '(())))))

(define (find-packages regexp)
  "Find GNU packages which satisfy REGEXP."
  (let ((name-rx (make-regexp regexp)))
    (filter (lambda (package)
              (false-if-exception
               (regexp-exec name-rx (gnu-package-name package))))
            (official-gnu-packages))))

(define gnu-package?
  (memoize
   (lambda (package)
     "Return true if PACKAGE is a GNU package.  This procedure may access the
network to check in GNU's database."
     ;; TODO: Find a way to determine that a package is non-GNU without going
     ;; through the network.
     (let ((url  (and=> (package-source package) origin-uri))
           (name (package-name package)))
       (or (and (string? url) (string-prefix? "mirror://gnu" url))
           (and (member name (map gnu-package-name (official-gnu-packages)))
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

  (define (release-file file)
    ;; Return #f if FILE is not a release tarball, otherwise return
    ;; PACKAGE-VERSION.
    (and (not (string-suffix? ".sig" file))
         (regexp-exec release-rx file)
         (not (regexp-exec alpha-rx file))
         (let ((s (sans-extension file)))
           (and (regexp-exec %package-name-rx s) s))))

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
                                 (and=> (release-file file)
                                        (cut cons <> directory)))
                                (_ #f))
                              files)
                  result))))))))

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
