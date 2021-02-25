;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix describe)
  #:use-module (guix memoization)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module ((guix utils) #:select (location-file))
  #:use-module ((guix store) #:select (%store-prefix store-path?))
  #:use-module ((guix config) #:select (%state-directory))
  #:autoload   (guix channels) (channel-name
                                sexp->channel
                                manifest-entry-channel)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (current-profile
            current-profile-date
            current-profile-entries
            current-channels
            package-path-entries

            package-provenance
            package-channels
            manifest-entry-with-provenance
            manifest-entry-provenance))

;;; Commentary:
;;;
;;; This module provides supporting code to allow a Guix instance to find, at
;;; run time, which profile it's in (profiles created by 'guix pull').  That
;;; allows it to read meta-information about itself (e.g., repository URL and
;;; commit ID) and to find other channels available in the same profile.  It's
;;; a bit like ELPA's pkg-info.el.
;;;
;;; Code:

(define initial-program-arguments
  ;; Save the initial program arguments.  This allows us to see the "real"
  ;; 'guix' program, even if 'guix repl -s' calls 'set-program-arguments'
  ;; later on.
  (program-arguments))

(define current-profile
  (mlambda ()
    "Return the profile (created by 'guix pull') the calling process lives in,
or #f if this is not applicable."
    (match initial-program-arguments
      ((program . _)
       (and (string-suffix? "/bin/guix" program)
            ;; Note: We want to do _lexical dot-dot resolution_.  Using ".."
            ;; for real would instead take us into the /gnu/store directory
            ;; that ~/.config/guix/current/bin points to, whereas we want to
            ;; obtain ~/.config/guix/current.
            (let ((candidate (dirname (dirname program))))
              (and (file-exists? (string-append candidate "/manifest"))
                   candidate)))))))

(define (current-profile-date)
  "Return the creation date of the current profile (produced by 'guix pull'),
as a number of seconds since the Epoch, or #f if it could not be determined."
  ;; Normally 'current-profile' will return ~/.config/guix/current.  We need
  ;; to 'readlink' once to get '/var/guix/…/guix-profile', whose mtime is the
  ;; piece of information we're looking for.
  (let loop ((profile (current-profile)))
    (match profile
      (#f #f)
      ((? store-path?) #f)
      (file
       (if (string-prefix? %state-directory file)
           (and=> (lstat file) stat:mtime)
           (catch 'system-error
             (lambda ()
               (let ((target (readlink file)))
                 (loop (if (string-prefix? "/" target)
                           target
                           (string-append (dirname file) "/" target)))))
             (const #f)))))))

(define (channel-metadata)
  "Return the 'guix' channel metadata sexp from (guix config) if available;
otherwise return #f."
  ;; Older 'build-self.scm' would create a (guix config) file without the
  ;; '%channel-metadata' variable.  Thus, properly deal with a lack of
  ;; information.
  (let ((module (resolve-interface '(guix config))))
    (and=> (module-variable module '%channel-metadata) variable-ref)))

(define current-profile-entries
  (mlambda ()
    "Return the list of entries in the 'guix pull' profile the calling process
lives in, or the empty list if this is not applicable."
    (match (current-profile)
      (#f '())
      (profile
       (let ((manifest (profile-manifest profile)))
         (manifest-entries manifest))))))

(define current-channel-entries
  (mlambda ()
    "Return manifest entries corresponding to extra channels--i.e., not the
'guix' channel."
    (remove (lambda (entry)
              (string=? (manifest-entry-name entry) "guix"))
            (current-profile-entries))))

(define current-channels
  (mlambda ()
    "Return the list of channels currently available, including the 'guix'
channel.  Return the empty list if this information is missing."
    (match (current-profile-entries)
      (()
       ;; As a fallback, if we're not running from a profile, use 'guix'
       ;; channel metadata from (guix config).
       (match (channel-metadata)
         (#f '())
         (sexp (or (and=> (sexp->channel sexp 'guix) list) '()))))
      (entries
       (filter-map manifest-entry-channel entries)))))

(define (package-path-entries)
  "Return two values: the list of package path entries to be added to the
package search path, and the list to be added to %LOAD-COMPILED-PATH.  These
entries are taken from the 'guix pull' profile the calling process lives in,
when applicable."
  ;; Filter out Guix itself.
  (unzip2 (map (lambda (entry)
                 (list (string-append (manifest-entry-item entry)
                                      "/share/guile/site/"
                                      (effective-version))
                       (string-append (manifest-entry-item entry)
                                      "/lib/guile/" (effective-version)
                                      "/site-ccache")))
               (current-channel-entries))))

(define (package-channels package)
  "Return the list of channels providing PACKAGE or an empty list if it could
not be determined."
  (match (and=> (package-location package) location-file)
    (#f '())
    (file
     (let ((file (if (string-prefix? "/" file)
                     file
                     (search-path %load-path file))))
       (if (and file
                (string-prefix? (%store-prefix) file))
           (filter-map
            (lambda (entry)
              (let ((item (manifest-entry-item entry)))
                (and (or (string-prefix? item file)
                         (string=? "guix" (manifest-entry-name entry)))
                     (manifest-entry-channel entry))))
            (current-profile-entries))
           '())))))

(define (package-provenance package)
  "Return the provenance of PACKAGE as an sexp for use as the 'provenance'
property of manifest entries, or #f if it could not be determined."
  (define (entry-source entry)
    (match (assq 'source
                 (manifest-entry-properties entry))
      (('source value) value)
      (_ #f)))

  (let* ((channels (package-channels package))
         (names (map (compose symbol->string channel-name) channels)))
    ;; Always store information about the 'guix' channel and
    ;; optionally about the specific channel FILE comes from.
    (or (let ((main  (and=> (find (lambda (entry)
                                    (string=? "guix"
                                              (manifest-entry-name entry)))
                                  (current-profile-entries))
                            entry-source))
              (extra (any (lambda (entry)
                            (let ((item (manifest-entry-item entry))
                                  (name (manifest-entry-name entry)))
                              (and (member name names)
                                   (not (string=? name "guix"))
                                   (entry-source entry))))
                          (current-profile-entries))))
          (and main
               `(,main
                 ,@(if extra (list extra) '())))))))

(define (manifest-entry-with-provenance entry)
  "Return ENTRY with an additional 'provenance' property if it's not already
there."
  (let ((properties (manifest-entry-properties entry)))
    (if (assq 'provenance properties)
        entry
        (let ((item (manifest-entry-item entry)))
          (manifest-entry
            (inherit entry)
            (properties
             (match (and (package? item) (package-provenance item))
               (#f   properties)
               (sexp `((provenance ,@sexp)
                       ,@properties)))))))))

(define (manifest-entry-provenance entry)
  "Return the list of channels ENTRY comes from.  Return the empty list if
that information is missing."
  (match (assq-ref (manifest-entry-properties entry) 'provenance)
    ((main extras ...)
     ;; XXX: Until recently, channel sexps lacked the channel name.  For
     ;; entries created by 'manifest-entry-with-provenance', the first sexp
     ;; is known to be the 'guix channel, and for the other ones, invent a
     ;; fallback name (it's OK as the name is just a "pet name").
     (match (sexp->channel main 'guix)
       (#f '())
       (channel
        (let loop ((extras   extras)
                   (counter  1)
                   (channels (list channel)))
          (match extras
            (()
             (reverse channels))
            ((head . tail)
             (let* ((name  (string->symbol
                            (format #f "channel~a" counter)))
                    (extra (sexp->channel head name)))
               (if extra
                   (loop tail (+ 1 counter) (cons extra channels))
                   (loop tail counter channels)))))))))
    (_
     '())))
