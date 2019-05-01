;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix store roots)
  #:use-module (guix config)
  #:use-module ((guix store) #:select (store-path? %gc-roots-directory))
  #:use-module (guix sets)
  #:use-module (guix build syscalls)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:re-export (%gc-roots-directory)
  #:export (gc-roots
            user-owned?))

;;; Commentary:
;;;
;;; This module provides tools to list and access garbage collector roots ("GC
;;; roots").
;;;
;;; Code:

(define %profile-directory
  ;; Directory where user profiles are stored.
  ;; XXX: This is redundant with the definition in (guix profiles) and not
  ;; entirely needed since in practice /var/guix/gcroots/profiles links to
  ;; it.
  (string-append %state-directory "/profiles"))

(define (gc-roots)
  "Return the list of garbage collector roots (\"GC roots\").  This includes
\"regular\" roots fount in %GC-ROOTS-DIRECTORY as well as indirect roots that
are user-controlled symlinks stored anywhere on the file system."
  (define (regular? file)
    (match file
      (((or "." "..") . _) #f)
      (_ #t)))

  (define (file-type=? type)
    (match-lambda
      ((file . properties)
       (match (assq-ref properties 'type)
         ('unknown
          (let ((stat (lstat file)))
            (eq? type (stat:type stat))))
         (actual-type
          (eq? type actual-type))))))

  (define directory?
    (file-type=? 'directory))

  (define symlink?
    (file-type=? 'symlink))

  (define canonical-root
    (match-lambda
      ((file . properties)
       (let ((target (readlink file)))
         (cond ((store-path? target)
                ;; Regular root: FILE points to the store.
                file)

               ;; Indirect root: FILE points to a user-controlled file outside
               ;; the store.
               ((string-prefix? "/" target)
                target)
               (else
                (string-append (dirname file) "/" target)))))))

  (let loop ((directories (list %gc-roots-directory
                                %profile-directory))
             (roots       '())
             (visited     (set)))
    (match directories
      (()
       roots)
      ((directory . rest)
       (if (set-contains? visited directory)
           (loop rest roots visited)
           (let*-values (((scope)
                          (cut string-append directory "/" <>))
                         ((sub-directories files)
                          (partition directory?
                                     (map (match-lambda
                                            ((file . properties)
                                             (cons (scope file) properties)))
                                          (scandir* directory regular?)))))
             (loop (append rest (map first sub-directories))
                   (append (map canonical-root (filter symlink? files))
                           roots)
                   (set-insert directory visited))))))))

(define* (user-owned? root #:optional (uid (getuid)))
  "Return true if ROOT exists and is owned by UID, false otherwise."
  ;; If ROOT is an indirect root, then perhaps it no longer exists.  Thus,
  ;; catch 'system-error' exceptions.
  (catch 'system-error
    (lambda ()
      (define stat
        (lstat root))

      (= (stat:uid stat) uid))
    (const #f)))
