;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs io ports)
  #:re-export (%gc-roots-directory)
  #:export (gc-roots
            user-owned?
            busy-store-items))

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
\"regular\" roots found in %GC-ROOTS-DIRECTORY as well as indirect roots that
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


;;;
;;; Listing "busy" store items: those referenced by currently running
;;; processes.
;;;

(define %proc-directory
  ;; Mount point of Linuxish /proc file system.
  "/proc")

(define (proc-file-roots dir file)
  "Return a one-element list containing the file pointed to by DIR/FILE,
or the empty list."
  (or (and=> (false-if-exception (readlink (string-append dir "/" file)))
             list)
      '()))

(define proc-exe-roots (cut proc-file-roots <> "exe"))
(define proc-cwd-roots (cut proc-file-roots <> "cwd"))

(define (proc-fd-roots dir)
  "Return the list of store files referenced by DIR, which is a
/proc/XYZ directory."
  (let ((dir (string-append dir "/fd")))
    (filter-map (lambda (file)
                  (let ((target (false-if-exception
                                 (readlink (string-append dir "/" file)))))
                    (and target
                         (string-prefix? "/" target)
                         target)))
                (or (scandir dir string->number) '()))))

(define (proc-maps-roots dir)
  "Return the list of store files referenced by DIR, which is a
/proc/XYZ directory."
  (define %file-mapping-line
    (make-regexp "^.*[[:blank:]]+/([^ ]+)$"))

  (call-with-input-file (string-append dir "/maps")
    (lambda (maps)
      (let loop ((line  (read-line maps))
                 (roots '()))
        (cond ((eof-object? line)
               roots)
              ((regexp-exec %file-mapping-line line)
               =>
               (lambda (match)
                 (let ((file (string-append "/"
                                            (match:substring match 1))))
                   (loop (read-line maps)
                         (cons file roots)))))
              (else
               (loop (read-line maps) roots)))))))

(define (proc-environ-roots dir)
  "Return the list of store files referenced by DIR/environ, where DIR is a
/proc/XYZ directory."
  (define split-on-nul
    (cute string-tokenize <>
          (char-set-complement (char-set #\nul))))

  (define (rhs-file-names str)
    (let ((equal (string-index str #\=)))
      (if equal
          (let* ((str (substring str (+ 1 equal)))
                 (rx  (string-append (regexp-quote %store-directory)
                                     "/[0-9a-z]{32}-[a-zA-Z0-9\\._+-]+")))
            (map match:substring (list-matches rx str)))
          '())))

  (define environ
    (string-append dir "/environ"))

  (append-map rhs-file-names
              (split-on-nul
               (call-with-input-file environ
                 get-string-all))))

(define (referenced-files)
  "Return the list of referenced store items."
  (append-map (lambda (pid)
                (let ((proc (string-append %proc-directory "/" pid)))
                  (catch 'system-error
                    (lambda ()
                      (append (proc-exe-roots proc)
                              (proc-cwd-roots proc)
                              (proc-fd-roots proc)
                              (proc-maps-roots proc)
                              (proc-environ-roots proc)))
                    (lambda args
                      (let ((err (system-error-errno args)))
                        (if (or (= ENOENT err)    ;TOCTTOU race
                                (= ESRCH err)     ;ditto
                                (= EACCES err))   ;not running as root
                            '()
                            (apply throw args)))))))
              (scandir %proc-directory string->number
                       (lambda (a b)
                         (< (string->number a) (string->number b))))))

(define canonicalize-store-item
  (let* ((store  (string-append %store-directory "/"))
         (prefix (string-length store)))
    (lambda (file)
      "Return #f if FILE is not a store item; otherwise, return the store file
name without any sub-directory components."
      (and (string-prefix? store file)
           (string-append store
                          (let ((base (string-drop file prefix)))
                            (match (string-index base #\/)
                              (#f    base)
                              (slash (string-take base slash)))))))))

(define (busy-store-items)
  "Return the list of store items used by the currently running processes.

This code should typically run as root; it allows the garbage collector to
determine which store items must not be deleted."
  (delete-duplicates
   (filter-map canonicalize-store-item (referenced-files))))
