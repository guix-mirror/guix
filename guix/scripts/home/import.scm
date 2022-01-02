;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Arjan Adriaanse <arjan@adriaan.se>
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

(define-module (guix scripts home import)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:autoload   (guix scripts package) (manifest-entry-version-prefix)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (import-manifest

            ;; For tests.
            manifest+configuration-files->code))

;;; Commentary:
;;;
;;; This module provides utilities for generating home service
;;; configurations from existing "dotfiles".
;;;
;;; Code:

(define (basename+remove-dots file-name)
  "Remove the dot from the dotfile FILE-NAME; replace the other dots in
FILE-NAME with \"-\", and return the basename of it."
  (string-map (match-lambda
                (#\. #\-)
                (c c))
              (let ((base (basename file-name)))
                (if (string-prefix? "." base)
                    (string-drop base 1)
                    base))))

(define (generate-bash-configuration+modules destination-directory)
  (define (destination-append path)
    (string-append destination-directory "/" path))

  (define (bash-alias->pair line)
    (if (string-prefix? "alias" line)
        (let ((matched (string-match "alias (.+)=\"?'?([^\"']+)\"?'?" line)))
          `(,(match:substring matched 1) . ,(match:substring matched 2)))
        '()))
  
  (define (parse-aliases input)
    (let loop ((line (read-line input))
               (result '()))
      (if (eof-object? line)
          (reverse result)
          (loop (read-line input)
                (cons (bash-alias->pair line) result)))))

  (let ((rc (destination-append ".bashrc"))
        (profile (destination-append ".bash_profile"))
        (logout (destination-append ".bash_logout")))
    `((service home-bash-service-type
               (home-bash-configuration
                ,@(if (file-exists? rc)
                      `((aliases
                         ',(let* ((port (open-pipe* OPEN_READ "bash" "-i" "-c" "alias"))
                               (alist (parse-aliases port)))
                           (close-port port)
                           (filter (negate null?) alist))))
                      '())
                ,@(if (file-exists? rc)
                      `((bashrc
                         (list (local-file ,rc
                                           ,(basename+remove-dots rc)))))
                      '())
                ,@(if (file-exists? profile)
                      `((bash-profile
                         (list (local-file ,profile
                                           ,(basename+remove-dots profile)))))
                      '())
                ,@(if (file-exists? logout)
                      `((bash-logout
                         (list (local-file ,logout
                                           ,(basename+remove-dots logout)))))
                      '())))
      (guix gexp)
      (gnu home services shells))))

(define %files+configurations-alist
  `((".bashrc" . ,generate-bash-configuration+modules)
    (".bash_profile" . ,generate-bash-configuration+modules)
    (".bash_logout" . ,generate-bash-configuration+modules)))

(define (configurations+modules configuration-directory)
  "Return a list of procedures which when called, generate code for a home
service declaration.  Copy configuration files to CONFIGURATION-DIRECTORY; the
generated service declarations will refer to those files that have been saved
in CONFIGURATION-DIRECTORY."
  (define configurations
    (delete-duplicates
     (filter-map (match-lambda
                   ((file . proc)
                    (let ((absolute-path (string-append (getenv "HOME")
                                                        "/" file)))
                      (and (file-exists? absolute-path)
                           (begin
                             (copy-file absolute-path
                                        (string-append
                                         configuration-directory "/" file))
                             proc)))))
                 %files+configurations-alist)
     eq?))

  (map (lambda (proc) (proc configuration-directory)) configurations))

(define (manifest+configuration-files->code manifest
                                            configuration-directory)
  "Read MANIFEST and the user's configuration files listed in
%FILES+CONFIGURATIONS-ALIST, and return a 'home-environment' sexp.  Copy the
user's files to CONFIGURATION-DIRECTORY; the generated sexp refers to them."
  (match (manifest->code manifest
                         #:entry-package-version
                         manifest-entry-version-prefix)
    (('begin ('use-modules profile-modules ...)
             definitions ... ('packages->manifest packages))
     (match (configurations+modules configuration-directory)
       (((services . modules) ...)
        `(begin
           (use-modules (gnu home)
                        (gnu packages)
                        (gnu services)
                        ,@(delete-duplicates
                           (append profile-modules (concatenate modules))))

           ,@definitions

           (home-environment
            (packages ,packages)
            (services (list ,@services)))))))
    (('begin ('specifications->manifest packages))
     (match (configurations+modules configuration-directory)
       (((services . modules) ...)
        `(begin
           (use-modules (gnu home)
                        (gnu packages)
                        (gnu services)
                        ,@(delete-duplicates (concatenate modules)))

           (home-environment
            (packages (map (compose list specification->package+output)
                           ,packages))
            (services (list ,@services)))))))))

(define* (import-manifest
          manifest destination-directory
          #:optional (port (current-output-port)))
  "Write to PORT a <home-environment> corresponding to MANIFEST."
  (match (manifest+configuration-files->code manifest
                                             destination-directory)
    (('begin exp ...)
     (format port (G_ "\
;; This \"home-environment\" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is \"symbolic\": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by \"guix describe\".
;; See the \"Replicating Guix\" section in the manual.\n"))
     (for-each (lambda (exp)
                 (newline port)
                 (pretty-print exp port))
               exp))))
