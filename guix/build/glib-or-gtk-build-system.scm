;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build glib-or-gtk-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            glib-or-gtk-build))

;; Commentary:
;;
;; Builder-side code of the standard glib-or-gtk build procedure.
;;
;; Code:

(define (subdirectory-exists? parent sub-directory)
  (directory-exists? (string-append parent sub-directory)))

(define (directory-included? directory directories-list)
  "Is DIRECTORY included in DIRECTORIES-LIST?"
  (fold (lambda (s p) (or (string-ci=? s directory) p))
        #f directories-list))

(define (gtk-module-directories inputs)
  "Check for the existence of \"libdir/gtk-v.0\" in INPUTS.  Return a list
with all found directories."
  (let* ((version
          (if (string-match "gtk\\+-3"
                            (or (assoc-ref inputs "gtk+")
                                (assoc-ref inputs "source")
                                "gtk+-3")) ; we default to version 3
              "3.0"
              "2.0"))
         (gtk-module
          (lambda (input prev)
            (let* ((in (match input
                         ((_ . dir) dir)
                         (_ "")))
                   (libdir
                    (string-append in "/lib/gtk-" version)))
              (if (and (directory-exists? libdir)
                       (not (directory-included? libdir prev)))
                  (cons libdir prev)
                  prev)))))
    (fold gtk-module '() inputs)))

(define (schemas-directories inputs)
  "Check for the existence of \"datadir/glib-2.0/schemas\" in INPUTS.  Return
a list with all found directories."
  (define (glib-schemas input previous)
    (let* ((in (match input
                 ((_ . dir) dir)
                 (_ "")))
           (datadir (string-append in "/share")))
      (if (and (subdirectory-exists? datadir "/glib-2.0/schemas")
               (not (directory-included? datadir previous)))
          (cons datadir previous)
          previous)))

  (fold glib-schemas '() inputs))

(define* (wrap-all-programs #:key inputs outputs #:allow-other-keys)
  "Implement phase \"glib-or-gtk-wrap\": look for GSettings schemas and
gtk+-v.0 libraries and create wrappers with suitably set environment variables
if found."
  (let* ((out (assoc-ref outputs "out"))
         (bindir (string-append out "/bin"))
         (bin-list (find-files bindir ".*"))
         (schemas (schemas-directories (acons "out" out inputs)))
         (schemas-env-var
          (if (not (null? schemas))
              `("XDG_DATA_DIRS" ":" prefix ,schemas)
              #f))
         (gtk-mod-dirs (gtk-module-directories (acons "out" out inputs)))
         (gtk-mod-env-var
          (if (not (null? gtk-mod-dirs))
              `("GTK_PATH" ":" prefix ,gtk-mod-dirs)
              #f)))
    (cond
     ((and schemas-env-var gtk-mod-env-var)
      (for-each (cut wrap-program <> schemas-env-var gtk-mod-env-var)
                bin-list))
     (schemas-env-var
      (for-each (cut wrap-program <> schemas-env-var)
                bin-list))
     (gtk-mod-env-var
      (for-each (cut wrap-program <> gtk-mod-env-var)
                bin-list)))
    #t))

(define* (compile-glib-schemas #:key inputs outputs #:allow-other-keys)
  "Implement phase \"glib-or-gtk-compile-schemas\": compile \"glib\" schemas
if needed."
  (let* ((out (assoc-ref outputs "out"))
         (schemasdir (string-append out "/share/glib-2.0/schemas")))
    (if (and (directory-exists? schemasdir)
             (not (file-exists?
                   (string-append schemasdir "/gschemas.compiled"))))
        (system* "glib-compile-schemas" schemasdir)
        #t)))

(define %standard-phases
  (alist-cons-after
   'install 'glib-or-gtk-wrap wrap-all-programs
   (alist-cons-after
    'install 'glib-or-gtk-compile-schemas compile-glib-schemas
    gnu:%standard-phases)))

(define* (glib-or-gtk-build #:key inputs (phases %standard-phases)
                            #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; glib-or-gtk-build-system.scm ends here
