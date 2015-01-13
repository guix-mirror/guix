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
  #:use-module (ice-9 ftw)
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

;; We do not include $HOME/.guix-profile/gtk-v.0 (v=2 or 3) because we do not
;; want to mix gtk+-2 and gtk+-3 modules.  See
;; https://developer.gnome.org/gtk3/stable/gtk-running.html
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

;; See
;; http://www.freedesktop.org/wiki/DesktopThemeSpec
;; http://freedesktop.org/wiki/Specifications/sound-theme-spec
;; http://freedesktop.org/wiki/Specifications/icon-theme-spec
;; 
;; Currently desktop themes are not well supported and do not honor
;; XDG_DATA_DIRS.  One example is evince which only looks for desktop themes
;; in $HOME/.themes (for backward compatibility) and in XDG_DATA_HOME (which
;; defaults to $HOME/.local/share).  One way to handle these applications
;; appears to be by making $HOME/.themes a symlink to
;; $HOME/.guix-profile/share/themes.
(define (data-directories inputs)
  "Check for the existence of \"$datadir/glib-2.0/schemas\" or XDG themes data
in INPUTS.  Return a list with all found directories."
  (define (data-directory input previous)
    (let* ((in (match input
                 ((_ . dir) dir)
                 (_ "")))
           (datadir (string-append in "/share")))
      (if (and (or (subdirectory-exists? datadir "/glib-2.0/schemas")
                   (subdirectory-exists? datadir "/sounds")
                   (subdirectory-exists? datadir "/themes")
                   (subdirectory-exists? datadir "/cursors")
                   (subdirectory-exists? datadir "/wallpapers")
                   (subdirectory-exists? datadir "/icons"))                   
               (not (directory-included? datadir previous)))
          (cons datadir previous)
          previous)))

  (fold data-directory '() inputs))

;; All GIO modules are expected to be installed in GLib's $libdir/gio/modules
;; directory.  That directory has to include a file called giomodule.cache
;; listing all available modules.  GIO can be made aware of modules in other
;; directories with the help of the environment variable GIO_EXTRA_MODULES.
;; The official GIO documentation states that this environment variable should
;; only be used for testing and not in a production environment.  However, it
;; appears that there is no other way of specifying multiple modules
;; directories (NIXOS also does use this variable). See
;; https://developer.gnome.org/gio/stable/running-gio-apps.html
(define (gio-module-directories inputs)
  "Check for the existence of \"$libdir/gio/modules\" in the INPUTS and
returns a list with all found directories."
  (define (gio-module-directory input previous)
    (let* ((in (match input
                 ((_ . dir) dir)
                 (_ "")))
           (gio-mod-dir (string-append in "/lib/gio/modules")))
      (if (and (directory-exists? gio-mod-dir)
               (not (directory-included? gio-mod-dir previous)))
          (cons gio-mod-dir previous)
          previous)))

  (fold gio-module-directory '() inputs))

(define* (wrap-all-programs #:key inputs outputs
                            (glib-or-gtk-wrap-excluded-outputs '())
                            #:allow-other-keys)
  "Implement phase \"glib-or-gtk-wrap\": look for GSettings schemas and
gtk+-v.0 libraries and create wrappers with suitably set environment variables
if found.

Wrapping is not applied to outputs whose name is listed in
GLIB-OR-GTK-WRAP-EXCLUDED-OUTPUTS.  This is useful when an output is known not
to contain any GLib or GTK+ binaries, and where wrapping would gratuitously
add a dependency of that output on GLib and GTK+."
  (define handle-output
    (match-lambda
     ((output . directory)
      (unless (member output glib-or-gtk-wrap-excluded-outputs)
        (let* ((bindir       (string-append directory "/bin"))
               (bin-list     (find-files bindir ".*"))
               (datadirs     (data-directories
                              (alist-cons output directory inputs)))
               (gtk-mod-dirs (gtk-module-directories
                              (alist-cons output directory inputs)))
               (gio-mod-dirs (gio-module-directories
                              (alist-cons output directory inputs)))
               (data-env-var
                (if (not (null? datadirs))
                    `("XDG_DATA_DIRS" ":" prefix ,datadirs)
                    #f))
               (gtk-mod-env-var
                (if (not (null? gtk-mod-dirs))
                    `("GTK_PATH" ":" prefix ,gtk-mod-dirs)
                    #f))
               (gio-mod-env-var 
                (if (not (null? gio-mod-dirs))
                    `("GIO_EXTRA_MODULES" ":" prefix ,gio-mod-dirs)
                    #f)))
          (cond
           ((and data-env-var gtk-mod-env-var gio-mod-env-var)
            (for-each (cut wrap-program <>
                           data-env-var
                           gtk-mod-env-var
                           gio-mod-env-var)
                      bin-list))
           ((and data-env-var gtk-mod-env-var (not gio-mod-env-var))
            (for-each (cut wrap-program <>
                           data-env-var
                           gtk-mod-env-var)
                      bin-list))
           ((and data-env-var (not gtk-mod-env-var) gio-mod-env-var)
            (for-each (cut wrap-program <>
                           data-env-var
                           gio-mod-env-var)
                      bin-list))
           ((and (not data-env-var) gtk-mod-env-var gio-mod-env-var)
            (for-each (cut wrap-program <>
                           gio-mod-env-var
                           gtk-mod-env-var)
                      bin-list))
           ((and data-env-var (not gtk-mod-env-var) (not gio-mod-env-var))
            (for-each (cut wrap-program <>
                           data-env-var)
                      bin-list))
           ((and (not data-env-var) gtk-mod-env-var (not gio-mod-env-var))
            (for-each (cut wrap-program <>
                           gtk-mod-env-var)
                      bin-list))
           ((and (not data-env-var) (not gtk-mod-env-var) gio-mod-env-var)
            (for-each (cut wrap-program <>
                           gio-mod-env-var)
                      bin-list))))))))

  (for-each handle-output outputs)
  #t)

(define* (compile-glib-schemas #:key outputs #:allow-other-keys)
  "Implement phase \"glib-or-gtk-compile-schemas\": compile \"glib\" schemas
if needed."
  (every (match-lambda
          ((output . directory)
           (let ((schemasdir (string-append directory
                                            "/share/glib-2.0/schemas")))
             (if (and (directory-exists? schemasdir)
                      (not (file-exists?
                            (string-append schemasdir "/gschemas.compiled"))))
                 (zero? (system* "glib-compile-schemas" schemasdir))
                 #t))))
         outputs))

(define* (generate-icon-cache #:key outputs #:allow-other-keys)
  "Implement phase \"glib-or-gtk-icon-cache\": generate icon cache if
needed."
  (every (match-lambda
          ((output . directory)
           (let ((iconsdir (string-append directory
                                            "/share/icons")))
             (when (file-exists? iconsdir)
               (with-directory-excursion iconsdir
                 (for-each
                  (lambda (dir)
                    (unless (file-exists?
                             (string-append iconsdir "/" dir "/"
                                            "icon-theme.cache"))
                      (system* "gtk-update-icon-cache"
                               "--ignore-theme-index"
                               (string-append iconsdir "/" dir))))
                  (scandir "."
                           (lambda (name)
                             (and
                              (not (equal? name "."))
                              (not (equal? name ".."))
                              (equal? 'directory
                                      (stat:type (stat name)))))))))
             #t)))
         outputs))

(define %standard-phases
  (alist-cons-after
   'install 'glib-or-gtk-wrap wrap-all-programs
   (alist-cons-after
    'install 'glib-or-gtk-icon-cache generate-icon-cache
    (alist-cons-after
     'install 'glib-or-gtk-compile-schemas compile-glib-schemas
     gnu:%standard-phases))))

(define* (glib-or-gtk-build #:key inputs (phases %standard-phases)
                            #:allow-other-keys #:rest args)
  "Build the given package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

;;; glib-or-gtk-build-system.scm ends here
