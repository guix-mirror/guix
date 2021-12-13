;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages pkg-config)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (guix memoization)
  #:export (pkg-config))

;; This is the "primitive" pkg-config package.  People should use `pkg-config'
;; (see below) rather than `%pkg-config', but we export `%pkg-config' so that
;; `fold-packages' finds it.
(define-public %pkg-config
  (package
   (name "pkg-config")
   (version "0.29.2")
   (source (origin
            (method url-fetch)
            (uri (list
                  (string-append
                   "http://fossies.org/linux/misc/pkg-config-" version
                   ".tar.gz")

                  ;; FIXME: The following URL redirects to HTTPS, which
                  ;; creates bootstrapping problems:
                  ;; <http://bugs.gnu.org/22774>.
                  (string-append
                   "http://pkgconfig.freedesktop.org/releases/pkg-config-"
                   version ".tar.gz")))
            (sha256
             (base32
              "14fmwzki1rlz8bs2p810lk6jqdxsk966d8drgsjmi54cd00rrikg"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      '("--with-internal-glib"
        ;; Those variables are guessed incorrectly when cross-compiling.
        ;; See: https://developer.gimp.org/api/2.0/glib/glib-cross-compiling.html.
        ,@(if (%current-target-system)
              '("glib_cv_stack_grows=no"
                "glib_cv_uscore=no"
                "ac_cv_func_posix_getpwuid_r=yes"
                "ac_cv_func_posix_getgrgid_r=yes")
              '()))))
   (native-search-paths
    (list (search-path-specification
           (variable "PKG_CONFIG_PATH")
           (files '("lib/pkgconfig" "lib64/pkgconfig" "share/pkgconfig")))))
   (home-page "https://www.freedesktop.org/wiki/Software/pkg-config")
   (license gpl2+)
   (synopsis "Helper tool used when compiling applications and libraries")
   (description
    "pkg-config is a helper tool used when compiling applications and
libraries.  It helps you insert the correct compiler options on the
command line so an application can use gcc -o test test.c `pkg-config
--libs --cflags glib-2.0` for instance, rather than hard-coding values
on where to find glib (or other libraries).  It is language-agnostic, so
it can be used for defining the location of documentation tools, for
instance.")))

(define cross-pkg-config
  (mlambda (target)
    "Return a pkg-config for TARGET, essentially just a wrapper called
`TARGET-pkg-config', as `configure' scripts like it."
    ;; See <http://www.flameeyes.eu/autotools-mythbuster/pkgconfig/cross-compiling.html>
    ;; for details.
    (package
      (inherit %pkg-config)
      (name (string-append (package-name %pkg-config) "-" target))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))

                     (let* ((in     (assoc-ref %build-inputs "pkg-config"))
                            (out    (assoc-ref %outputs "out"))
                            (bin    (string-append out "/bin"))
                            (prog   (string-append ,target "-pkg-config"))
                            (native (string-append in "/bin/pkg-config")))

                       (mkdir-p bin)

                       ;; Create a `TARGET-pkg-config' -> `pkg-config' symlink.
                       ;; This satisfies the pkg.m4 macros, which use
                       ;; AC_PROG_TOOL to determine the `pkg-config' program
                       ;; name.
                       (symlink native (string-append bin "/" prog))

                       ;; Also make 'pkg.m4' available, some packages might
                       ;; expect it.
                       (mkdir-p (string-append out "/share"))
                       (symlink (string-append in "/share/aclocal")
                                (string-append out "/share/aclocal"))
                       #t))))
      (native-inputs `(("pkg-config" ,%pkg-config)))

      ;; Ignore native inputs, and set `PKG_CONFIG_PATH' for target inputs.
      (native-search-paths '())
      (search-paths (package-native-search-paths %pkg-config)))))

(define (pkg-config-for-target target)
  "Return a pkg-config package for TARGET, which may be either #f for a native
build, or a GNU triplet."
  (if target
      (cross-pkg-config target)
      %pkg-config))

;; This hack allows us to automatically choose the native or the cross
;; `pkg-config' depending on whether it's being used in a cross-build
;; environment or not.
(define-syntax pkg-config
  (identifier-syntax (pkg-config-for-target (%current-target-system))))

;; This hack allows for using both "pkg-config" and "TARGET-pkg-config"
;; at the same time.  Simply using '%pkg-config' and 'pkg-config' won't
;; work because they both use the "PKG_CONFIG_PATH" environment variable.
(define-public pkg-config-for-build
  (package
    (inherit (hidden-package %pkg-config))
    (name "pkg-config-for-build")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (inputs
     (list bash-minimal %pkg-config))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       ,#~(begin
            (use-modules (guix build utils))
            (define where (string-append #$output "/bin/pkg-config"))
            (mkdir-p (dirname where))
            (call-with-output-file where
              (lambda (port)
                (format port "#!~a
export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH_FOR_BUILD\"
exec ~a \"$@\""
                        (search-input-file %build-inputs "bin/bash")
                        (search-input-file %build-inputs "bin/pkg-config"))))
            (chmod where #o500))))
    (native-search-paths
     (map (lambda (original)
            (search-path-specification
             (inherit original)
             (variable "PKG_CONFIG_PATH_FOR_BUILD")))
          (package-native-search-paths %pkg-config)))))
