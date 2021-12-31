;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Carl Dong <contact@carldong.me>
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

(define-module (gnu packages installers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system scons)
  #:use-module (guix utils))

(define (make-nsis machine target-arch nsis-target-type)
  (let* ((triplet (string-append machine "-" "w64-mingw32"))
         (xbinutils (cross-binutils triplet))
         (xlibc (cross-libc triplet))
         (xgcc (cross-gcc triplet #:libc xlibc)))
    (package
      (name (string-append "nsis-" machine))
      (version "3.05")
      (source (origin
                (method url-fetch)
                (uri (string-append "http://prdownloads.sourceforge.net/nsis/nsis-"
                                    version "-src.tar.bz2"))
                (sha256
                 (base32
                  "1sbwx5vzpddharkb7nj4q5z3i5fbg4lan63ng738cw4hmc4v7qdn"))
                (patches (search-patches "nsis-env-passthru.patch"
                                         "nsis-source-date-epoch.patch"))))
      (build-system scons-build-system)
      (native-inputs `(("xgcc" ,xgcc)
                       ("xbinutils" ,xbinutils)
                       ("mingw-w64" ,xlibc)))
      (inputs (list zlib))
      (arguments
       `(#:scons ,scons-python2
         #:modules ((srfi srfi-1)
                    (srfi srfi-26)
                    (guix build utils)
                    (guix build scons-build-system))
         #:tests? #f
         #:scons-flags `("UNICODE=yes"
                         "SKIPUTILS=MakeLangId,Makensisw,NSIS Menu,SubStart,zip2exe"
                         "SKIPDOC=COPYING"
                         "STRIP_CP=no"
                         ,(string-append "PREFIX=" %output)
                         ,(string-append "TARGET_ARCH=" ,target-arch)
                         ,(string-append "XGCC_W32_PREFIX=" ,triplet "-")
                         ,(string-append "PREFIX_PLUGINAPI_INC=" (assoc-ref %build-inputs "mingw-w64") "/include/")
                         ,(string-append "PREFIX_PLUGINAPI_LIB=" (assoc-ref %build-inputs "mingw-w64") "/lib/"))
         #:build-targets '("makensis"
                           "stubs"
                           "plugins"
                           "utils")
         #:install-targets '("install-stubs"
                             "install-plugins"
                             "install-data"
                             "install-utils"
                             "install-compiler"
                             "install-conf")
         #:phases (modify-phases %standard-phases
                    (add-before 'build 'fix-env
                      (lambda _
                        (define* (filter-delimited-string delimited-string predicate #:optional (delimiter #\:))
                          ;; Given a DELIMITED-STRING delimited by DELIMITER,
                          ;; only keep items that satisfy PREDICATE
                          (string-join
                           (filter predicate (string-split delimited-string delimiter))
                           (string delimiter)))
                        (define (mingw-path? path)
                          (string-prefix? (assoc-ref %build-inputs "mingw-w64") path))
                        (for-each
                         (lambda (env-name)
                           (let ((env-val (getenv env-name)))
                             ;; Remove all mingw-w64 paths from env vars meant
                             ;; for native toolchain
                             (setenv env-name
                                     (filter-delimited-string env-val (negate mingw-path?)))
                             ;; Add the removed paths back into CROSS_-prefixed
                             ;; version of env vars
                             (setenv (string-append "CROSS_" env-name)
                                     (filter-delimited-string env-val mingw-path?))))
                         '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH" "LIBRARY_PATH"))
                        ;; Hack to place mingw-w64 path at the end of search
                        ;; paths.  Could probably use a specfile and dirafter
                        (setenv "CROSS_C_INCLUDE_PATH"
                                (string-join
                                 `(,@(map (cut string-append
                                               (assoc-ref %build-inputs "xgcc")
                                               "/lib/gcc/" ,triplet "/"
                                               ,(package-version xgcc) <>)
                                          '("/include"
                                            "/include-fixed"))
                                   ,(getenv "CROSS_C_INCLUDE_PATH"))
                                 ":"))
                        (setenv "CROSS_CPLUS_INCLUDE_PATH"
                                (string-join
                                 `(,@(map (cut string-append (assoc-ref %build-inputs "xgcc") <>)
                                          `("/include/c++"
                                            ,(string-append "/include/c++/" ,triplet)
                                            "/include/c++/backward"
                                            ,@(map (cut string-append "/lib/gcc/" ,triplet "/" ,(package-version xgcc) <>)
                                                   '("/include"
                                                     "/include-fixed"))))
                                   ,(getenv "CROSS_CPLUS_INCLUDE_PATH"))
                                 ":"))))
                    (add-before 'build 'fix-target-detection
                      (lambda _
                        ;; NSIS target detection is screwed up, manually change
                        ;; it ourselves
                        (substitute* "Source/build.cpp" (("m_target_type=TARGET_X86ANSI")
                                                         (string-append "m_target_type=" ,nsis-target-type))))))))
      (home-page "http://nsis.sourceforge.net/")
      (synopsis "Professional open source system to create Windows installers")
      (description
       "NSIS (Nullsoft Scriptable Install System) is a professional open source
system to create Windows installers. It is designed to be as small and flexible
as possible and is therefore very suitable for internet distribution.")
      (license (license:non-copyleft "file://COPYING"
                                     "See COPYING in the distribution.")))))

(define-public nsis-x86_64
  (make-nsis "x86_64" "amd64" "TARGET_AMD64"))

(define-public nsis-i686
  (make-nsis "i686" "x86" "TARGET_X86UNICODE"))
