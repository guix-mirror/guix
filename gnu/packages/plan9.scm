;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 宋文武 <iyzsong@member.fsf.org>
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

(define-module (gnu packages plan9)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xorg))

(define-public drawterm
  (let ((revision "1")
        (commit "c97fe4693f6112504d6f13fab46f7cc8b27685c1"))
    (package
      (name "drawterm")
      (version (git-version "20210628" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.9front.org/plan9front/drawterm")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "059sl60ap6c9lz8k91k6bd34694a290wm0s93b2vfszzzv683spw"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CONF=unix"
                            (string-append "CC=" ,(cc-for-target)))
         #:tests? #f                    ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no configure script
           (replace 'install            ; no install target
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/"))
                      (man (string-append out "/share/man/man1/")))
                 (install-file "drawterm" bin)
                 (install-file "drawterm.1" man)))))))
      (inputs
       (list libx11 libxt))
      (synopsis "Connect to Plan 9 systems")
      (home-page "http://drawterm.9front.org")
      (description
       "@command{drawterm} is a client for connecting venerable systems to
Plan 9 systems.  It behaves like a Plan 9 kernel and will attempt to
reconstruct a Plan 9 terminal-like experience from a non-Plan 9 system.")
      (license license:expat))))
