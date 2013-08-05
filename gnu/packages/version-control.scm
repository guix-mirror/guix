;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages version-control)
  #:use-module ((guix licenses) #:select (asl2.0 gpl1+ gpl2+ gpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages apr)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages system)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages compression))

(define-public bazaar
  (package
    (name "bazaar")
    (version "2.5.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://launchpad.net/bzr/2.5/" version
                          "/+download/bzr-" version ".tar.gz"))
      (sha256
       (base32
        "10krjbzia2avn09p0cdlbx2wya0r5v11w5ymvyl72af5dkx4cwwn"))))
    (build-system python-build-system)
    (inputs
     ;; Note: 'tools/packaging/lp-upload-release' and 'tools/weavemerge.sh'
     ;; require Zsh.
     `(("gettext" ,guix:gettext)))
    (arguments
     `(#:tests? #f)) ; no test target
    (home-page "https://gnu.org/software/bazaar")
    (synopsis "Decentralized revision control system")
    (description
     "GNU Bazaar is a distributed version control system, which supports both
central version control and distributed version control.  Developers can
organize their workspace in whichever way they want.  It is possible to work
from a command line or use a GUI application.")
    (license gpl2+)))

(define-public subversion
  (package
    (name "subversion")
    (version "1.7.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://archive.apache.org/dist/subversion/subversion-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "11inl9n1riahfnbk1fax0dysm2swakzhzhpmm2zvga6fikcx90zw"))))
    (build-system gnu-build-system)
    (inputs
      `(("apr" ,apr)
        ("apr-util" ,apr-util)
        ("perl" ,perl)
        ("python" ,python)
        ("sqlite" ,sqlite)
        ("zlib" ,zlib)))
    (home-page "http://subversion.apache.org/")
    (synopsis "Subversion, a revision control system")
    (description
     "Subversion exists to be universally recognized and adopted as an
open-source, centralized version control system characterized by its
reliability as a safe haven for valuable data; the simplicity of its model and
usage; and its ability to support the needs of a wide variety of users and
projects, from individuals to large-scale enterprise operations.")
    (license asl2.0)))

(define-public rcs
  (package
    (name "rcs")
    (version "5.9.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/rcs/rcs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0w26vsx732dcmb5qfhlkkzvrk1sx6d74qibrn914n14j0ci90jcq"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/rcs/")
    (synopsis "Per-file local revision control system")
    (description
     "The GNU Revision Control System (RCS) manages multiple revisions of
files. RCS automates the storing, retrieval, logging, identification, and
merging of revisions.  RCS is useful for text that is revised frequently,
including source code, programs, documentation, graphics, papers, and form
letters.")
    (license gpl3+)))

(define-public cvs
  (package
    (name "cvs")
    (version "1.12.13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://ftp.gnu.org/non-gnu/cvs/source/feature/"
                   version "/cvs-" version ".tar.bz2"))
             (sha256
              (base32
               "0pjir8cwn0087mxszzbsi1gyfc6373vif96cw4q3m1x6p49kd1bq"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The test suite looks flawed, and the package is obsolete anyway.
     '(#:tests? #f))
    (inputs `(("zlib" ,zlib)
              ("nano" ,nano)))                    ; the default editor
    (home-page "http://cvs.nongnu.org")
    (synopsis "Historical centralized version control system")
    (description
     "CVS is a version control system, an important component of Source
Configuration Management (SCM).  Using it, you can record the history of
sources files, and documents.  It fills a similar role to the free software
RCS, PRCS, and Aegis packages.")
    (license gpl1+)))

(define-public vc-dwim
  (package
    (name "vc-dwim")
    (version "1.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/vc-dwim/vc-dwim-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "094pjwshvazlgagc254in2xvrp93vhcj0kb5ms17qs7sch99x9z2"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("inetutils" ,inetutils)     ; for `hostname', used in the tests
              ("emacs" ,emacs)))           ; for `ctags'
    (home-page "http://www.gnu.org/software/vc-dwim/")
    (synopsis "Version-control-agnostic ChangeLog diff and commit tool")
    (description
     "vc-dwim is a version-control-agnostic ChangeLog diff and commit
tool. vc-chlog is a helper tool for writing GNU-style ChangeLog entries.")
    (license gpl3+)))
