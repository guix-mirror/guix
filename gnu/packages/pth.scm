;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
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

(define-module (gnu packages pth)
  #:use-module (gnu packages autotools)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public pth
  (package
    (name "pth")
    (version "2.0.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pth/pth-" version
                          ".tar.gz"))
      (sha256
       (base32
        "0ckjqw5kz5m30srqi87idj7xhpw6bpki43mj07bazjm2qmh3cdbj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'update-config-scripts
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             ;; Replace outdated config.guess and config.sub.
             (for-each (lambda (file)
                         (install-file
                          (search-input-file
                           (or native-inputs inputs)
                           (string-append "/bin/" file)) "."))
                       '("config.guess" "config.sub")))))))
    (native-inputs
     (list config))
    (home-page "https://www.gnu.org/software/pth/")
    (synopsis "Portable thread library")
    (description
     "GNU Pth is a portable library providing non-preemptive, priority-based
scheduling for multiple execution threads.  Each thread has its own
program-counter, run-time stack, signal mask and errno variable.  Threads are
scheduled in a cooperative way, rather than in the standard preemptive way,
such that they are managed according to priority and events.  However, Pth
also features emulation of POSIX.1c threads (\"pthreads\") for backwards
compatibility.")
    (license lgpl2.1+)))
