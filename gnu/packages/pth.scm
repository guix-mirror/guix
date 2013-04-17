;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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
    (arguments '(#:parallel-build? #f))
    (home-page "http://www.gnu.org/software/pth")
    (synopsis "Portable thread library")
    (description
     "Pth is a very portable POSIX/ANSI-C based library for Unix
platforms which provides non-preemptive priority-based scheduling for
multiple threads of execution (aka ``multithreading'') inside
event-driven applications. All threads run in the same address space of
the server application, but each thread has it's own individual
program-counter, run-time stack, signal mask and errno variable.

The thread scheduling itself is done in a cooperative way, i.e., the
threads are managed by a priority- and event-based non-preemptive
scheduler. The intention is that this way one can achieve better
portability and run-time performance than with preemptive
scheduling. The event facility allows threads to wait until various
types of events occur, including pending I/O on file descriptors,
asynchronous signals, elapsed timers, pending I/O on message ports,
thread and process termination, and even customized callback functions.

Additionally Pth provides an optional emulation API for POSIX.1c
threads (\"Pthreads\") which can be used for backward compatibility to
existing multithreaded applications.")
    (license lgpl2.1+)))
