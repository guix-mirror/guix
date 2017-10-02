;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Dave Love <fx@gnu.org>
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

(define-module (gnu packages profiling)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:) ; avoid zlib, expat clashes
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)      ;for "which"
  #:use-module (gnu packages fabric-management)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python))

;; Fixme: Separate out lib and fix resulting cycle errors; separate libpfm
;; output(?); build libmsr and add that component.
(define-public papi
  (package
    (name "papi")
    (version "5.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://icl.utk.edu/projects/papi/downloads/papi-"
                           version ".tar.gz"))
       (sha256 (base32 "1m62s8fkjjgq04ayf18jcxc33rqfd7nrkdw1gr54q5pn4cijrp29"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("lm-sensors" ,lm-sensors "lib")
       ("rdma-core" ,rdma-core)
       ("infiniband-diags" ,infiniband-diags "lib")
       ("net-tools" ,net-tools)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("gfortran" ,gfortran)))
    (arguments
     '(#:tests? #f ; no check target
       #:configure-flags
       ;; These are roughly per Fedora, but elide mx (assumed to be dead, even
       ;; Open-MX) and add and powercap -- I don't know the pros/cons of
       ;; infiniband and infiniband_mad, but you can't use them together, and
       ;; the umad version needs at least one patch.
       ;; Implicit enabled components: perf_event perf_event_uncore
       `("--with-perf-events" "--with-shared-lib=yes" "--with-shlib"
         "--with-static-lib=no"
         "--with-components=appio coretemp example lustre micpower net rapl \
stealtime lmsensors infiniband powercap"
         ;; So utils get rpath set correctly:
         ,(string-append "LDFLAGS=-Xlinker -rpath -Xlinker "
                         (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoconf
           (lambda _
             (chdir "src")
             (zero? (system* "autoconf"))))
         ;; Amalgamating with the following clause gives double substitution.
         (add-before 'patch-source-shebangs 'patch-components
           (lambda _
             (with-directory-excursion "src/components"
               (substitute* '("lmsensors/configure" "infiniband_umad/configure")
                 (("/bin/sh") (which "sh"))))
             #t))
         (add-after 'configure 'components
           (lambda*  (#:key inputs #:allow-other-keys)
             (with-directory-excursion "components"
               (and
                (with-directory-excursion "infiniband_umad"
                  (zero? (system* "./configure")))
                (with-directory-excursion "lmsensors"
                  (let ((base  (assoc-ref inputs "lm-sensors")))
                    (zero?
                     (system*
                      "./configure"
                      (string-append "--with-sensors_incdir=" base
                                     "/include/sensors")
                      (string-append "--with-sensors_libdir=" base "/lib")))))))))
         (add-after 'install 'extra-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "out")
                                       "/share/doc")))
               (mkdir-p doc)
               (chdir "..")             ; we went into src above
               (for-each (lambda (file)
                           (install-file file doc))
                         '("README" "RELEASENOTES.txt" "LICENSE.txt"))
               #t))))))
    (home-page "http://icl.cs.utk.edu/papi/")
    (synopsis "Performance Application Programming Interface")
    (description
     "PAPI provides the tool designer and application engineer with a consistent
interface and methodology for use of the performance counter hardware found in
most major microprocessors.  PAPI enables software engineers to see, in near
real time, the relation between software performance and processor events.

In addition, PAPI provides access to a collection of components that expose
performance measurement opportunites across the hardware and software stack.")
    ;; See Debian papi copyright file.
    (license (list license:bsd-3
                   license:lgpl2.1+     ;src/components/infiniband/pscanf.h
                   ;; not used in output
                   license:gpl2+ ;src/components/appio/tests/iozone/gengnuplot.sh
                                 ;src/libpfm-3.y/*/multiplex*
                   ;; "BSD-like": src/libpfm-3.y/*, src/libpfm4/*
                   ;; lgpl2.1+: src/perfctr-2.*/*
                   ))))

;; NB. there's a potential name clash with libotf.
(define-public otf2
  (package
    (name "otf2")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.vi-hps.org/upload/packages/otf2/otf2-"
                           version ".tar.gz"))
       (sha256 (base32 "1lyaqhdfaqm1kd23yk71g71vkscw83s7m57j017y768h8sh8xlwa"))))
    (native-inputs `(("python" ,python)))
    (outputs '("doc"                              ; 18MB
               "lib"
               "out"))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-shared" "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'licence
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (output)
                         (let ((doc (string-append (assoc-ref outputs output)
                                                   "/share/doc/otf2")))
                           (install-file "COPYING" doc)))
                       '("lib" "doc"))
             #t)))))
    (home-page "http://www.vi-hps.org/projects/score-p/")
    (synopsis "Open Trace Format 2 library")
    (description "The Open Trace Format 2 (OTF2) is a scalable, memory
efficient event trace data format plus support library.")
    (license license:bsd-3)))

(define-public opari2
  (package
    (name "opari2")
    (version "2.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (let* ((parts (string-split version #\.) )
                  (major (car parts))
                  (minor (cadr parts)))
             (string-append "http://www.vi-hps.org/upload/packages/opari2/opari2-"
                            version ".tar.gz")))
      (sha256 (base32 "1ph8l5c646bm9l5vcn8rrbjvkyi7y8yvn2ny95r6kmlzs766g3q8"))))
    (build-system gnu-build-system)
    (inputs `(("gfortran" ,gfortran)))
    (native-inputs `(("gawk" ,gawk)     ;for tests
                     ("which" ,which)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'licence
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "out")
                                       "/share/doc/opari2")))
               (install-file "COPYING" doc)
               #t))))))
    (home-page "http://www.vi-hps.org/projects/score-p")
    (synopsis "OpenMP runtime performance measurement instrumenter")
    (description "OPARI2 is a source-to-source instrumentation tool for OpenMP
and hybrid codes.  It surrounds OpenMP directives and runtime library calls
with calls to the POMP2 measurement interface.")
    (license license:bsd-3)))
