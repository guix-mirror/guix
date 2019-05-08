;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fabric-management)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

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
     `(#:tests? #f ; no check target
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
             (invoke "autoconf")
             #t))
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
               (with-directory-excursion "infiniband_umad"
                 (invoke "./configure"))
               (with-directory-excursion "lmsensors"
                 (let ((base (assoc-ref inputs "lm-sensors")))
                   (invoke "./configure"
                           (string-append "--with-sensors_incdir="
                                          base "/include/sensors")
                           (string-append "--with-sensors_libdir="
                                          base "/lib")))))
             #t))
         (add-after 'install 'extra-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "out")
                                       "/share/doc/" ,name "-" ,version)))
               (chdir "..")             ; we went into src above
               (for-each (lambda (file)
                           (install-file file doc))
                         '("README" "RELEASENOTES.txt"))
               #t))))))
    (home-page "https://icl.utk.edu/papi/")
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
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.vi-hps.org/upload/packages/otf2/otf2-"
                           version ".tar.gz"))
       (sha256 (base32 "1ls7rz6qwnqbkifpafc95bnfh3m9xbs74in8zxlnhfbgwx11nn81"))))
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
    (description "The Open Trace Format 2 (@dfn{OTF2}) is a scalable,
memory-efficient event trace data format plus support library.")
    (license license:bsd-3)))

(define-public opari2
  (package
    (name "opari2")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.vi-hps.org/cms/upload/packages/opari2/"
                           "opari2-" version ".tar.gz"))
       (sha256 (base32 "0cd1l30x8hygvbh6yay7mn483vjq3z4b7wnsffs80rznj93k57pn"))))
    (build-system gnu-build-system)
    (inputs `(("gfortran" ,gfortran)))
    (native-inputs `(("gawk" ,gawk)     ; for tests
                     ("which" ,which)))
    (home-page "http://www.vi-hps.org/projects/score-p")
    (synopsis "OpenMP runtime performance measurement instrumenter")
    (description "OPARI2 is a source-to-source instrumentation tool for OpenMP
and hybrid codes.  It surrounds OpenMP directives and runtime library calls
with calls to the POMP2 measurement interface.")
    (license license:bsd-3)))

(define-public cube
  (package
    (name "cube")
    (version "4.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://apps.fz-juelich.de/scalasca/releases/cube/4.3/dist/cube-"
             version ".tar.gz"))
       (sha256 (base32 "04irflia4rfw02093w9nx7rr98r640y4q8hisjywvd4b7r3nzhhx"))
       (patches (search-patches "cube-nocheck.patch"))))
    (inputs `(("dbus" ,dbus)
              ("zlib" ,zlib)))
    (native-inputs `(("perl" ,perl)
                     ("qtbase" ,qtbase)           ; native because of qmake
                     ("which" ,which)))

    ;; FIXME: The doc is 14MB, but adding a doc output results in a cycle.
    (outputs '("out"                              ;"doc"
               "lib"))

    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--enable-shared" "--disable-static" "--disable-silent-rules"
         ,(string-append "LDFLAGS=-L" (assoc-ref %outputs "lib") "/lib"))
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'rpath
           ;; Account for moving GUI stuff
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((wl (string-append "-Wl,-rpath=" (assoc-ref outputs "out")
                                      "/lib")))
               (substitute* "build-backend/Makefile"
                 (("^cube_LDFLAGS =") (string-append "cube_LDFLAGS = " wl))
                 (("^libheatmap_plugin_la_LDFLAGS =")
                  (string-append "libheatmap_plugin_la_LDFLAGS = " wl))
                 (("^libbarplot_plugin_la_LDFLAGS =")
                  (string-append "libbarplot_plugin_la_LDFLAGS = " wl)))
               #t)))
         (add-before 'install 'includes-cube
           ;; It tries to install here before include exists.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((inc (string-append (assoc-ref outputs "lib") "/include")))
               (mkdir-p (string-append inc "/cube"))
               (mkdir-p (string-append inc "/cubew"))
               #t)))
         (add-after 'install 'licence
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "lib")
                                       "/share/doc/cube")))
               (install-file "COPYING" doc)
               #t)))
         ;; XXX: Commented due to cycle (see comment above.)
         ;; (add-after 'install 'doc
         ;;   (lambda _
         ;;     (let ((share (string-append (assoc-ref %outputs "doc")
         ;;                                 "/share")))
         ;;       (mkdir-p share)
         ;;       (rename-file (string-append %output "/share/doc")
         ;;                    (string-append share "/doc")))))
         (add-after 'install 'gui-stuff
           ;; Get the Qt horror dependencies out of the lib closure
           (lambda _
             (let ((outlib (string-append (assoc-ref %outputs "out") "/lib"))
                   (lib (string-append (assoc-ref %outputs "lib") "/lib")))
               (mkdir-p outlib)
               (rename-file (string-append lib "/cube-plugins")
                            (string-append outlib "/cube-plugins"))
               (for-each (lambda (file)
                           (rename-file
                            file (string-append outlib "/" (basename file))))
                         (append (find-files lib "libgraphwidgetcommon-plugin\\..*")
                                 (find-files lib "libcube4gui\\.so.*")))
               #t)))
         (add-after 'install 'move-include
           ;; Most of the headers end up under %output for some reason,
           ;; despite --includedir in configure.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((outinc (string-append (assoc-ref outputs "out")
                                          "/include"))
                   (libinc (string-append (assoc-ref outputs "lib")
                                          "/include")))
               (for-each (lambda (file)
                           (let ((from (string-append outinc "/" file)))
                             (copy-recursively from libinc)
                             (delete-file-recursively from)))
                         '("cube" "cubew"))
               #t)))

         ;; XXX: This doesn't work because cube-config, which is needed for
         ;; building stuff, sources cube-config-frontend.  We don't want that
         ;; in the lib output because it pulls in >1GB via QT.
         ;;
         ;; (add-after 'install 'cube-config
         ;;   (lambda _
         ;;     (let* ((lib (assoc-ref %outputs "lib"))
         ;;            (libbin (string-append lib "/bin")))
         ;;       (mkdir-p libbin)
         ;;       (system (string-append "mv " (assoc-ref %outputs "out")
         ;;                              "/bin/cube-config* " libbin))
         ;;       (substitute* (list (string-append libbin "/cube-config"))
         ;;         (("^prefix=.*") (string-append "prefix=" lib))
         ;;         (("^exec_prefix=\"\\$\\{prefix\\}\"")
         ;;          (string-append "exec_prefix=" lib))))))
         (add-after 'install 'cube-config
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((lib (assoc-ref outputs "lib"))
                    (libbin (string-append lib "/bin")))
               (mkdir-p libbin)
               (install-file (string-append %output "/bin/cube-config") libbin)
               (install-file (string-append %output "/bin/cube-config-backend")
                             libbin)
               (substitute* (list (string-append libbin "/cube-config"))
                 (("^source .*frontend.*$") "")
                 (((assoc-ref outputs "out")) lib))
               #t))))))
    (home-page "http://www.scalasca.org/software/cube-4.x/download.html")
    (synopsis "Performance report explorer for parallel programs")
    (description
     "CUBE (CUBE Uniform Behavioral Encoding) is a tool to display a variety
of performance metrics for parallel programs including MPI and OpenMP
applications.  CUBE allows interactive exploration of a multidimensional
performance space in a scalable fashion.  Scalability is achieved in two ways:
hierarchical decomposition of individual dimensions and aggregation across
different dimensions.  All performance metrics are uniformly accommodated in
the same display and thus provide the ability to easily compare the effects of
different kinds of performance behavior.")
    (license license:bsd-3)))

(define (make-scorep mpi)
  (package
    (name (string-append "scorep-" (package-name mpi)))
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.vi-hps.org/upload/packages/scorep/scorep-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0h45357djna4dn9jyxx0n36fhhms3jrf22988m9agz1aw2jfivs9"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled software.
               '(begin
                  (for-each delete-file-recursively
                            '("vendor/opari2" "vendor/cube"))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     `(("mpi" ,mpi)
       ("papi" ,papi)
       ("opari2" ,opari2)
       ("libunwind" ,libunwind)
       ("otf2" ,otf2)
       ("cubelib" ,cube "lib")                    ;for lib, include
       ("openmpi" ,openmpi)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("flex" ,flex)
       ("cube" ,cube)                             ;for cube-config
       ("bison" ,bison)
       ("python" ,python)
       ("doxygen" ,doxygen)
       ("which" ,which)))
    (arguments
     `(#:configure-flags
       (list "--enable-shared" "--disable-static"
             (string-append "--with-opari2="
                            (assoc-ref %build-inputs "opari2"))
             (string-append "--with-cube="
                            (assoc-ref %build-inputs "cube")))
       #:parallel-tests? #f
       #:make-flags '("V=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'licence
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "out")
                                       "/share/doc/scorep")))
               (install-file "COPYING" doc)
               #t))))))
    (home-page "http://www.vi-hps.org/projects/score-p/")
    (synopsis "Performance measurement infrastructure for parallel code")
    (description
     "The Score-P (Scalable Performance Measurement Infrastructure for
Parallel Codes) measurement infrastructure is a scalable and easy-to-use tool
suite for profiling, event trace recording, and online analysis of
high-performance computing (HPC) applications.")
    (license license:cpl1.0)))

(define-public scorep-openmpi (make-scorep openmpi))
