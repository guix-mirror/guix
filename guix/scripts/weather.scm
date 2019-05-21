;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
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

(define-module (guix scripts weather)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (guix progress)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix grafts)
  #:use-module ((guix build syscalls) #:select (terminal-columns))
  #:use-module (guix scripts substitute)
  #:use-module (guix http-client)
  #:use-module (guix ci)
  #:use-module (guix sets)
  #:use-module (guix graph)
  #:autoload   (guix scripts graph) (%bag-node-type)
  #:use-module (gnu packages)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 vlist)
  #:export (guix-weather))

(define (all-packages)
  "Return the list of public packages we are going to query."
  (fold-packages (lambda (package result)
                   (match (package-replacement package)
                     ((? package? replacement)
                      (cons* replacement package result))
                     (#f
                      (cons package result))))
                 '()

                 ;; Dismiss deprecated packages but keep hidden packages.
                 #:select? (negate package-superseded)))

(define (call-with-progress-reporter reporter proc)
  "This is a variant of 'call-with-progress-reporter' that works with monadic
scope."
  ;; TODO: Move to a more appropriate place.
  (with-monad %store-monad
    (start-progress-reporter! reporter)
    (mlet* %store-monad ((report -> (lambda ()
                                      (progress-reporter-report! reporter)))
                         (result (proc report)))
      (stop-progress-reporter! reporter)
      (return result))))

(define* (package-outputs packages
                          #:optional (system (%current-system)))
  "Return the list of outputs of all of PACKAGES for the given SYSTEM."
  (let ((packages (filter (cut supported-package? <> system) packages)))
    (format (current-error-port)
            (G_ "computing ~h package derivations for ~a...~%")
            (length packages) system)

    (call-with-progress-reporter (progress-reporter/bar (length packages))
      (lambda (report)
        (foldm %store-monad
               (lambda (package result)
                 (mlet %store-monad ((drv (package->derivation package system
                                                               #:graft? #f)))
                   (report)
                   (match (derivation->output-paths drv)
                     (((names . items) ...)
                      (return (append items result))))))
               '()
               packages)))))

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))

(define-syntax-rule (let/time ((time result exp)) body ...)
  (call-with-time (lambda () exp) (lambda (time result) body ...)))

(define (histogram field proc seed lst)
  "Return an alist giving a histogram of all the values of FIELD for elements
of LST. FIELD must be a one element procedure that returns a field's value.
For each FIELD value, call PROC with the previous field-specific result.
Example:

  (histogram car (lambda (x n) (+ 1 n)) 0 '((a . x)(b . y)(a . z)))
  => ((a . 2) (b . 1))

meaning that we have two a's and one b."
  (let loop ((lst lst)
             (result '()))
    (match lst
      (()
       result)
      ((head . tail)
       (let ((value (field head)))
         (loop tail
               (match (assoc-ref result value)
                 (#f
                  `((,value . ,(proc head seed)) ,@result))
                 (previous
                  `((,value . ,(proc head previous))
                    ,@(alist-delete value result))))))))))

(define (throughput lst timestamp)
  "Return the throughput, in items per second, given the elements of LST,
calling TIMESTAMP to get the \"timestamp\" of each item."
  (let ((oldest (reduce min +inf.0 (map build-timestamp lst)))
        (now    (time-second (current-time time-utc))))
    (/ (length lst) (- now oldest) 1.)))

(define (queued-subset queue items)
  "Return the subset of ITEMS, a list of store file names, that appears in
QUEUE, a list of builds.  Return #f if elements in QUEUE lack information
about the derivations queued, as is the case with Hydra."
  (define queued
    (append-map (lambda (build)
                  (match (false-if-exception
                          (read-derivation-from-file (build-derivation build)))
                    (#f
                     '())
                    (drv
                     (match (derivation->output-paths drv)
                       (((names . items) ...) items)))))
                queue))

  (if (any (negate build-derivation) queue)
      #f                                          ;no derivation information
      (lset-intersection string=? queued items)))

(define (report-server-coverage server items)
  "Report the subset of ITEMS available as substitutes on SERVER."
  (define MiB (* (expt 2 20) 1.))

  (format #t (G_ "looking for ~h store items on ~a...~%")
          (length items) server)

  (let/time ((time narinfos (lookup-narinfos server items)))
    (format #t "~a~%" server)
    (let ((obtained  (length narinfos))
          (requested (length items))
          (missing   (lset-difference string=?
                                      items (map narinfo-path narinfos)))
          (sizes     (filter-map narinfo-file-size narinfos))
          (time      (+ (time-second time)
                        (/ (time-nanosecond time) 1e9))))
      (format #t (G_ "  ~2,1f% substitutes available (~h out of ~h)~%")
              (* 100. (/ obtained requested 1.))
              obtained requested)
      (let ((total (/ (reduce + 0 sizes) MiB)))
        (match (length sizes)
          ((? zero?)
           (format #t (G_  "  unknown substitute sizes~%")))
          (len
           (if (= len obtained)
               (format #t (G_ "  ~,1h MiB of nars (compressed)~%") total)
               (format #t (G_ "  at least ~,1h MiB of nars (compressed)~%")
                       total)))))
      (format #t (G_ "  ~,1h MiB on disk (uncompressed)~%")
              (/ (reduce + 0 (map narinfo-size narinfos)) MiB))
      (format #t (G_ "  ~,3h seconds per request (~,1h seconds in total)~%")
              (/ time requested 1.) time)
      (format #t (G_ "  ~,1h requests per second~%")
              (/ requested time 1.))

      (guard (c ((http-get-error? c)
                 (if (= 404 (http-get-error-code c))
                     (format (current-error-port)
                             (G_ "  (continuous integration information \
unavailable)~%"))
                     (format (current-error-port)
                             (G_ "  '~a' returned ~a (~s)~%")
                             (uri->string (http-get-error-uri c))
                             (http-get-error-code c)
                             (http-get-error-reason c)))))
        (let* ((max    %query-limit)
               (queue  (queued-builds server max))
               (len    (length queue))
               (histo  (histogram build-system
                                  (lambda (build count)
                                    (+ 1 count))
                                  0 queue)))
          (newline)
          (unless (null? missing)
            (match (queued-subset queue missing)
              (#f #f)
              ((= length queued)
               (let ((missing (length missing)))
                 (format #t (G_ "  ~,1f% (~h out of ~h) of the missing items \
are queued~%")
                         (* 100. (/ queued missing))
                         queued missing)))))

          (if (>= len max)
              (format #t (G_ "  at least ~h queued builds~%") len)
              (format #t (G_ "  ~h queued builds~%") len))
          (for-each (match-lambda
                      ((system . count)
                       (format #t (G_ "      ~a: ~a (~0,1f%)~%")
                               system count (* 100. (/ count len)))))
                    histo))

        (let* ((latest     (latest-builds server))
               (builds/sec (throughput latest build-timestamp)))
          (format #t (G_ "  build rate: ~1,2f builds per hour~%")
                  (* builds/sec 3600.))
          (for-each (match-lambda
                      ((system . builds)
                       (format #t (G_ "      ~a: ~,2f builds per hour~%")
                               system
                               (* (throughput builds build-timestamp)
                                  3600.))))
                    (histogram build-system cons '() latest)))))))


;;;
;;; Command-line options.
;;;

(define (show-help)
  (display (G_ "Usage: guix weather [OPTIONS] [PACKAGES ...]
Report the availability of substitutes.\n"))
  (display (G_ "
      --substitute-urls=URLS
                         check for available substitutes at URLS"))
  (display (G_ "
  -m, --manifest=MANIFEST
                         look up substitutes for packages specified in MANIFEST"))
  (display (G_ "
  -c, --coverage[=COUNT]
                         show substitute coverage for packages with at least
                         COUNT dependents"))
  (display (G_ "
  -s, --system=SYSTEM    consider substitutes for SYSTEM--e.g., \"i686-linux\""))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  (list  (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix weather")))

         (option '("substitute-urls") #t #f
                 (lambda (opt name arg result . rest)
                   (let ((urls (string-tokenize arg)))
                     (for-each (lambda (url)
                                 (unless (string->uri url)
                                   (leave (G_ "~a: invalid URL~%") url)))
                               urls)
                     (apply values
                            (alist-cons 'substitute-urls urls
                                        (alist-delete 'substitute-urls result))
                            rest))))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest arg result)))
         (option '(#\c "coverage") #f #t
                 (lambda (opt name arg result)
                   (alist-cons 'coverage
                               (if arg (string->number* arg) 0)
                               result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg result)))))

(define %default-options
  `((substitute-urls . ,%default-substitute-urls)))

(define (load-manifest file)
  "Load the manifest from FILE and return the list of packages it refers to."
  (let* ((user-module (make-user-module '((guix profiles) (gnu))))
         (manifest    (load* file user-module)))
    (map manifest-entry-item
         (manifest-transitive-entries manifest))))


;;;
;;; Missing package substitutes.
;;;

(define* (package-partition-boundary pred packages
                                     #:key (system (%current-system)))
  "Return the subset of PACKAGES that are at the \"boundary\" between those
that match PRED and those that don't.  The returned packages themselves do not
match PRED but they have at least one direct dependency that does.

Note: The assumption is that, if P matches PRED, then all the dependencies of
P match PRED as well."
  ;; XXX: Graph theoreticians surely have something to teach us about this...
  (let loop ((packages packages)
             (result (setq))
             (visited vlist-null))
    (define (visited? package)
      (vhash-assq package visited))

    (match packages
      ((package . rest)
       (cond ((visited? package)
              (loop rest result visited))
             ((pred package)
              (loop rest result (vhash-consq package #t visited)))
             (else
              (let* ((bag  (package->bag package system))
                     (deps (filter-map (match-lambda
                                         ((label (? package? package) . _)
                                          (and (not (pred package))
                                               package))
                                         (_ #f))
                                       (bag-direct-inputs bag))))
                (loop (append deps rest)
                      (if (null? deps)
                          (set-insert package result)
                          result)
                      (vhash-consq package #t visited))))))
      (()
       (set->list result)))))

(define (package->output-mapping packages system)
  "Return a vhash that maps each item of PACKAGES to its corresponding output
store file names for SYSTEM."
  (foldm %store-monad
         (lambda (package mapping)
           (mlet %store-monad ((drv (package->derivation package system
                                                         #:graft? #f)))
             (return (vhash-consq package
                                  (match (derivation->output-paths drv)
                                    (((names . outputs) ...)
                                     outputs))
                                  mapping))))
         vlist-null
         packages))

(define (substitute-oracle server items)
  "Return a procedure that, when passed a store item (one of those listed in
ITEMS), returns true if SERVER has a substitute for it, false otherwise."
  (define available
    (fold (lambda (narinfo set)
            (set-insert (narinfo-path narinfo) set))
          (set)
          (lookup-narinfos server items)))

  (cut set-contains? available <>))

(define* (report-package-coverage-per-system server packages system
                                             #:key (threshold 0))
  "Report on the subset of PACKAGES that lacks SYSTEM substitutes on SERVER,
sorted by decreasing number of dependents.  Do not display those with less
than THRESHOLD dependents."
  (mlet* %store-monad ((packages -> (package-closure packages #:system system))
                       (mapping    (package->output-mapping packages system))
                       (back-edges (node-back-edges %bag-node-type packages)))
    (define items
      (vhash-fold (lambda (package items result)
                    (append items result))
                  '()
                  mapping))

    (define substitutable?
      (substitute-oracle server items))

    (define substitutable-package?
      (lambda (package)
        (match (vhash-assq package mapping)
          ((_ . items)
           (find substitutable? items))
          (#f
           #f))))

    (define missing
      (package-partition-boundary substitutable-package? packages
                                  #:system system))

    (define missing-count
      (length missing))

    (if (zero? threshold)
        (format #t (N_ "The following ~a package is missing from '~a' for \
'~a':~%"
                       "The following ~a packages are missing from '~a' for \
'~a':~%"
                       missing-count)
                missing-count server system)
        (format #t (N_ "~a package is missing from '~a' for '~a':~%"
                       "~a packages are missing from '~a' for '~a', among \
which:~%"
                       missing-count)
                missing-count server system))

    (for-each (match-lambda
                ((package count)
                 (match (vhash-assq package mapping)
                   ((_ . items)
                    (when (>= count threshold)
                      (format #t "  ~4d\t~a@~a\t~{~a ~}~%"
                              count
                              (package-name package) (package-version package)
                              items)))
                   (#f                      ;PACKAGE must be an internal thing
                    #f))))
              (sort (zip missing
                         (map (lambda (package)
                                (node-reachable-count (list package)
                                                      back-edges))
                              missing))
                    (match-lambda*
                      (((_ count1) (_ count2))
                       (< count2 count1)))))
    (return #t)))

(define* (report-package-coverage server packages systems
                                  #:key (threshold 0))
  "Report on the substitute coverage for PACKAGES, for each of SYSTEMS, on
SERVER.  Display information for packages with at least THRESHOLD dependents."
  (with-store store
    (run-with-store store
      (foldm %store-monad
             (lambda (system _)
               (report-package-coverage-per-system server packages system
                                                   #:threshold threshold))
             #f
             systems))))


;;;
;;; Entry point.
;;;

(define (guix-weather . args)
  (define (package-list opts)
    ;; Return the package list specified by OPTS.
    (let ((file (assoc-ref opts 'manifest))
          (base (filter-map (match-lambda
                              (('argument . spec)
                               (specification->package spec))
                              (_
                               #f))
                            opts)))
      (if (and (not file) (null? base))
          (all-packages)
          (append base
                  (if file (load-manifest file) '())))))

  (with-error-handling
    (parameterize ((current-terminal-columns (terminal-columns)))
      (let* ((opts     (parse-command-line args %options
                                           (list %default-options)
                                           #:build-options? #f))
             (urls     (assoc-ref opts 'substitute-urls))
             (systems  (match (filter-map (match-lambda
                                            (('system . system) system)
                                            (_ #f))
                                          opts)
                         (() (list (%current-system)))
                         (systems systems)))
             (packages (package-list opts))
             (items    (with-store store
                         (parameterize ((%graft? #f))
                           (concatenate
                            (run-with-store store
                              (mapm %store-monad
                                    (lambda (system)
                                      (package-outputs packages system))
                                    systems)))))))
        (for-each (lambda (server)
                    (report-server-coverage server items)
                    (match (assoc-ref opts 'coverage)
                      (#f #f)
                      (threshold
                       (report-package-coverage server packages systems
                                                #:threshold threshold))))
                  urls)))))

;;; Local Variables:
;;; eval: (put 'let/time 'scheme-indent-function 1)
;;; End:
