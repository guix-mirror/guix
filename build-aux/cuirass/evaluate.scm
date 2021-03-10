;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

;;; This program replicates the behavior of Cuirass's 'evaluate' process.
;;; It displays the evaluated jobs on the standard output.

(use-modules (guix channels)
             (guix derivations)
             (guix git-download)
             (guix inferior)
             (guix packages)
             (guix store)
             (guix ui)
             ((guix ui) #:select (build-notifier))
             (ice-9 match)
             (ice-9 threads))

(define %top-srcdir
  (and=> (assq-ref (current-source-location) 'filename)
         (lambda (file)
           (canonicalize-path
            (string-append (dirname file) "/../..")))))

(match (command-line)
  ((command directory)
   (let ((real-build-things build-things))
     (with-store store
       ;; Make sure we don't resort to substitutes.
       (set-build-options store
                          #:use-substitutes? #f
                          #:substitute-urls '())

       ;; The evaluation of Guix itself requires building a "trampoline"
       ;; program, and possibly everything it depends on.  Thus, allow builds
       ;; but print a notification.
       (with-build-handler (build-notifier #:use-substitutes? #f)

         ;; Add %TOP-SRCDIR to the store with a proper Git predicate so we
         ;; work from a clean checkout.
         (let ((source (add-to-store store "guix-source" #t
                                     "sha256" %top-srcdir
                                     #:select? (git-predicate %top-srcdir))))
           (define instances
             (list (checkout->channel-instance source)))

           (define channels
             (map channel-instance-channel instances))

           (define derivation
             ;; Compute the derivation of Guix for COMMIT.
             (run-with-store store
               (channel-instances->derivation instances)))

           ;; TODO: Remove 'show-what-to-build' call when Cuirass' 'evaluate'
           ;; scripts uses 'with-build-handler'.
           (show-what-to-build store (list derivation))
           (build-derivations store (list derivation))


           ;; Evaluate jobs on a per-system basis for two reasons.  It speeds
           ;; up the evaluation speed as the evaluations can be performed
           ;; concurrently.  It also decreases the amount of memory needed per
           ;; evaluation process.
           (n-par-for-each
            (/ (current-processor-count) 2)
            (lambda (system)
              (with-store store
                (let ((inferior
                       (open-inferior (derivation->output-path derivation)))
                      (channels (map channel-instance->sexp instances)))
                  (inferior-eval '(use-modules (gnu ci)) inferior)
                  (let ((jobs
                         (inferior-eval-with-store
                          inferior store
                          `(lambda (store)
                             (cuirass-jobs store
                                           '((subset . all)
                                             (systems . ,(list system))
                                             (channels . ,channels))))))
                        (file
                         (string-append directory "/jobs-" system ".scm")))
                    (call-with-output-file file
                      (lambda (port)
                        (write jobs port)))))))
            %cuirass-supported-systems))))))
  (x
   (format (current-error-port) "Wrong command: ~a~%." x)
   (exit 1)))
