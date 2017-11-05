;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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
;;; You should have received a copy of thye GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services admin)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages logging)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:export (%default-rotations
            %rotated-files

            log-rotation
            log-rotation?
            log-rotation-frequency
            log-rotation-files
            log-rotation-options
            log-rotation-post-rotate

            rottlog-configuration
            rottlog-configuration?
            rottlog-service
            rottlog-service-type

            <tailon-configuration-file>
            tailon-configuration-file
            tailon-configuration-file?
            tailon-configuration-file-files
            tailon-configuration-file-bind
            tailon-configuration-file-relative-root
            tailon-configuration-file-allow-transfers?
            tailon-configuration-file-follow-names?
            tailon-configuration-file-tail-lines
            tailon-configuration-file-allowed-commands
            tailon-configuration-file-debug?
            tailon-configuration-file-http-auth
            tailon-configuration-file-users

            <tailon-configuration>
            tailon-configuration
            tailon-configuration?
            tailon-configuration-config-file
            tailon-configuration-package

            tailon-service-type))

;;; Commentary:
;;;
;;; This module implements configuration of rottlog by writing
;;; /etc/rottlog/{rc,hourly|daily|weekly}.  Example usage
;;;
;;;     (mcron-service)
;;;     (service rottlog-service-type)
;;;
;;; Code:

(define-record-type* <log-rotation> log-rotation make-log-rotation
  log-rotation?
  (files       log-rotation-files)                ;list of strings
  (frequency   log-rotation-frequency             ;symbol
               (default 'weekly))
  (post-rotate log-rotation-post-rotate           ;#f | gexp
               (default #f))
  (options     log-rotation-options               ;list of strings
               (default '())))

(define %rotated-files
  ;; Syslog files subject to rotation.
  '("/var/log/messages" "/var/log/secure" "/var/log/maillog"))

(define %default-rotations
  (list (log-rotation                             ;syslog files
         (files %rotated-files)

         ;; Restart syslogd after rotation.
         (options '("sharedscripts"))
         (post-rotate #~(let ((pid (call-with-input-file "/var/run/syslog.pid"
                                     read)))
                          (kill pid SIGHUP))))
        (log-rotation
         (files '("/var/log/shepherd.log" "/var/log/guix-daemon.log")))))

(define (log-rotation->config rotation)
  "Return a string-valued gexp representing the rottlog configuration snippet
for ROTATION."
  (define post-rotate
    (let ((post (log-rotation-post-rotate rotation)))
      (and post
           (program-file "rottlog-post-rotate.scm" post))))

  #~(let ((post #$post-rotate))
      (string-append (string-join '#$(log-rotation-files rotation) ",")
                     " {"
                     #$(string-join (log-rotation-options rotation)
                                    "\n  " 'prefix)
                     (if post
                         (string-append "\n  postrotate\n    " post
                                        "\n  endscript\n")
                         "")
                     "\n}\n")))

(define (log-rotations->/etc-entries rotations)
  "Return the list of /etc entries for ROTATIONS, a list of <log-rotation>."
  (define (frequency-file frequency rotations)
    (computed-file (string-append "rottlog." (symbol->string frequency))
                   #~(call-with-output-file #$output
                       (lambda (port)
                         (for-each (lambda (str)
                                     (display str port))
                                   (list #$@(map log-rotation->config
                                                 rotations)))))))

  (let* ((frequencies (delete-duplicates
                       (map log-rotation-frequency rotations)))
         (table       (fold (lambda (rotation table)
                              (vhash-consq (log-rotation-frequency rotation)
                                           rotation table))
                            vlist-null
                            rotations)))
    (map (lambda (frequency)
           `(,(symbol->string frequency)
             ,(frequency-file frequency
                              (vhash-foldq* cons '() frequency table))))
         frequencies)))

(define (default-jobs rottlog)
  (list #~(job '(next-hour '(0))                  ;midnight
               (lambda ()
                 (system* #$(file-append rottlog "/sbin/rottlog"))))
        #~(job '(next-hour '(12))                 ;noon
               (lambda ()
                 (system* #$(file-append rottlog "/sbin/rottlog"))))))

(define-record-type* <rottlog-configuration>
  rottlog-configuration make-rottlog-configuration
  rottlog-configuration?
  (rottlog            rottlog-rottlog             ;package
                      (default rottlog))
  (rc-file            rottlog-rc-file             ;file-like
                      (default (file-append rottlog "/etc/rc")))
  (rotations          rottlog-rotations           ;list of <log-rotation>
                      (default %default-rotations))
  (jobs               rottlog-jobs                ;list of <mcron-job>
                      (default #f)))

(define (rottlog-etc config)
  `(("rottlog"
     ,(file-union "rottlog"
                  (cons `("rc" ,(rottlog-rc-file config))
                        (log-rotations->/etc-entries
                         (rottlog-rotations config)))))))

(define (rottlog-jobs-or-default config)
  (or (rottlog-jobs config)
      (default-jobs (rottlog-rottlog config))))

(define rottlog-service-type
  (service-type
   (name 'rottlog)
   (description
    "Periodically rotate log files using GNU@tie{}Rottlog and GNU@tie{}mcron.
Old log files are removed or compressed according to the configuration.")
   (extensions (list (service-extension etc-service-type rottlog-etc)
                     (service-extension mcron-service-type
                                        rottlog-jobs-or-default)

                     ;; Add Rottlog to the global profile so users can access
                     ;; the documentation.
                     (service-extension profile-service-type
                                        (compose list rottlog-rottlog))))
   (compose concatenate)
   (extend (lambda (config rotations)
             (rottlog-configuration
              (inherit config)
              (rotations (append (rottlog-rotations config)
                                 rotations)))))
   (default-value (rottlog-configuration))))


;;;
;;; Tailon
;;;

(define-record-type* <tailon-configuration-file>
  tailon-configuration-file make-tailon-configuration-file
  tailon-configuration-file?
  (files                   tailon-configuration-file-files
                           (default '("/var/log")))
  (bind                    tailon-configuration-file-bind
                           (default "localhost:8080"))
  (relative-root           tailon-configuration-file-relative-root
                           (default #f))
  (allow-transfers?        tailon-configuration-file-allow-transfers?
                           (default #t))
  (follow-names?           tailon-configuration-file-follow-names?
                           (default #t))
  (tail-lines              tailon-configuration-file-tail-lines
                           (default 200))
  (allowed-commands        tailon-configuration-file-allowed-commands
                           (default '("tail" "grep" "awk")))
  (debug?                  tailon-configuration-file-debug?
                           (default #f))
  (wrap-lines              tailon-configuration-file-wrap-lines
                           (default #t))
  (http-auth               tailon-configuration-file-http-auth
                           (default #f))
  (users                   tailon-configuration-file-users
                           (default #f)))

(define (tailon-configuration-files-string files)
  (string-append
   "\n"
   (string-join
    (map
     (lambda (x)
       (string-append
        "  - "
        (cond
         ((string? x)
          (simple-format #f "'~A'" x))
         ((list? x)
          (string-join
           (cons (simple-format #f "'~A':" (car x))
                 (map
                  (lambda (x) (simple-format #f "      - '~A'" x))
                  (cdr x)))
           "\n"))
         (else (error x)))))
     files)
    "\n")))

(define-gexp-compiler (tailon-configuration-file-compiler
                       (file <tailon-configuration-file>) system target)
  (match file
    (($ <tailon-configuration-file> files bind relative-root
                                    allow-transfers? follow-names?
                                    tail-lines allowed-commands debug?
                                    wrap-lines http-auth users)
     (text-file
      "tailon-config.yaml"
      (string-concatenate
       (filter-map
        (match-lambda
         ((key . #f) #f)
         ((key . value) (string-append key ": " value "\n")))

        `(("files" . ,(tailon-configuration-files-string files))
          ("bind" . ,bind)
          ("relative-root" . ,relative-root)
          ("allow-transfers" . ,(if allow-transfers? "true" "false"))
          ("follow-names" . ,(if follow-names? "true" "false"))
          ("tail-lines" . ,(number->string tail-lines))
          ("commands" . ,(string-append "["
                                        (string-join allowed-commands ", ")
                                        "]"))
          ("debug" . ,(if debug? "true" #f))
          ("wrap-lines" . ,(if wrap-lines "true" "false"))
          ("http-auth" . ,http-auth)
          ("users" . ,(if users
                          (string-concatenate
                           (cons "\n"
                                 (map (match-lambda
                                       ((user . pass)
                                        (string-append
                                         "  " user ":" pass)))
                                      users)))
                          #f)))))))))

(define-record-type* <tailon-configuration>
  tailon-configuration make-tailon-configuration
  tailon-configuration?
  (config-file tailon-configuration-config-file
               (default (tailon-configuration-file)))
  (package tailon-configuration-package
           (default tailon)))

(define tailon-shepherd-service
  (match-lambda
    (($ <tailon-configuration> config-file package)
     (list (shepherd-service
            (provision '(tailon))
            (documentation "Run the tailon daemon.")
            (start #~(make-forkexec-constructor
                      `(,(string-append #$package "/bin/tailon")
                        "-c" ,#$config-file)
                      #:user "tailon"
                      #:group "tailon"))
            (stop #~(make-kill-destructor)))))))

(define %tailon-accounts
  (list (user-group (name "tailon") (system? #t))
        (user-account
         (name "tailon")
         (group "tailon")
         (system? #t)
         (comment "tailon")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define tailon-service-type
  (service-type
   (name 'tailon)
   (description
    "Run Tailon, a Web application for monitoring, viewing, and searching log
files.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailon-shepherd-service)
          (service-extension account-service-type
                             (const %tailon-accounts))))
   (compose concatenate)
   (extend (lambda (parameter files)
             (tailon-configuration
              (inherit parameter)
              (config-file
               (let ((old-config-file
                      (tailon-configuration-config-file parameter)))
                 (tailon-configuration-file
                  (inherit old-config-file)
                  (files (append (tailon-configuration-file-files old-config-file)
                                 files))))))))
   (default-value (tailon-configuration))))

;;; admin.scm ends here
