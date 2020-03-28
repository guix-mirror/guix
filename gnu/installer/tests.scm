;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:export (&pattern-not-matched
            pattern-not-matched?

            %installer-socket-file
            open-installer-socket

            converse
            conversation-log-port

            choose-locale+keyboard
            enter-host-name+passwords
            choose-services
            choose-partitioning
            conclude-installation

            edit-configuration-file))

;;; Commentary:
;;;
;;; This module provides tools to test the guided "graphical" installer in a
;;; non-interactive fashion.  The core of it is 'converse': it allows you to
;;; state Expect-style dialogues, which happen over the Unix-domain socket the
;;; installer listens to.  Higher-level procedures such as
;;; 'choose-locale+keyboard' are provided to perform specific parts of the
;;; dialogue.
;;;
;;; Code:

(define %installer-socket-file
  ;; Socket the installer listens to.
  "/var/guix/installer-socket")

(define* (open-installer-socket #:optional (file %installer-socket-file))
  "Return a socket connected to the installer."
  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
    (connect sock AF_UNIX file)
    sock))

(define-condition-type &pattern-not-matched &error
  pattern-not-matched?
  (pattern pattern-not-matched-pattern)
  (sexp    pattern-not-matched-sexp))

(define (pattern-error pattern sexp)
  (raise (condition
          (&pattern-not-matched
           (pattern pattern) (sexp sexp)))))

(define conversation-log-port
  ;; Port where debugging info is logged
  (make-parameter (current-error-port)))

(define (converse-debug pattern)
  (format (conversation-log-port)
          "conversation expecting pattern ~s~%"
          pattern))

(define-syntax converse
  (lambda (s)
    "Convert over PORT: read sexps from there, match them against each
PATTERN, and send the corresponding REPLY.  Raise to '&pattern-not-matched'
when one of the PATTERNs is not matched."

    ;; XXX: Strings that appear in PATTERNs must be in the language the
    ;; installer is running in.  In the future, we should add support to allow
    ;; writing English strings in PATTERNs and have the pattern matcher
    ;; automatically translate them.

    ;; Here we emulate 'pmatch' syntax on top of 'match'.  This is ridiculous
    ;; but that's because 'pmatch' compares objects with 'eq?', making it
    ;; pretty useless, and it doesn't support ellipses and such.

    (define (quote-pattern s)
      ;; Rewrite the pattern S from pmatch style (a ,b) to match style like
      ;; ('a b).
      (with-ellipsis :::
        (syntax-case s (unquote _ ...)
          ((unquote id) #'id)
          (_ #'_)
          (... #'...)
          (id
           (identifier? #'id)
           #''id)
          ((lst :::) (map quote-pattern #'(lst :::)))
          (pattern #'pattern))))

    (define (match-pattern s)
      ;; Match one pattern without a guard.
      (syntax-case s ()
        ((port (pattern reply) continuation)
         (with-syntax ((pattern (quote-pattern #'pattern)))
           #'(let ((pat 'pattern))
               (converse-debug pat)
               (match (read port)
                 (pattern
                  (let ((data (call-with-values (lambda () reply)
                                list)))
                    (for-each (lambda (obj)
                                (write obj port)
                                (newline port))
                              data)
                    (force-output port)
                    (continuation port)))
                 (sexp
                  (pattern-error pat sexp))))))))

    (syntax-case s ()
      ((_ port (pattern reply) rest ...)
       (match-pattern #'(port (pattern reply)
                              (lambda (port)
                                (converse port rest ...)))))
      ((_ port (pattern guard reply) rest ...)
       #`(let ((skip? (not guard))
               (next  (lambda (p)
                        (converse p rest ...))))
           (if skip?
               (next port)
               #,(match-pattern #'(port (pattern reply) next)))))
      ((_ port)
       #t))))

(define* (choose-locale+keyboard port
                                 #:key
                                 (language "English")
                                 (location "Hong Kong")
                                 (timezone '("Europe" "Zagreb"))
                                 (keyboard
                                  '("English (US)"
                                    "English (intl., with AltGr dead keys)")))
  "Converse over PORT with the guided installer to choose the specified
LANGUAGE, LOCATION, TIMEZONE, and KEYBOARD."
  (converse port
    ((list-selection (title "Locale language")
                     (multiple-choices? #f)
                     (items _))
     language)
    ((list-selection (title "Locale location")
                     (multiple-choices? #f)
                     (items _))
     location)
    ((menu (title "GNU Guix install")
           (text _)
           (items (,guided _ ...)))           ;"Guided graphical installation"
     guided)
    ((list-selection (title "Timezone")
                     (multiple-choices? #f)
                     (items _))
     (first timezone))
    ((list-selection (title "Timezone")
                     (multiple-choices? #f)
                     (items _))
     (second timezone))
    ((list-selection (title "Layout")
                     (multiple-choices? #f)
                     (items _))
     (first keyboard))
    ((list-selection (title "Variant")
                     (multiple-choices? #f)
                     (items _))
     (second keyboard))))

(define* (enter-host-name+passwords port
                                    #:key
                                    (host-name "guix")
                                    (root-password "foo")
                                    (users '(("alice" "pass1")
                                             ("bob" "pass2")
                                             ("charlie" "pass3"))))
  "Converse over PORT with the guided installer to choose HOST-NAME,
ROOT-PASSWORD, and USERS."
  (converse port
    ((input (title "Hostname") (text _) (default _))
     host-name)
    ((input (title "System administrator password") (text _) (default _))
     root-password)
    ((input (title "Password confirmation required") (text _) (default _))
     root-password)
    ((add-users)
     (match users
       (((names passwords) ...)
        (map (lambda (name password)
               `(user (name ,name) (real-name ,(string-titlecase name))
                      (home-directory ,(string-append "/home/" name))
                      (password ,password)))
             names passwords))))))

(define* (choose-services port
                          #:key
                          (choose-desktop-environment? (const #f))
                          (choose-network-service?
                           (lambda (service)
                             (or (string-contains service "SSH")
                                 (string-contains service "NSS"))))
                          (choose-network-management-tool?
                           (lambda (service)
                             (string-contains service "DHCP"))))
  "Converse over PORT to choose networking services."
  (define desktop-environments '())

  (converse port
    ((checkbox-list (title "Desktop environment") (text _)
                    (items ,services))
     (let ((desktops (filter choose-desktop-environment? services)))
       (set! desktop-environments desktops)
       desktops))
    ((checkbox-list (title "Network service") (text _)
                    (items ,services))
     (filter choose-network-service? services))

    ;; The "Network management" dialog shows up only when no desktop
    ;; environments have been selected, hence the guard.
    ((list-selection (title "Network management")
                     (multiple-choices? #f)
                     (items ,services))
     (null? desktop-environments)
     (find choose-network-management-tool? services))))

(define (edit-configuration-file file)
  "Edit FILE, an operating system configuration file generated by the
installer, by adding a marionette service such that the installed OS is
instrumented for further testing."
  (define (read-expressions port)
    (let loop ((result '()))
      (match (read port)
        ((? eof-object?)
         (reverse result))
        (exp
         (loop (cons exp result))))))

  (define (edit exp)
    (match exp
      (('operating-system _ ...)
       `(marionette-operating-system ,exp
                                     #:imported-modules
                                     '((gnu services herd)
                                       (guix build utils)
                                       (guix combinators))))
      (_
       exp)))

  (let ((content (call-with-input-file file read-expressions)))
    (call-with-output-file file
      (lambda (port)
        (format port "\
;; Operating system configuration edited for automated testing.~%~%")

        (pretty-print '(use-modules (gnu tests)) port)
        (for-each (lambda (exp)
                    (pretty-print (edit exp) port)
                    (newline port))
                  content)))

    #t))

(define* (choose-partitioning port
                              #:key
                              (encrypted? #t)
                              (passphrase "thepassphrase")
                              (edit-configuration-file
                               edit-configuration-file))
  "Converse over PORT to choose the partitioning method.  When ENCRYPTED? is
true, choose full-disk encryption with PASSPHRASE as the LUKS passphrase.
This conversation goes past the final dialog box that shows the configuration
file, actually starting the installation process."
  (converse port
    ((list-selection (title "Partitioning method")
                     (multiple-choices? #f)
                     (items (,not-encrypted ,encrypted _ ...)))
     (if encrypted?
         encrypted
         not-encrypted))
    ((list-selection (title "Disk") (multiple-choices? #f)
                     (items (,disk _ ...)))
     disk)

    ;; The "Partition table" dialog pops up only if there's not already a
    ;; partition table.
    ((list-selection (title "Partition table")
                     (multiple-choices? #f)
                     (items _))
     "gpt")
    ((list-selection (title "Partition scheme")
                     (multiple-choices? #f)
                     (items (,one-partition _ ...)))
     one-partition)
    ((list-selection (title "Guided partitioning")
                     (multiple-choices? #f)
                     (items (,disk _ ...)))
     disk)
    ((input (title "Password required")
            (text _) (default #f))
     encrypted?                                   ;only when ENCRYPTED?
     passphrase)
    ((input (title "Password confirmation required")
            (text _) (default #f))
     encrypted?
     passphrase)
    ((confirmation (title "Format disk?") (text _))
     #t)
    ((info (title "Preparing partitions") _ ...)
     (values))                                    ;nothing to return
    ((file-dialog (title "Configuration file")
                  (text _)
                  (file ,configuration-file))
     (edit-configuration-file configuration-file))))

(define (conclude-installation port)
  "Conclude the installation by checking over PORT that we get the final
messages once the 'guix system init' process has completed."
  (converse port
    ((pause)                                      ;"Press Enter to continue."
     #t)
    ((installation-complete)                      ;congratulations!
     (values))))

;;; Local Variables:
;;; eval: (put 'converse 'scheme-indent-function 1)
;;; eval: (put 'with-ellipsis 'scheme-indent-function 1)
;;; End:
