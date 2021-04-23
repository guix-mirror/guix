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
            start-installation
            complete-installation

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
                             (string-contains service "DHCP")))
                          (choose-misc-service?
                           (lambda (service)
                             (string-contains service "NTP")))
                          (choose-other-service? (const #f)))

  "Converse over PORT to choose services."
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
     (find choose-network-management-tool? services))

    ((checkbox-list (title "Console services") (text _)
                    (items ,services))
     (null? desktop-environments)
     (filter choose-misc-service? services))

    ((checkbox-list (title "Printing and document services") (text _)
                    (items ,services))
     (filter choose-other-service? services))))

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
                              (uefi-support? #f)
                              (passphrase "thepassphrase")
                              (edit-configuration-file
                               edit-configuration-file))
  "Converse over PORT to choose the partitioning method.  When ENCRYPTED? is
true, choose full-disk encryption with PASSPHRASE as the LUKS passphrase.

When UEFI-SUPPORT? is true, assume that we are running the installation tests
on an UEFI capable machine.

This conversation stops when the user partitions have been formatted, right
before the installer generates the configuration file and shows it in a dialog
box. "
  (converse port
    ((list-selection (title "Partitioning method")
                     (multiple-choices? #f)
                     (items (,not-encrypted ,encrypted _ ...)))
     (if encrypted?
         encrypted
         not-encrypted))
    ((list-selection (title "Disk") (multiple-choices? #f)
                     (items (,disks ...)))
     ;; When running the installation from an ISO image, the CD/DVD drive
     ;; shows up in the list.  Avoid it.
     (find (lambda (disk)
             (not (or (string-contains disk "DVD")
                      (string-contains disk "CD-ROM"))))
           disks))

    ;; The "Partition table" dialog pops up only if there's not already a
    ;; partition table and if the system does not support UEFI.
    ((list-selection (title "Partition table")
                     (multiple-choices? #f)
                     (items _))
     ;; When UEFI is supported, the partition is forced to GPT by the
     ;; installer.
     (not uefi-support?)
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
    ((starting-final-step)
     ;; Do not return anything.  The reply will be sent by
     ;; 'conclude-installation' and in the meantime the installer just waits
     ;; for us, giving us a chance to do things such as changing partition
     ;; UUIDs before it generates the configuration file.
     (values))))

(define (start-installation port)
  "Start the installation by checking over PORT that we get the generated
configuration file, accepting it and starting the installation, and then
receiving the pause message once the 'guix system init' process has
completed."
  ;; Assume the previous message received was 'starting-final-step'; here we
  ;; send the reply to that message, which lets the installer continue.
  (write #t port)
  (newline port)
  (force-output port)

  (converse port
    ((file-dialog (title "Configuration file")
                  (text _)
                  (file ,configuration-file))
     (edit-configuration-file configuration-file))
    ((pause)                                      ;"Press Enter to continue."
     (values))))

(define (complete-installation port)
  "Complete the installation by replying to the installer pause message and
waiting for the installation-complete message."
  ;; Assume the previous message received was 'pause'; here we send the reply
  ;; to that message, which lets the installer continue.
  (write #t port)
  (newline port)
  (force-output port)

  (converse port
    ((installation-complete)
     (values))))

;;; Local Variables:
;;; eval: (put 'converse 'scheme-indent-function 1)
;;; eval: (put 'with-ellipsis 'scheme-indent-function 1)
;;; End:
