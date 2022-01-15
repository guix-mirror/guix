;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt)
  #:use-module (gnu installer record)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer dump)
  #:use-module (gnu installer newt ethernet)
  #:use-module (gnu installer newt final)
  #:use-module (gnu installer newt parameters)
  #:use-module (gnu installer newt hostname)
  #:use-module (gnu installer newt keymap)
  #:use-module (gnu installer newt locale)
  #:use-module (gnu installer newt menu)
  #:use-module (gnu installer newt network)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt partition)
  #:use-module (gnu installer newt services)
  #:use-module (gnu installer newt substitutes)
  #:use-module (gnu installer newt timezone)
  #:use-module (gnu installer newt user)
  #:use-module (gnu installer newt utils)
  #:use-module (gnu installer newt welcome)
  #:use-module (gnu installer newt wifi)
  #:use-module (guix config)
  #:use-module (guix discovery)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (newt)
  #:export (newt-installer))

(define (init)
  (newt-init)
  (clear-screen)
  (set-screen-size!)
  (installer-log-line "Display is ~ax~a." (screen-columns) (screen-rows))
  (push-help-line
   (format #f (G_ "Press <F1> for installation parameters."))))

(define (exit)
  (newt-finish)
  (clear-screen))

(define (exit-error error)
  (newt-set-color COLORSET-ROOT "white" "red")
  (define action
    (run-textbox-page
     #:info-text (G_ "The installer has encountered an unexpected problem. \
The backtrace is displayed below. You may choose to exit or create a dump \
archive.")
     #:title (G_ "Unexpected problem")
     #:content error
     #:buttons-spec
     (list
      (cons (G_ "Dump") (const 'dump))
      (cons (G_ "Exit") (const 'exit)))))
  (newt-set-color COLORSET-ROOT "white" "blue")
  action)

(define (report-page dump-archive)
  (define text
    (format #f (G_ "The dump archive was created as ~a.  Would you like to \
send this archive to the Guix servers?") dump-archive))
  (define title (G_ "Dump archive created"))
  (when (run-confirmation-page text title)
    (let* ((uploaded-name (send-dump-report dump-archive))
           (text (if uploaded-name
                     (format #f (G_ "The dump was uploaded as ~a.  Please \
report it by email to ~a.") uploaded-name %guix-bug-report-address)
                     (G_ "The dump could not be uploaded."))))
      (run-error-page
       text
       (G_ "Dump upload result")))))

(define (dump-page dump-dir)
  (define files
    (scandir dump-dir (lambda (x)
                        (not (or (string=? x ".")
                                 (string=? x ".."))))))
  (fold (match-lambda*
          (((file . enable?) acc)
           (if enable?
               (cons file acc)
               acc)))
        '()
        (run-dump-page
         dump-dir
         (map (lambda (x)
                (cons x #f))
              files))))

(define (newt-run-command . args)
  (define command-output "")
  (define (line-accumulator line)
    (set! command-output
          (string-append/shared command-output line "\n")))
  (define displayed-command
    (string-join
     (map (lambda (s) (string-append "\"" s "\"")) args)
     " "))
  (define result (run-external-command-with-line-hooks (list line-accumulator)
                                                       args))
  (define exit-val (status:exit-val result))
  (define term-sig (status:term-sig result))
  (define stop-sig (status:stop-sig result))

  (if (and exit-val (zero? exit-val))
      #t
      (let ((info-text
             (cond
              (exit-val
               (format #f (G_ "External command ~s exited with code ~a")
                       args exit-val))
              (term-sig
               (format #f (G_ "External command ~s terminated by signal ~a")
                       args term-sig))
              (stop-sig
               (format #f (G_ "External command ~s stopped by signal ~a")
                       args stop-sig)))))
        (run-textbox-page #:title (G_ "External command error")
                          #:info-text info-text
                          #:content command-output
                          #:buttons-spec
                          (list
                           (cons "Ignore" (const #t))
                           (cons "Abort"
                                 (lambda ()
                                   (abort-to-prompt 'installer-step 'abort)))
                           (cons "Report"
                                 (lambda ()
                                   (raise
                                    (condition
                                     ((@@ (guix build utils)
                                          &invoke-error)
                                      (program (car args))
                                      (arguments (cdr args))
                                      (exit-status exit-val)
                                      (term-signal term-sig)
                                      (stop-signal stop-sig)))))))))))

(define (final-page result prev-steps)
  (run-final-page result prev-steps))

(define* (locale-page #:key
                      supported-locales
                      iso639-languages
                      iso3166-territories)
  (run-locale-page
   #:supported-locales supported-locales
   #:iso639-languages iso639-languages
   #:iso3166-territories iso3166-territories))

(define (timezone-page zonetab)
  (run-timezone-page zonetab))

(define (welcome-page logo)
  (run-welcome-page logo))

(define (menu-page steps)
  (run-menu-page steps))

(define* (keymap-page layouts context)
  (run-keymap-page layouts #:context context))

(define (network-page)
  (run-network-page))

(define (substitutes-page)
  (run-substitutes-page))

(define (hostname-page)
  (run-hostname-page))

(define (user-page)
  (run-user-page))

(define (partition-page)
  (run-partitioning-page))

(define (services-page)
  (run-services-page))

(define (parameters-menu menu-proc)
  (newt-set-help-callback menu-proc))

(define (parameters-page keyboard-layout-selection)
  (run-parameters-page keyboard-layout-selection))

(define newt-installer
  (installer
   (name 'newt)
   (init init)
   (exit exit)
   (exit-error exit-error)
   (final-page final-page)
   (keymap-page keymap-page)
   (locale-page locale-page)
   (menu-page menu-page)
   (network-page network-page)
   (substitutes-page substitutes-page)
   (timezone-page timezone-page)
   (hostname-page hostname-page)
   (user-page user-page)
   (partition-page partition-page)
   (services-page services-page)
   (welcome-page welcome-page)
   (parameters-menu parameters-menu)
   (parameters-page parameters-page)
   (dump-page dump-page)
   (run-command newt-run-command)
   (report-page report-page)))
