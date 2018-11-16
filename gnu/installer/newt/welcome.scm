;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu installer newt welcome)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt utils)
  #:use-module (guix build syscalls)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:export (run-welcome-page))

;; Margin between screen border and newt root window.
(define margin-left (make-parameter 3))
(define margin-top (make-parameter 3))

;; Expected width and height for the logo.
(define logo-width (make-parameter 50))
(define logo-height (make-parameter 23))

(define (nearest-exact-integer x)
  "Given a real number X, return the nearest exact integer, with ties going to
the nearest exact even integer."
  (inexact->exact (round x)))

(define* (run-menu-page title logo
                        #:key
                        listbox-items
                        listbox-item->text)
  "Run a page with the given TITLE, to ask the user to choose between
LISTBOX-ITEMS displayed in a listbox. The listbox items are converted to text
using LISTBOX-ITEM->TEXT procedure. Display the textual LOGO in the center of
the page. Contrary to other pages, we cannot resort to grid layouts, because
we want this page to occupy all the screen space available."
  (define (fill-listbox listbox items)
    (map (lambda (item)
           (let* ((text (listbox-item->text item))
                  (key (append-entry-to-listbox listbox text)))
             (cons key item)))
         items))

  (let* ((windows
          (make-window (margin-left)
                       (margin-top)
                       (- (screen-columns) (* 2 (margin-left)))
                       (- (screen-rows) (* 2 (margin-top)))
                       title))
         (logo-textbox
          (make-textbox (nearest-exact-integer
                         (- (/ (screen-columns) 2)
                            (+ (/ (logo-width) 2) (margin-left))))
                        (margin-top) (logo-width) (logo-height) 0))
         (text (set-textbox-text logo-textbox
                                 (read-all logo)))
         (options-listbox
          (make-listbox (margin-left)
                        (+ (logo-height) (margin-top))
                        (- (screen-rows) (+ (logo-height)
                                            (* (margin-top) 4)))
                        (logior FLAG-BORDER FLAG-RETURNEXIT)))
         (keys (fill-listbox options-listbox listbox-items))
         (form (make-form)))
    (set-listbox-width options-listbox (- (screen-columns)
                                          (* (margin-left) 4)))
    (add-components-to-form form logo-textbox options-listbox)

    (receive (exit-reason argument)
        (run-form form)
      (dynamic-wind
        (const #t)
        (lambda ()
          (when (eq? exit-reason 'exit-component)
            (cond
             ((components=? argument options-listbox)
              (let* ((entry (current-listbox-entry options-listbox))
                     (item (assoc-ref keys entry)))
                (match item
                  ((text . proc)
                   (proc))))))))
        (lambda ()
          (destroy-form-and-pop form))))))

(define (run-welcome-page logo)
  "Run a welcome page with the given textual LOGO displayed at the center of
the page. Ask the user to choose between manual installation, graphical
installation and reboot."
  (run-menu-page
   (G_ "GNU GuixSD install")
   logo
   #:listbox-items
   `((,(G_ "Install using the unguided shell based process")
      .
      ,(lambda ()
         (clear-screen)
         (newt-suspend)
         (system* "bash" "-l")
         (newt-resume)))
     (,(G_ "Graphical install using a guided terminal based interface")
      .
      ,(const #t))
     (,(G_ "Reboot")
      .
      ,(lambda ()
         (newt-finish)
         (reboot))))
   #:listbox-item->text car))
