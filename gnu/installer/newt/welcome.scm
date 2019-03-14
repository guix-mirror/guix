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

;; Expected width and height for the logo.
(define logo-width (make-parameter 43))
(define logo-height (make-parameter 19))

(define info-textbox-width (make-parameter 70))
(define options-listbox-height (make-parameter 5))

(define* (run-menu-page title info-text logo
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

  (let* ((logo-textbox
          (make-textbox -1 -1 (logo-width) (logo-height) 0))
         (info-textbox
          (make-reflowed-textbox -1 -1
                                 info-text
                                 (info-textbox-width)))
         (options-listbox
          (make-listbox -1 -1
                        (options-listbox-height)
                        (logior FLAG-BORDER FLAG-RETURNEXIT)))
         (keys (fill-listbox options-listbox listbox-items))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT logo-textbox
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT options-listbox))
         (form (make-form)))

    (set-textbox-text logo-textbox (read-all logo))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

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
   (G_ "GNU Guix install")
   (G_ "Welcome to GNU Guix system installer!

Please note that the present graphical installer is still under heavy \
development, so you might want to prefer using the shell based process. \
The documentation is accessible at any time by pressing CTRL-ALT-F2.")
   logo
   #:listbox-items
   `((,(G_ "Graphical install using a terminal based interface")
      .
      ,(const #t))
     (,(G_ "Install using the shell based process")
      .
      ,(lambda ()
         ;; Switch to TTY3, where a root shell is available for shell based
         ;; install. The other root TTY's would have been ok too.
         (system* "chvt" "3")
         (run-welcome-page logo)))
     (,(G_ "Reboot")
      .
      ,(lambda ()
         (newt-finish)
         (reboot))))
   #:listbox-item->text car))
