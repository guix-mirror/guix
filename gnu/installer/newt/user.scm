;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu installer newt user)
  #:use-module (gnu installer user)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (newt)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (run-user-page))

(define (run-user-add-page)
  (define (pad-label label)
    (string-pad-right label 20))

  (let* ((label-name
          (make-label -1 -1 (pad-label (G_ "Name"))))
         (label-home-directory
          (make-label -1 -1 (pad-label (G_ "Home directory"))))
         (label-password
          (make-label -1 -1 (pad-label (G_ "Password"))))
         (entry-width 30)
         (entry-name (make-entry -1 -1 entry-width))
         (entry-home-directory (make-entry -1 -1 entry-width))
         (entry-password (make-entry -1 -1 entry-width
                                     #:flags FLAG-PASSWORD))
         (entry-grid (make-grid 3 4))
         (button-grid (make-grid 1 1))
         (ok-button (make-button -1 -1 (G_ "OK")))
         (grid (make-grid 1 2))
         (title (G_ "User creation"))
         (set-entry-grid-field
          (cut set-grid-field entry-grid <> <> GRID-ELEMENT-COMPONENT <>))
         (form (make-form)))

    (set-entry-grid-field 0 0 label-name)
    (set-entry-grid-field 1 0 entry-name)
    (set-entry-grid-field 0 1 label-home-directory)
    (set-entry-grid-field 1 1 entry-home-directory)
    (set-entry-grid-field 0 2 label-password)
    (set-entry-grid-field 1 2 entry-password)

    (set-grid-field button-grid 0 0 GRID-ELEMENT-COMPONENT ok-button)

    (add-component-callback
     entry-name
     (lambda (component)
       (set-entry-text entry-home-directory
                       (string-append "/home/" (entry-value entry-name)))))

    (add-components-to-form form
                            label-name label-home-directory label-password
                            entry-name entry-home-directory entry-password
                            ok-button)

    (make-wrapped-grid-window (vertically-stacked-grid
                               GRID-ELEMENT-SUBGRID entry-grid
                               GRID-ELEMENT-SUBGRID button-grid)
                              title)
    (let ((error-page
           (lambda ()
             (run-error-page (G_ "Empty inputs are not allowed.")
                             (G_ "Empty input")))))
      (receive (exit-reason argument)
          (run-form form)
        (dynamic-wind
          (const #t)
          (lambda ()
            (when (eq? exit-reason 'exit-component)
              (cond
               ((components=? argument ok-button)
                (let ((name           (entry-value entry-name))
                      (home-directory (entry-value entry-home-directory))
                      (password       (entry-value entry-password)))
                  (if (or (string=? name "")
                          (string=? home-directory ""))
                      (begin
                        (error-page)
                        (run-user-add-page))
                      (user
                       (name name)
                       (home-directory home-directory)
                       (password password))))))))
          (lambda ()
            (destroy-form-and-pop form)))))))

(define (run-root-password-page)
  ;; TRANSLATORS: Leave "root" untranslated: it refers to the name of the
  ;; system administrator account.
  (run-input-page (G_ "Please choose a password for the system \
administrator (\"root\").")
                  (G_ "System administrator password")
                  #:input-flags FLAG-PASSWORD))

(define (run-user-page)
  (define (run users)
    (let* ((listbox (make-listbox
                     -1 -1 10
                     (logior FLAG-SCROLL FLAG-BORDER)))
           (info-textbox
            (make-reflowed-textbox
             -1 -1
             (G_ "Please add at least one user to system\
 using the 'Add' button.")
             40 #:flags FLAG-BORDER))
           (add-button (make-compact-button -1 -1 (G_ "Add")))
           (del-button (make-compact-button -1 -1 (G_ "Delete")))
           (listbox-button-grid
            (apply
             vertically-stacked-grid
             GRID-ELEMENT-COMPONENT add-button
             `(,@(if (null? users)
                     '()
                     (list GRID-ELEMENT-COMPONENT del-button)))))
           (ok-button (make-button -1 -1 (G_ "OK")))
           (exit-button (make-button -1 -1 (G_ "Exit")))
           (title "User creation")
           (grid
            (vertically-stacked-grid
             GRID-ELEMENT-COMPONENT info-textbox
             GRID-ELEMENT-SUBGRID (horizontal-stacked-grid
                                   GRID-ELEMENT-COMPONENT listbox
                                   GRID-ELEMENT-SUBGRID listbox-button-grid)
             GRID-ELEMENT-SUBGRID (horizontal-stacked-grid
                                   GRID-ELEMENT-COMPONENT ok-button
                                   GRID-ELEMENT-COMPONENT exit-button)))
           (sorted-users (sort users (lambda (a b)
                                       (string<= (user-name a)
                                                 (user-name b)))))
           (listbox-elements
            (map
             (lambda (user)
               `((key . ,(append-entry-to-listbox listbox
                                                  (user-name user)))
                 (user . ,user)))
             sorted-users))
           (form (make-form)))


      (add-form-to-grid grid form #t)
      (make-wrapped-grid-window grid title)
      (if (null? users)
          (set-current-component form add-button)
          (set-current-component form ok-button))

      (receive (exit-reason argument)
          (run-form form)
        (dynamic-wind
          (const #t)
          (lambda ()
            (when (eq? exit-reason 'exit-component)
              (cond
               ((components=? argument add-button)
                (run (cons (run-user-add-page) users)))
               ((components=? argument del-button)
                (let* ((current-user-key (current-listbox-entry listbox))
                       (users
                        (map (cut assoc-ref <> 'user)
                             (remove (lambda (element)
                                       (equal? (assoc-ref element 'key)
                                               current-user-key))
                                     listbox-elements))))
                  (run users)))
               ((components=? argument ok-button)
                (when (null? users)
                  (run-error-page (G_ "Please create at least one user.")
                                  (G_ "No user"))
                  (run users))
                (reverse users)))))
          (lambda ()
            (destroy-form-and-pop form))))))

  ;; Add a "root" user simply to convey the root password.
  (cons (user (name "root")
              (home-directory "/root")
              (password (run-root-password-page)))
        (run '())))
