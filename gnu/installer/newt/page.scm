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

(define-module (gnu installer newt page)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (newt)
  #:export (draw-info-page
            draw-connecting-page
            run-input-page
            run-error-page
            run-confirmation-page
            run-listbox-selection-page
            run-scale-page
            run-checkbox-tree-page
            run-file-textbox-page))

;;; Commentary:
;;;
;;; Some helpers around guile-newt to draw or run generic pages. The
;;; difference between 'draw' and 'run' terms comes from newt library. A page
;;; is drawn when the form it contains does not expect any user
;;; interaction. In that case, it is necessary to call (newt-refresh) to force
;;; the page to be displayed. When a form is 'run', it is blocked waiting for
;;; any action from the user (press a button, input some text, ...).
;;;
;;; Code:

(define (draw-info-page text title)
  "Draw an informative page with the given TEXT as content.  Set the title of
this page to TITLE."
  (let* ((text-box
          (make-reflowed-textbox -1 -1 text 40
                                 #:flags FLAG-BORDER))
         (grid (make-grid 1 1))
         (form (make-form)))
    (set-grid-field grid 0 0 GRID-ELEMENT-COMPONENT text-box)
    (add-component-to-form form text-box)
    (make-wrapped-grid-window grid title)
    (draw-form form)
    ;; This call is imperative, otherwise the form won't be displayed. See the
    ;; explanation in the above commentary.
    (newt-refresh)
    form))

(define (draw-connecting-page service-name)
  "Draw a page to indicate a connection in in progress."
  (draw-info-page
   (format #f (G_ "Connecting to ~a, please wait.") service-name)
   (G_ "Connection in progress")))

(define* (run-input-page text title
                         #:key
                         (allow-empty-input? #f)
                         (default-text #f)
                         (input-field-width 40)
                         (input-flags 0))
  "Run a page to prompt user for an input. The given TEXT will be displayed
above the input field. The page title is set to TITLE. Unless
allow-empty-input? is set to #t, an error page will be displayed if the user
enters an empty input.  INPUT-FLAGS is a bitwise-or'd set of flags for the
input box, such as FLAG-PASSWORD."
  (let* ((text-box
          (make-reflowed-textbox -1 -1 text
                                 input-field-width
                                 #:flags FLAG-BORDER))
         (grid (make-grid 1 3))
         (input-entry (make-entry -1 -1 20
                                  #:flags input-flags))
         (ok-button (make-button -1 -1 (G_ "OK")))
         (form (make-form)))

    (when default-text
      (set-entry-text input-entry default-text))

    (set-grid-field grid 0 0 GRID-ELEMENT-COMPONENT text-box)
    (set-grid-field grid 0 1 GRID-ELEMENT-COMPONENT input-entry
                    #:pad-top 1)
    (set-grid-field grid 0 2 GRID-ELEMENT-COMPONENT ok-button
                    #:pad-top 1)

    (add-components-to-form form text-box input-entry ok-button)
    (make-wrapped-grid-window grid title)
    (let ((error-page (lambda ()
                        (run-error-page (G_ "Please enter a non empty input.")
                                        (G_ "Empty input")))))
      (let loop ()
        (receive (exit-reason argument)
            (run-form form)
          (let ((input (entry-value input-entry)))
            (if (and (not allow-empty-input?)
                     (eq? exit-reason 'exit-component)
                     (string=? input ""))
                (begin
                  ;; Display the error page.
                  (error-page)
                  ;; Set the focus back to the input input field.
                  (set-current-component form input-entry)
                  (loop))
                (begin
                  (destroy-form-and-pop form)
                  input))))))))

(define (run-error-page text title)
  "Run a page to inform the user of an error. The page contains the given TEXT
to explain the error and an \"OK\" button to acknowledge the error. The title
of the page is set to TITLE."
  (let* ((text-box
          (make-reflowed-textbox -1 -1 text 40
                                 #:flags FLAG-BORDER))
         (grid (make-grid 1 2))
         (ok-button (make-button -1 -1 "OK"))
         (form (make-form)))

    (set-grid-field grid 0 0 GRID-ELEMENT-COMPONENT text-box)
    (set-grid-field grid 0 1 GRID-ELEMENT-COMPONENT ok-button
                    #:pad-top 1)

    ;; Set the background color to red to indicate something went wrong.
    (newt-set-color COLORSET-ROOT "white" "red")
    (add-components-to-form form text-box ok-button)
    (make-wrapped-grid-window grid title)
    (run-form form)
    ;; Restore the background to its original color.
    (newt-set-color COLORSET-ROOT "white" "blue")
    (destroy-form-and-pop form)))

(define* (run-confirmation-page text title
                                #:key (exit-button-procedure (const #f)))
  "Run a page to inform the user of an error. The page contains the given TEXT
to explain the error and an \"OK\" button to acknowledge the error. The title
of the page is set to TITLE."
  (let* ((text-box
          (make-reflowed-textbox -1 -1 text 40
                                 #:flags FLAG-BORDER))
         (ok-button (make-button -1 -1 (G_ "Continue")))
         (exit-button (make-button -1 -1 (G_ "Exit")))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT text-box
                GRID-ELEMENT-SUBGRID
                (horizontal-stacked-grid
                 GRID-ELEMENT-COMPONENT ok-button
                 GRID-ELEMENT-COMPONENT exit-button)))
         (form (make-form)))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (receive (exit-reason argument)
        (run-form form)
      (dynamic-wind
        (const #t)
        (lambda ()
          (case exit-reason
            ((exit-component)
             (cond
              ((components=? argument ok-button)
               #t)
              ((components=? argument exit-button)
               (exit-button-procedure))))))
        (lambda ()
          (destroy-form-and-pop form))))))

(define* (run-listbox-selection-page #:key
                                     info-text
                                     title
                                     (info-textbox-width 50)
                                     listbox-items
                                     listbox-item->text
                                     (listbox-height 20)
                                     (listbox-default-item #f)
                                     (listbox-allow-multiple? #f)
                                     (sort-listbox-items? #t)
                                     (allow-delete? #f)
                                     (skip-item-procedure?
                                      (const #f))
                                     button-text
                                     (button-callback-procedure
                                      (const #t))
                                     (button2-text #f)
                                     (button2-callback-procedure
                                      (const #t))
                                     (listbox-callback-procedure
                                      identity)
                                     (hotkey-callback-procedure
                                      (const #t)))
  "Run a page asking the user to select an item in a listbox. The page
contains, stacked vertically from the top to the bottom, an informative text
set to INFO-TEXT, a listbox and a button. The listbox will be filled with
LISTBOX-ITEMS converted to text by applying the procedure LISTBOX-ITEM->TEXT
on every item. The selected item from LISTBOX-ITEMS is returned. The button
text is set to BUTTON-TEXT and the procedure BUTTON-CALLBACK-PROCEDURE called
when it is pressed. The procedure LISTBOX-CALLBACK-PROCEDURE is called when an
item from the listbox is selected (by pressing the <ENTER> key).

INFO-TEXTBOX-WIDTH is the width of the textbox where INFO-TEXT will be
displayed. LISTBOX-HEIGHT is the height of the listbox.

If LISTBOX-DEFAULT-ITEM is set to the value of one of the items in
LISTBOX-ITEMS, it will be selected by default. Otherwise, the first element of
the listbox is selected.

If LISTBOX-ALLOW-MULTIPLE? is set to #t, multiple items from the listbox can
be selected (using the <SPACE> key). It that case, a list containing the
selected items will be returned.

If SORT-LISTBOX-ITEMS? is set to #t, the listbox items are sorted using
'string-locale<?' procedure (after being converted to text).

If ALLOW-DELETE? is #t, the form will return if the <DELETE> key is pressed,
otherwise nothing will happen.

Each time the listbox current item changes, call SKIP-ITEM-PROCEDURE? with the
current listbox item as argument. If it returns #t, skip the element and jump
to the next/previous one depending on the previous item, otherwise do
nothing."

  (define (fill-listbox listbox items)
    "Append the given ITEMS to LISTBOX, once they have been converted to text
with LISTBOX-ITEM->TEXT. Each item appended to the LISTBOX is given a key by
newt. Save this key by returning an association list under the form:

	((NEWT-LISTBOX-KEY . ITEM) ...)

where NEWT-LISTBOX-KEY is the key returned by APPEND-ENTRY-TO-LISTBOX, when
ITEM was inserted into LISTBOX."
    (map (lambda (item)
           (let* ((text (listbox-item->text item))
                  (key (append-entry-to-listbox listbox text)))
             (cons key item)))
         items))

  (define (sort-listbox-items listbox-items)
    "Return LISTBOX-ITEMS sorted using the 'string-locale<?' procedure on the text
corresponding to each item in the list."
    (let* ((items (map (lambda (item)
                         (cons item (listbox-item->text item)))
                       listbox-items))
           (sorted-items
            (sort items (lambda (a b)
                          (let ((text-a (cdr a))
                                (text-b (cdr b)))
                            (string-locale<? text-a text-b))))))
      (map car sorted-items)))

  ;; Store the last selected listbox item's key.
  (define last-listbox-key (make-parameter #f))

  (define (previous-key keys key)
    (let ((index (list-index (cut eq? key <>) keys)))
      (and index
           (> index 0)
           (list-ref keys (- index 1)))))

  (define (next-key keys key)
    (let ((index (list-index (cut eq? key <>) keys)))
      (and index
           (< index (- (length keys) 1))
           (list-ref keys (+ index 1)))))

  (define (set-default-item listbox listbox-keys default-item)
    "Set the default item of LISTBOX to DEFAULT-ITEM. LISTBOX-KEYS is the
association list returned by the FILL-LISTBOX procedure. It is used because
the current listbox item has to be selected by key."
    (for-each (match-lambda
                ((key . item)
                 (when (equal? item default-item)
                   (set-current-listbox-entry-by-key listbox key))))
              listbox-keys))

  (let* ((listbox (make-listbox
                   -1 -1
                   listbox-height
                   (logior FLAG-SCROLL FLAG-BORDER FLAG-RETURNEXIT
                           (if listbox-allow-multiple?
                               FLAG-MULTIPLE
                               0))))
         (form (make-form))
         (info-textbox
          (make-reflowed-textbox -1 -1 info-text
                                 info-textbox-width
                                 #:flags FLAG-BORDER))
         (button (make-button -1 -1 button-text))
         (button2 (and button2-text
                       (make-button -1 -1 button2-text)))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT listbox
                GRID-ELEMENT-SUBGRID
                (apply
                 horizontal-stacked-grid
                 GRID-ELEMENT-COMPONENT button
                 `(,@(if button2
                         (list GRID-ELEMENT-COMPONENT button2)
                         '())))))
         (sorted-items (if sort-listbox-items?
                           (sort-listbox-items listbox-items)
                           listbox-items))
         (keys (fill-listbox listbox sorted-items)))

    ;; On every listbox element change, check if we need to skip it. If yes,
    ;; depending on the 'last-listbox-key', jump forward or backward. If no,
    ;; do nothing.
    (add-component-callback
     listbox
     (lambda (component)
       (let* ((current-key (current-listbox-entry listbox))
              (listbox-keys (map car keys))
              (last-key (last-listbox-key))
              (item (assoc-ref keys current-key))
              (prev-key (previous-key listbox-keys current-key))
              (next-key (next-key listbox-keys current-key)))
         ;; Update last-listbox-key before a potential call to
         ;; set-current-listbox-entry-by-key, because it will immediately
         ;; cause this callback to be called for the new entry.
         (last-listbox-key current-key)
         (when (skip-item-procedure? item)
           (when (eq? prev-key last-key)
             (if next-key
                 (set-current-listbox-entry-by-key listbox next-key)
                 (set-current-listbox-entry-by-key listbox prev-key)))
           (when (eq? next-key last-key)
             (if prev-key
                 (set-current-listbox-entry-by-key listbox prev-key)
                 (set-current-listbox-entry-by-key listbox next-key)))))))

    (when listbox-default-item
      (set-default-item listbox keys listbox-default-item))

    (when allow-delete?
      (form-add-hotkey form KEY-DELETE))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (receive (exit-reason argument)
        (run-form form)
      (dynamic-wind
        (const #t)
        (lambda ()
          (case exit-reason
            ((exit-component)
             (cond
              ((components=? argument button)
               (button-callback-procedure))
              ((and button2
                    (components=? argument button2))
               (button2-callback-procedure))
              ((components=? argument listbox)
               (if listbox-allow-multiple?
                   (let* ((entries (listbox-selection listbox))
                          (items (map (lambda (entry)
                                        (assoc-ref keys entry))
                                      entries)))
                     (listbox-callback-procedure items))
                   (let* ((entry (current-listbox-entry listbox))
                          (item (assoc-ref keys entry)))
                     (listbox-callback-procedure item))))))
            ((exit-hotkey)
             (let* ((entry (current-listbox-entry listbox))
                    (item (assoc-ref keys entry)))
               (hotkey-callback-procedure argument item)))))
        (lambda ()
          (destroy-form-and-pop form))))))

(define* (run-scale-page #:key
                         title
                         info-text
                         (info-textbox-width 50)
                         (scale-width 40)
                         (scale-full-value 100)
                         scale-update-proc
                         (max-scale-update 5))
  "Run a page with a progress bar (called 'scale' in newt). The given
INFO-TEXT is displayed in a textbox above the scale. The width of the textbox
is set to INFO-TEXTBOX-WIDTH. The width of the scale is set to
SCALE-WIDTH. SCALE-FULL-VALUE indicates the value that correspond to 100% of
the scale.

The procedure SCALE-UPDATE-PROC shall return a new scale
value. SCALE-UPDATE-PROC will be called until the returned value is superior
or equal to SCALE-FULL-VALUE, but no more than MAX-SCALE-UPDATE times. An
error is raised if the MAX-SCALE-UPDATE limit is reached."
  (let* ((info-textbox
          (make-reflowed-textbox -1 -1 info-text
                                 info-textbox-width
                                 #:flags FLAG-BORDER))
         (scale (make-scale -1 -1 scale-width scale-full-value))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT scale))
         (form (make-form)))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (draw-form form)
    ;; This call is imperative, otherwise the form won't be displayed. See the
    ;; explanation in the above commentary.
    (newt-refresh)

    (dynamic-wind
      (const #t)
      (lambda ()
        (let loop ((i max-scale-update)
                   (last-value 0))
          (let ((value (scale-update-proc last-value)))
            (set-scale-value scale value)
            ;; Same as above.
            (newt-refresh)
            (unless (>= value scale-full-value)
              (if (> i 0)
                  (loop (- i 1) value)
                  (error "Max scale updates reached."))))))
      (lambda ()
        (destroy-form-and-pop form)))))

(define %none-selected
  (circular-list #f))

(define* (run-checkbox-tree-page #:key
                                 info-text
                                 title
                                 items
                                 (selection %none-selected)
                                 item->text
                                 (info-textbox-width 50)
                                 (checkbox-tree-height 10)
                                 (ok-button-callback-procedure
                                  (const #t))
                                 (exit-button-callback-procedure
                                  (const #t)))
  "Run a page allowing the user to select one or multiple items among ITEMS in
a checkbox list. The page contains vertically stacked from the top to the
bottom, an informative text set to INFO-TEXT, the checkbox list and two
buttons, 'Ok' and 'Exit'. The page title's is set to TITLE. ITEMS are
converted to text using ITEM->TEXT before being displayed in the checkbox
list.  SELECTION is a list of Booleans of the same length as ITEMS that
specifies which items are initially checked.

INFO-TEXTBOX-WIDTH is the width of the textbox where INFO-TEXT will be
displayed. CHECKBOX-TREE-HEIGHT is the height of the checkbox list.

OK-BUTTON-CALLBACK-PROCEDURE is called when the 'Ok' button is pressed.
EXIT-BUTTON-CALLBACK-PROCEDURE is called when the 'Exit' button is
pressed.

This procedure returns the list of checked items in the checkbox list among
ITEMS when 'Ok' is pressed."
  (define (fill-checkbox-tree checkbox-tree items)
    (map (lambda (item selected?)
           (let* ((item-text (item->text item))
                  (key (add-entry-to-checkboxtree checkbox-tree item-text
                                                  (if selected?
                                                      FLAG-SELECTED
                                                      0))))
             (cons key item)))
         items
         selection))

  (let* ((checkbox-tree
          (make-checkboxtree -1 -1
                             checkbox-tree-height
                             FLAG-BORDER))
         (info-textbox
          (make-reflowed-textbox -1 -1 info-text
                                 info-textbox-width
                                 #:flags FLAG-BORDER))
         (ok-button (make-button -1 -1 (G_ "OK")))
         (exit-button (make-button -1 -1 (G_ "Exit")))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT checkbox-tree
                GRID-ELEMENT-SUBGRID
                (horizontal-stacked-grid
                 GRID-ELEMENT-COMPONENT ok-button
                 GRID-ELEMENT-COMPONENT exit-button)))
         (keys (fill-checkbox-tree checkbox-tree items))
         (form (make-form)))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (receive (exit-reason argument)
        (run-form form)
      (dynamic-wind
        (const #t)
        (lambda ()
          (case exit-reason
            ((exit-component)
             (cond
              ((components=? argument ok-button)
               (let* ((entries (current-checkbox-selection checkbox-tree))
                      (current-items (map (lambda (entry)
                                            (assoc-ref keys entry))
                                          entries)))
                 (ok-button-callback-procedure)
                 current-items))
              ((components=? argument exit-button)
               (exit-button-callback-procedure))))))
        (lambda ()
          (destroy-form-and-pop form))))))

(define* (run-file-textbox-page #:key
                                info-text
                                title
                                file
                                (info-textbox-width 50)
                                (file-textbox-width 50)
                                (file-textbox-height 30)
                                (exit-button? #t)
                                (ok-button-callback-procedure
                                 (const #t))
                                (exit-button-callback-procedure
                                 (const #t)))
  (let* ((info-textbox
          (make-reflowed-textbox -1 -1 info-text
                                 info-textbox-width
                                 #:flags FLAG-BORDER))
         (file-text (read-all file))
         (file-textbox
          (make-textbox -1 -1
                        file-textbox-width
                        file-textbox-height
                        (logior FLAG-SCROLL FLAG-BORDER)))
         (ok-button (make-button -1 -1 (G_ "OK")))
         (exit-button (make-button -1 -1 (G_ "Exit")))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT file-textbox
                GRID-ELEMENT-SUBGRID
                (apply
                 horizontal-stacked-grid
                 GRID-ELEMENT-COMPONENT ok-button
                 `(,@(if exit-button?
                         (list GRID-ELEMENT-COMPONENT exit-button)
                         '())))))
         (form (make-form)))

    (set-textbox-text file-textbox file-text)
    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (receive (exit-reason argument)
        (run-form form)
      (dynamic-wind
        (const #t)
        (lambda ()
          (case exit-reason
            ((exit-component)
             (cond
              ((components=? argument ok-button)
               (ok-button-callback-procedure))
              ((and exit-button?
                    (components=? argument exit-button))
               (exit-button-callback-procedure))))))
        (lambda ()
          (destroy-form-and-pop form))))))
