;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
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

(define-module (gnu build svg)
  #:use-module (rsvg)
  #:use-module (cairo)
  #:use-module (srfi srfi-11)
  #:export (svg->png))

(define* (downscaled-surface surface
                             #:key
                             source-width source-height
                             width height)
  "Return a new rendering context where SURFACE is scaled to WIDTH x HEIGHT."
  (let ((cr (cairo-create (cairo-image-surface-create 'argb32
                                                      width height))))
    (cairo-scale cr (/ width source-width) (/ height source-height))
    (cairo-set-source-surface cr surface 0 0)
    (cairo-pattern-set-filter (cairo-get-source cr) 'best)
    (cairo-rectangle cr 0 0 source-width source-height)
    (cairo-fill cr)
    cr))

(define* (svg->png in-svg out-png
                   #:key width height)
  "Render the file at IN-SVG as a PNG file in OUT-PNG.  When WIDTH and HEIGHT
are provided, use them as the dimensions of OUT-PNG; otherwise preserve the
dimensions of IN-SVG."
  (define svg
    (rsvg-handle-new-from-file in-svg))

  (let-values (((origin-width origin-height em ex)
                (rsvg-handle-get-dimensions svg)))
    (let* ((surf (cairo-image-surface-create 'argb32
                                             origin-width origin-height))
           (cr   (cairo-create surf)))
      (rsvg-handle-render-cairo svg cr)
      (cairo-surface-flush surf)
      (let ((cr (if (and width height
                         (not (= width origin-width))
                         (not (= height origin-height)))
                    (downscaled-surface surf
                                        #:source-width origin-width
                                        #:source-height origin-height
                                        #:width width
                                        #:height height)
                    cr)))
        (cairo-surface-write-to-png (cairo-get-target cr) out-png)))))

;;; svg.scm ends here
