;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Jesse Gibbons <jgibbons2357+guix@gmail.com>
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

(define-module (gnu packages cvassistant)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression))

(define-public cvassistant
  (package
    (name "cvassistant")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/cvassistant/"
                                  "cvassistant-" version "-src.tar.bz2"))
              (sha256
               (base32
                "1y2680bazyiwm50gdhdd4982ckbjscrkbw2mngyk7yw708iadvr7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-donation-banner
           ;; Remove dialog box with a donation link, as suggested by
           ;; the INSTALL file.
           (lambda _
             (substitute* "controllers/mainwindow.cpp"
               (("//(#define NO_DONATION_PROMPT)" _ line) line))
             #t))
         (add-after 'unpack 'fix-quazip-directory
           (lambda _
             (substitute* "models/resumedocument.h"
               (("quazip(/quazipfile\\.h)" _ suffix)
                (string-append "quazip5" suffix)))
             #t))
         (add-after 'fix-quazip-directory 'fix-quazip-link
           (lambda _
             (substitute* "CVAssistant.pro"
               (("lquazip-qt5")
                "lquazip5"))
             #t))
         (add-after 'fix-quazip-directory 'fix-install-root
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "CVAssistant.pro"
                 (("/usr/(bin|share/)" _ suffix)
                  (string-append out "/" suffix)))
               #t)))
         (replace 'configure
           (lambda _ (invoke "qmake"))))))
    (inputs
     (list qtbase-5 quazip-0 zlib))
    (home-page "https://cvassistant.sourceforge.io/")
    (synopsis "Job application organizer")
    (description "Whether you're looking for a job or trying to help
  a friend to find one, CVAssistant is a tool for you.  It helps you by
  preparing resumes and cover letters and organizing your job
  application process.  It:
  @itemize
  @item Stores all your skills and experiences.
  @item Creates resumes tailored for each job you apply.
  @item Creates cover letters summarized to match each job
  advertisement.
  @item Keeps a history of job applications so you are ready when you
  receive a phone call.
  @item Writes resumes in your language.  All languages are supported!
  @end itemize")
    (license license:gpl3+)))
