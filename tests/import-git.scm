;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz
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

(define-module (test-import-git)
  #:use-module (git)
  #:use-module (guix git)
  #:use-module (guix tests)
  #:use-module (guix packages)
  #:use-module (guix import git)
  #:use-module (guix git-download)
  #:use-module (guix tests git)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;; Test the (guix import git) tools.

(test-begin "git")

(define* (make-package directory version #:optional (properties '()))
  (dummy-package "test-package"
    (version version)
    (properties properties)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "file://" directory))
             (commit version)))
       (sha256
        (base32
         "0000000000000000000000000000000000000000000000000000"))))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: no custom prefix, suffix, and delimiter"
  "1.0.1"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "1.0.1" "Release 1.0.1"))
    (let ((package (make-package directory "1.0.0")))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: custom prefix, no suffix and delimiter"
  "1.0.1"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "prefix-1.0.1" "Release 1.0.1"))
    (let ((package (make-package directory "1.0.0"
                                 '((release-tag-prefix . "prefix-")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: custom suffix, no prefix and delimiter"
  "1.0.1"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "1.0.1-suffix-123" "Release 1.0.1"))
    (let ((package (make-package directory "1.0.0"
                                 '((release-tag-suffix . "-suffix-[0-9]*")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: custom delimiter, no prefix and suffix"
  "2021.09.07"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "2021-09-07" "Release 2021-09-07"))
    (let ((package (make-package directory "2021-09-06"
                                 '((release-tag-version-delimiter . "-")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: empty delimiter, no prefix and suffix"
  "20210907"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "20210907" "Release 20210907"))
    (let ((package (make-package directory "20210906"
                                 '((release-tag-version-delimiter . "")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: custom prefix and suffix, no delimiter"
  "2.0.0"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "Release-2.0.0suffix-1" "Release 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((release-tag-prefix . "Release-")
                                   (release-tag-suffix . "suffix-[0-9]")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: custom prefix, suffix, and delimiter"
  "2.0.0"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "Release-2_0_0suffix-1" "Release 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((release-tag-prefix . "Release-")
                                   (release-tag-suffix . "suffix-[0-9]")
                                   (release-tag-version-delimiter . "_")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: only pre-releases available"
  #f
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "2.0.0-rc1" "Release candidate for 2.0.0"))
    (let ((package (make-package directory "1.0.0")))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases"
  "2.0.0-rc1"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "2.0.0-rc1" "Release candidate for 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases, and custom prefix"
  "2.0.0-rc1"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "version-2.0.0-rc1" "Release candidate for 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)
                                   (release-tag-prefix . "version-")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases, and custom suffix"
  "2.0.0-rc1"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "2.0.0-rc1-suffix" "Release candidate for 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)
                                   (release-tag-suffix . "-suffix")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases, delimiter conflicts with pre-release part"
  "2.0.0_alpha"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "2_0_0_alpha" "Alpha release for 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)
                                   (release-tag-version-delimiter . "_")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases, and custom suffix and prefix"
  "2.0.0-alpha"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "prefix123-2.0.0-alpha-suffix" "Alpha release for 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)
                                   (release-tag-prefix . "prefix[0-9]{3}-")
                                   (release-tag-suffix . "-suffix")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases, and custom suffix, prefix, and delimiter"
  "2.0.0-alpha"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "prefix123-2-0-0-alpha-suffix" "Alpha release for 2.0.0"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)
                                   (release-tag-prefix . "prefix[0-9]{3}-")
                                   (release-tag-suffix . "-suffix")
                                   (release-tag-version-delimiter . "-")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: accept pre-releases, no delimiter, and custom suffix, prefix"
  "2alpha"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "prefix123-2alpha-suffix" "Alpha release for version 2"))
    (let ((package (make-package directory "1.0.0"
                                 '((accept-pre-releases? . #t)
                                   (release-tag-prefix . "prefix[0-9]{3}-")
                                   (release-tag-suffix . "-suffix")
                                   (release-tag-version-delimiter . "")))))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: no tags found"
  #f
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit"))
    (let ((package (make-package directory "1.0.0")))
      (latest-git-tag-version package))))

(unless (which (git-command)) (test-skip 1))
(test-equal "latest-git-tag-version: no valid tags found"
  #f
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "First commit")
        (tag "Test" "Test tag"))
    (let ((package (make-package directory "1.0.0")))
      (latest-git-tag-version package))))

(test-end "git")
