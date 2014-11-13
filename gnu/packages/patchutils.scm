;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages patchutils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages less)
  #:use-module (gnu packages perl))

(define-public patchutils
  (package
    (name "patchutils")
    (version "0.3.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://cyberelk.net/tim/data/patchutils/stable/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0g5df00cj4nczrmr4k791l7la0sq2wnf8rn981fsrz1f3d2yix4i"))
      (patches
       (list (search-patch "patchutils-xfail-gendiff-tests.patch")))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:parallel-tests? #f
       #:phases (alist-cons-before
                 'check 'patch-test-scripts
                 (lambda _
                   (let ((echo (which "echo")))
                     (substitute*
                         (find-files "tests" "^run-test$")
                       (("/bin/echo") echo))))
                 (alist-cons-after
                  'install 'wrap-program
                  ;; Point installed scripts to the utilities they need.
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let* ((out       (assoc-ref outputs "out"))
                           (diffutils (assoc-ref inputs "diffutils"))
                           (sed       (assoc-ref inputs "sed"))
                           (gawk      (assoc-ref inputs "gawk")))
                      (for-each
                       (lambda (prog)
                         (wrap-program (string-append out "/bin/" prog)
                                       `("PATH" ":" prefix
                                         ,(map (lambda (dir)
                                                 (string-append dir "/bin"))
                                               (list diffutils sed gawk)))))
                       '("dehtmldiff" "editdiff" "espdiff"))))
                  %standard-phases))))
    (home-page "http://cyberelk.net/tim/software/patchutils")
    (synopsis "Collection of tools for manipulating patch files")
    (description
     "Patchutils is a collection of programs that can manipulate patch files
in useful ways such as interpolating between two pre-patches, combining two
incremental patches, fixing line numbers in hand-edited patches, and simply
listing the files modified by a patch.")
    (license gpl2+)))

(define-public quilt
  (package
    (name "quilt")
    (version "0.61")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/quilt/"
                          name "-" version ".tar.gz"))
      (sha256
       (base32
        "1hwz58djkq9cv46sjwxbp2v5m8yjr41kd0nm1zm1xm6418khmv0y"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("less" ,less)
              ("file" ,file-5.20)                 ;work around CVE-2014-3710
              ("ed" ,ed)))
    (arguments
     '(#:parallel-tests? #f
       #:phases 
       (alist-cons-before
        'check 'patch-tests
        (lambda _
          (substitute*
              '("test/run"
                "test/edit.test") 
            (("/bin/sh") (which "sh")))
          ;; TODO: Run the mail tests once the mail feature can be supported.
          (delete-file "test/mail.test"))
        (alist-cons-after
         'install 'wrap-program
         ;; quilt's configure checks for the absolute path to the utilities it
         ;; needs, but uses only the name when invoking them, so we need to
         ;; make sure the quilt script can find those utilities when run.
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((out       (assoc-ref outputs "out"))
                  (coreutils (assoc-ref inputs "coreutils"))
                  (diffutils (assoc-ref inputs "diffutils"))
                  (findutils (assoc-ref inputs "findutils"))
                  (less      (assoc-ref inputs "less"))
                  (file      (assoc-ref inputs "file"))
                  (ed        (assoc-ref inputs "ed"))
                  (sed       (assoc-ref inputs "sed"))
                  (bash      (assoc-ref inputs "bash"))
                  (grep      (assoc-ref inputs "grep")))
             (wrap-program (string-append out "/bin/quilt")
                           `("PATH" ":" prefix
                             ,(map (lambda (dir)
                                     (string-append dir "/bin"))
                                   (list coreutils diffutils findutils
                                         less file ed sed bash grep))))))
         %standard-phases))))
    (home-page "https://savannah.nongnu.org/projects/quilt/")
    (synopsis "Script for managing patches to software")
    (description
     "Quilt allows you to easily manage large numbers of patches by keeping
track of the changes each patch makes.  Patches can be applied, un-applied,
refreshed, and more.")
    (license gpl2)))
