;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests-style)
  #:use-module (guix packages)
  #:use-module (guix scripts style)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module ((guix build utils) #:select (substitute*))
  #:use-module (guix gexp)                        ;for the reader extension
  #:use-module (guix diagnostics)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages multiprecision)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print))

(define (call-with-test-package inputs proc)
  (call-with-temporary-directory
   (lambda (directory)
     (call-with-output-file (string-append directory "/my-packages.scm")
       (lambda (port)
         (pretty-print
          `(begin
             (define-module (my-packages)
               #:use-module (guix)
               #:use-module (guix licenses)
               #:use-module (gnu packages acl)
               #:use-module (gnu packages base)
               #:use-module (gnu packages multiprecision)
               #:use-module (srfi srfi-1))

             (define base
               (package
                 (inherit coreutils)
                 (inputs '())
                 (native-inputs '())
                 (propagated-inputs '())))

             (define (sdl-union . lst)
               (package
                 (inherit base)
                 (name "sdl-union")))

             (define-public my-coreutils
               (package
                 (inherit base)
                 ,@inputs
                 (name "my-coreutils"))))
          port)))

     (proc directory))))

(define test-directory
  ;; Directory where the package definition lives.
  (make-parameter #f))

(define-syntax-rule (with-test-package fields exp ...)
  (call-with-test-package fields
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      ;; Run as a separate process to make sure FILE is reloaded.
      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")
      (system* "cat" file)

      (load file)
      (parameterize ((test-directory directory))
        exp ...))))

(define* (read-lines port line #:optional (count 1))
  "Read COUNT lines from PORT, starting from LINE."
  (let loop ((lines '())
             (count count))
    (cond ((< (port-line port) (- line 1))
           (read-char port)
           (loop lines count))
          ((zero? count)
           (string-concatenate-reverse lines))
          (else
           (match (read-line port 'concat)
             ((? eof-object?)
              (loop lines 0))
             (line
              (loop (cons line lines) (- count 1))))))))

(define* (read-package-field package field #:optional (count 1))
  (let* ((location (package-field-location package field))
         (file (location-file location))
         (line (location-line location)))
    (call-with-input-file (if (string-prefix? "/" file)
                              file
                              (string-append (test-directory) "/"
                                             file))
      (lambda (port)
        (read-lines port line count)))))

(define-syntax-rule (test-pretty-print str args ...)
  "Test equality after a round-trip where STR is passed to
'read-with-comments' and the resulting sexp is then passed to
'pretty-print-with-comments'."
  (test-equal str
    (call-with-output-string
      (lambda (port)
        (let ((exp (call-with-input-string str
                     read-with-comments)))
         (pretty-print-with-comments port exp args ...))))))


(test-begin "style")

(test-equal "nothing to rewrite"
  '()
  (with-test-package '()
    (package-direct-inputs (@ (my-packages) my-coreutils))))

(test-equal "input labels, mismatch"
  (list `(("foo" ,gmp) ("bar" ,acl))
        "      (inputs `((\"foo\" ,gmp) (\"bar\" ,acl)))\n")
  (with-test-package '((inputs `(("foo" ,gmp) ("bar" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs))))

(test-equal "input labels, simple"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "      (inputs (list gmp acl))\n")
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs))))

(test-equal "input labels, long list with one item per line"
  (list (concatenate (make-list 4 `(("gmp" ,gmp) ("acl" ,acl))))
        "\
        (list gmp
              acl
              gmp
              acl
              gmp
              acl
              gmp
              acl))\n")
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                 ("gmp" ,gmp) ("acl" ,acl)
                                 ("gmp" ,gmp) ("acl" ,acl)
                                 ("gmp" ,gmp) ("acl" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 8))))

(test-equal "input labels, sdl-union"
  "\
        (list gmp acl
              (sdl-union 1 2 3 4)))\n"
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                 ("sdl-union" ,(sdl-union 1 2 3 4)))))
    (read-package-field (@ (my-packages) my-coreutils) 'inputs 2)))

(test-equal "input labels, output"
  (list `(("gmp" ,gmp "debug") ("acl" ,acl))
        "      (inputs (list `(,gmp \"debug\") acl))\n")
  (with-test-package '((inputs `(("gmp" ,gmp "debug") ("acl" ,acl))))
    (list (package-direct-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs))))

(test-equal "input labels, prepend"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (prepend gmp acl)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                 ,@(package-propagated-inputs coreutils))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 2))))

(test-equal "input labels, prepend + delete"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (delete \"gmp\")
          (prepend gmp acl)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp)
                                 ("acl" ,acl)
                                 ,@(alist-delete "gmp"
                                                 (package-propagated-inputs coreutils)))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 3))))

(test-equal "input labels, prepend + delete multiple"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (delete \"foo\" \"bar\" \"baz\")
          (prepend gmp acl)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp)
                                 ("acl" ,acl)
                                 ,@(fold alist-delete
                                         (package-propagated-inputs coreutils)
                                         '("foo" "bar" "baz")))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 3))))

(test-equal "input labels, replace"
  (list '()                                 ;there's no "gmp" input to replace
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (replace \"gmp\" gmp)))\n")
  (with-test-package '((inputs `(("gmp" ,gmp)
                                 ,@(alist-delete "gmp"
                                                 (package-propagated-inputs coreutils)))))
    (list (package-inputs (@ (my-packages) my-coreutils))
          (read-package-field (@ (my-packages) my-coreutils) 'inputs 2))))

(test-equal "input labels, 'safe' policy"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
      (inputs (list gmp acl))\n")
  (call-with-test-package '((inputs `(("GMP" ,gmp) ("ACL" ,acl)))
                            (arguments '()))      ;no build system arguments
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "inputs"
               "--input-simplification=safe")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)))))

(test-equal "input labels, 'safe' policy, nothing changed"
  (list `(("GMP" ,gmp) ("ACL" ,acl))
        "\
      (inputs `((\"GMP\" ,gmp) (\"ACL\" ,acl)))\n")
  (call-with-test-package '((inputs `(("GMP" ,gmp) ("ACL" ,acl)))
                            ;; Non-empty argument list, so potentially unsafe
                            ;; input simplification.
                            (arguments
                             '(#:configure-flags
                               (assoc-ref %build-inputs "GMP"))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (system* "guix" "style" "-L" directory "my-coreutils"
               "-S" "inputs"
               "--input-simplification=safe")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)))))

(test-equal "input labels, margin comment"
  (list `(("gmp" ,gmp))
        `(("acl" ,acl))
        "      (inputs (list gmp)) ;margin comment\n"
        "      (native-inputs (list acl)) ;another one\n")
  (call-with-test-package '((inputs `(("gmp" ,gmp)))
                            (native-inputs `(("acl" ,acl))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        (("\"gmp\"(.*)$" _ rest)
         (string-append "\"gmp\"" (string-trim-right rest)
                        " ;margin comment\n"))
        (("\"acl\"(.*)$" _ rest)
         (string-append "\"acl\"" (string-trim-right rest)
                        " ;another one\n")))
      (system* "cat" file)

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (package-native-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs)
            (read-package-field (@ (my-packages) my-coreutils) 'native-inputs)))))

(test-equal "input labels, margin comment on long list"
  (list (concatenate (make-list 4 `(("gmp" ,gmp) ("acl" ,acl))))
        "\
        (list gmp ;margin comment
              acl
              gmp ;margin comment
              acl
              gmp ;margin comment
              acl
              gmp ;margin comment
              acl))\n")
  (call-with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl)
                                      ("gmp" ,gmp) ("acl" ,acl)
                                      ("gmp" ,gmp) ("acl" ,acl)
                                      ("gmp" ,gmp) ("acl" ,acl))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        (("\"gmp\"(.*)$" _ rest)
         (string-append "\"gmp\"" (string-trim-right rest)
                        " ;margin comment\n")))
      (system* "cat" file)

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs 8)))))

(test-equal "input labels, line comment"
  (list `(("gmp" ,gmp) ("acl" ,acl))
        "\
      (inputs (list gmp
                    ;; line comment!
                    acl))\n")
  (call-with-test-package '((inputs `(("gmp" ,gmp) ("acl" ,acl))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        ((",gmp\\)(.*)$" _ rest)
         (string-append ",gmp)\n   ;; line comment!\n" rest)))

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs 3)))))

(test-equal "input labels, modify-inputs and margin comment"
  (list `(("gmp" ,gmp) ("acl" ,acl) ("mpfr" ,mpfr))
        "\
        (modify-inputs (package-propagated-inputs coreutils)
          (prepend gmp ;margin comment
                   acl ;another one
                   mpfr)))\n")
  (call-with-test-package '((inputs
                             `(("gmp" ,gmp) ("acl" ,acl) ("mpfr" ,mpfr)
                               ,@(package-propagated-inputs coreutils))))
    (lambda (directory)
      (define file
        (string-append directory "/my-packages.scm"))

      (substitute* file
        ((",gmp\\)(.*)$" _ rest)
         (string-append ",gmp) ;margin comment\n" rest))
        ((",acl\\)(.*)$" _ rest)
         (string-append ",acl) ;another one\n" rest)))

      (system* "guix" "style" "-L" directory "-S" "inputs"
               "my-coreutils")

      (load file)
      (list (package-inputs (@ (my-packages) my-coreutils))
            (read-package-field (@ (my-packages) my-coreutils) 'inputs 4)))))

(test-pretty-print "(list 1 2 3 4)")
(test-pretty-print "(list 1
                          2
                          3
                          4)"
                   #:long-list 3
                   #:indent 20)
(test-pretty-print "\
(list abc
      def)"
                   #:max-width 11)
(test-pretty-print "\
(#:foo
 #:bar)"
                   #:max-width 10)

(test-pretty-print "\
(#:first 1
 #:second 2
 #:third 3)")

(test-pretty-print "\
((x
  1)
 (y
  2)
 (z
  3))"
                   #:max-width 3)

(test-pretty-print "\
(let ((x 1)
      (y 2)
      (z 3)
      (p 4))
  (+ x y))"
                   #:max-width 11)

(test-pretty-print "\
(lambda (x y)
  ;; This is a procedure.
  (let ((z (+ x y)))
    (* z z)))")

(test-pretty-print "\
#~(string-append #$coreutils \"/bin/uname\")")

(test-pretty-print "\
(package
  (inherit coreutils)
  (version \"42\"))")

(test-pretty-print "\
(modify-phases %standard-phases
  (add-after 'unpack 'post-unpack
    (lambda _
      #t))
  (add-before 'check 'pre-check
    (lambda* (#:key inputs #:allow-other-keys)
      do things ...)))")

(test-pretty-print "\
(#:phases (modify-phases sdfsdf
            (add-before 'x 'y
              (lambda _
                xyz))))")

(test-pretty-print "\
(description \"abcdefghijkl
mnopqrstuvwxyz.\")"
                   #:max-width 30)

(test-pretty-print "\
(description
 \"abcdefghijkl
mnopqrstuvwxyz.\")"
                   #:max-width 12)

(test-pretty-print "\
(description
 \"abcdefghijklmnopqrstuvwxyz\")"
                   #:max-width 33)

(test-pretty-print "\
(modify-phases %standard-phases
  (replace 'build
    ;; Nicely indented in 'modify-phases' context.
    (lambda _
      #t)))")

(test-pretty-print "\
(modify-inputs inputs
  ;; Regular indentation for 'replace' here.
  (replace \"gmp\" gmp))")

(test-pretty-print "\
(package
  ;; Here 'sha256', 'base32', and 'arguments' must be
  ;; immediately followed by a newline.
  (source (origin
            (method url-fetch)
            (sha256
             (base32
              \"not a real base32 string\"))))
  (arguments
   '(#:phases %standard-phases
     #:tests? #f)))")

(test-equal "pretty-print-with-comments, canonicalize-comment"
  "\
(list abc
      ;; Not a margin comment.
      ;; Ditto.
      ;;
      ;; There's a blank line above.
      def ;margin comment
      ghi)"
  (let ((sexp (call-with-input-string
                  "\
(list abc
  ;Not a margin comment.
  ;;;  Ditto.
  ;;;;;
  ; There's a blank line above.
  def  ;; margin comment
  ghi)"
                read-with-comments)))
    (call-with-output-string
      (lambda (port)
        (pretty-print-with-comments port sexp
                                    #:format-comment
                                    canonicalize-comment)))))

(test-end)

;; Local Variables:
;; eval: (put 'with-test-package 'scheme-indent-function 1)
;; eval: (put 'call-with-test-package 'scheme-indent-function 1)
;; End:
