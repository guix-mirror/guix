;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2015, 2016, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
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


(define-module (test build-utils)
  #:use-module (guix tests)
  #:use-module (guix build utils)
  #:use-module ((guix utils)
                #:select (%current-system call-with-temporary-directory))
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 popen))


(test-begin "build-utils")

(test-equal "alist-cons-before"
  '((a . 1) (x . 42) (b . 2) (c . 3))
  (alist-cons-before 'b 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-cons-before, reference not found"
  '((a . 1) (b . 2) (c . 3) (x . 42))
  (alist-cons-before 'z 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-cons-after"
  '((a . 1) (b . 2) (x . 42) (c . 3))
  (alist-cons-after 'b 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-cons-after, reference not found"
  '((a . 1) (b . 2) (c . 3) (x . 42))
  (alist-cons-after 'z 'x 42 '((a . 1) (b . 2) (c . 3))))

(test-equal "alist-replace"
  '((a . 1) (b . 77) (c . 3))
  (alist-replace 'b 77 '((a . 1) (b . 2) (c . 3))))

(test-assert "alist-replace, key not found"
  (not (false-if-exception
        (alist-replace 'z 77 '((a . 1) (b . 2) (c . 3))))))

(test-equal "fold-port-matches"
  (make-list 3 "Guix")
  (call-with-input-string "Guix is cool, Guix rocks, and it uses Guile, Guix!"
    (lambda (port)
      (fold-port-matches cons '() "Guix" port))))

(test-equal "fold-port-matches, trickier"
  (reverse '("Guix" "guix" "Guix" "guiX" "Guix"))
  (call-with-input-string "Guix, guix, GuiGuixguiX, Guix"
    (lambda (port)
      (fold-port-matches cons '()
                         (list (char-set #\G #\g)
                               (char-set #\u)
                               (char-set #\i)
                               (char-set #\x #\X))
                         port))))

(test-equal "fold-port-matches, with unmatched chars"
  '("Guix" #\, #\space
    "guix" #\, #\space
    #\G #\u #\i "Guix" "guiX" #\, #\space
    "Guix")
  (call-with-input-string "Guix, guix, GuiGuixguiX, Guix"
    (lambda (port)
      (reverse
       (fold-port-matches cons '()
                          (list (char-set #\G #\g)
                                (char-set #\u)
                                (char-set #\i)
                                (char-set #\x #\X))
                          port
                          cons)))))

(test-equal "wrap-program, one input, multiple calls"
  "hello world\n"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((bash (search-bootstrap-binary "bash" (%current-system)))
           (foo  (string-append directory "/foo")))

       (call-with-output-file foo
         (lambda (p)
           (format p
                   "#!~a~%echo \"${GUIX_FOO} ${GUIX_BAR}\"~%"
                   bash)))
       (chmod foo #o777)

       ;; wrap-program uses `which' to find bash for the wrapper shebang, but
       ;; it can't know about the bootstrap bash in the store, since it's not
       ;; named "bash".  Help it out a bit by providing a symlink it this
       ;; package's output.
       (with-environment-variable "PATH" (dirname bash)
         (wrap-program foo `("GUIX_FOO" prefix ("hello")))
         (wrap-program foo `("GUIX_BAR" prefix ("world")))

         ;; The bootstrap Bash is linked against an old libc and would abort
         ;; with an assertion failure when trying to load incompatible locale
         ;; data.
         (unsetenv "LOCPATH")

         (let* ((pipe (open-input-pipe foo))
                (str  (get-string-all pipe)))
           (with-directory-excursion directory
             (for-each delete-file '("foo" ".foo-real")))
           (and (zero? (close-pipe pipe))
                str)))))))

(test-assert "invoke/quiet, success"
  (begin
    (invoke/quiet "true")
    #t))

(test-assert "invoke/quiet, failure"
  (guard (c ((message-condition? c)
             (string-contains (condition-message c) "This is an error.")))
    (invoke/quiet "sh" "-c" "echo This is an error. ; false")
    #f))

(test-assert "invoke/quiet, failure, message on stderr"
  (guard (c ((message-condition? c)
             (string-contains (condition-message c)
                              "This is another error.")))
    (invoke/quiet "sh" "-c" "echo This is another error. >&2 ; false")
    #f))

(let ((script-contents "\
#!/anything/cabbage-bash-1.2.3/bin/sh

echo hello world"))

  (test-equal "wrap-script, simple case"
    (string-append
     (format #f "\
#!~a --no-auto-compile
#!#; Guix wrapper
#\\-~s
#\\-~s
"
             (which "guile")
             '(begin (let ((current (getenv "GUIX_FOO")))
                       (setenv "GUIX_FOO"
                               (if current
                                   (string-append "/some/path:/some/other/path"
                                                  ":" current)
                                   "/some/path:/some/other/path"))))
             '(let ((cl (command-line)))
                (apply execl "/anything/cabbage-bash-1.2.3/bin/sh"
                       (car cl) (append (quote ()) cl))))
     script-contents)
    (call-with-temporary-directory
     (lambda (directory)
       (let ((script-file-name (string-append directory "/foo")))
         (call-with-output-file script-file-name
           (lambda (port)
             (display script-contents port)))
         (chmod script-file-name #o777)
         (wrap-script script-file-name
                      `("GUIX_FOO" prefix ("/some/path"
                                           "/some/other/path")))
         (let ((str (call-with-input-file script-file-name get-string-all)))
           (with-directory-excursion directory
             (delete-file "foo"))
           str))))))

(let ((script-contents "\
#!/anything/cabbage-bash-1.2.3/bin/python3 -and -args
# vim:fileencoding=utf-8
print('hello world')"))

  (test-equal "wrap-script, with encoding declaration"
    (string-append
     (format #f "\
#!MYGUILE --no-auto-compile
#!#; # vim:fileencoding=utf-8
#\\-~s
#\\-~s
"
             '(begin (let ((current (getenv "GUIX_FOO")))
                       (setenv "GUIX_FOO"
                               (if current
                                   (string-append "/some/path:/some/other/path"
                                                  ":" current)
                                   "/some/path:/some/other/path"))))
             `(let ((cl (command-line)))
                (apply execl "/anything/cabbage-bash-1.2.3/bin/python3"
                       (car cl)
                       (append '("-and" "-args") cl))))
     script-contents)
    (call-with-temporary-directory
     (lambda (directory)
       (let ((script-file-name (string-append directory "/foo")))
         (call-with-output-file script-file-name
           (lambda (port)
             (format port script-contents)))
         (chmod script-file-name #o777)

         (wrap-script script-file-name
                      #:guile "MYGUILE"
                      `("GUIX_FOO" prefix ("/some/path"
                                           "/some/other/path")))
         (let ((str (call-with-input-file script-file-name get-string-all)))
           (with-directory-excursion directory
             (delete-file "foo"))
           str))))))

(test-assert "wrap-script, raises condition"
  (call-with-temporary-directory
   (lambda (directory)
     (let ((script-file-name (string-append directory "/foo")))
       (call-with-output-file script-file-name
         (lambda (port)
           (format port "This is not a script")))
       (chmod script-file-name #o777)
       (guard (c ((wrap-error? c) #t))
         (wrap-script script-file-name
                      #:guile "MYGUILE"
                      `("GUIX_FOO" prefix ("/some/path"
                                           "/some/other/path")))
         #f)))))

(define (arg-test bash-args)
  (call-with-temporary-directory
   (lambda (directory)
     (let ((script-file-name (string-append directory "/bash-test.sh")))
       (call-with-output-file script-file-name
         (lambda (port)
           (display (string-append "\
#!" (which "bash") bash-args "
echo \"$#$0$*${A}\"")
                    port)))

       (display "Unwrapped script contents:\n")
       (call-with-input-file script-file-name
         (lambda (port) (display (get-string-all port))))
       (newline) (newline)
       (chmod script-file-name #o777)
       (setenv "A" "A")
       (let* ((run-script (lambda _
                            (open-pipe*
                             OPEN_READ
                             script-file-name "1" "2" "3 3" "4")))
              (pipe (run-script))
              (unwrapped-output (get-string-all pipe)))
         (close-pipe pipe)

         (wrap-script script-file-name `("A" = ("A\nA")))

         (display "Wrapped script contents:\n")
         (call-with-input-file script-file-name
           (lambda (port) (display (get-string-all port))))
         (newline) (newline)

         (let* ((pipe (run-script))
                (wrapped-output (get-string-all pipe)))
           (close-pipe pipe)
           (display "./bash-test.sh 1 2 3\\ 3 4 # Output:\n")
           (display unwrapped-output) (newline)
           (display "./bash-test.sh 1 2 3\\ 3 4 # Output (wrapped):\n")
           (display wrapped-output) (newline)
           (string=? (string-append unwrapped-output "A\n")
                     wrapped-output)))))))

(test-assert "wrap-script, argument handling"
  (arg-test ""))

(test-assert "wrap-script, argument handling, bash --norc"
  (arg-test " --norc"))

(test-equal "substitute*, text contains a NUL byte, UTF-8"
  "c\0d"
  (with-fluids ((%default-port-encoding "UTF-8")
                (%default-port-conversion-strategy 'error))
    ;; The GNU libc is locale sensitive.  Depending on the value of LANG, the
    ;; test could fail with "string contains #\\nul character: ~S" or "cannot
    ;; convert wide string to output locale".
    (setlocale LC_ALL "en_US.UTF-8")
    (call-with-temporary-output-file
     (lambda (file port)
       (format port "a\0b")
       (flush-output-port port)

       (substitute* file
         (("a") "c")
         (("b") "d"))

       (with-input-from-file file
         (lambda _
           (get-string-all (current-input-port))))))))

(test-equal "search-input-file: exception if not found"
  `((path)
    (file . "does-not-exist"))
  (guard (e ((search-error? e)
             `((path . ,(search-error-path e))
               (file . ,(search-error-file e)))))
    (search-input-file '() "does-not-exist")))

(test-equal "search-input-file: can find if existent"
  (which "guile")
  (search-input-file
    `(("guile/bin" . ,(dirname (which "guile"))))
    "guile"))

(test-equal "search-input-file: can search in multiple directories"
  (which "guile")
  (call-with-temporary-directory
    (lambda (directory)
      (search-input-file
        `(("irrelevant" . ,directory)
          ("guile/bin" . ,(dirname (which "guile"))))
        "guile"))))

(test-end)
