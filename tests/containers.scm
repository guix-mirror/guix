;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (test-containers)
  #:use-module (guix utils)
  #:use-module (guix build syscalls)
  #:use-module (gnu build linux-container)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define (assert-exit x)
  (primitive-exit (if x 0 1)))

;; Skip these tests unless user namespaces are available.
(unless (file-exists? "/proc/self/ns/user")
  (exit 77))

(test-begin "containers")

(test-assert "call-with-container, user namespace"
  (zero?
   (call-with-container '()
     (lambda ()
       ;; The user is root within the new user namespace.
       (assert-exit (and (zero? (getuid)) (zero? (getgid)))))
     #:namespaces '(user))))

(test-assert "call-with-container, uts namespace"
  (zero?
   (call-with-container '()
     (lambda ()
       ;; The user is root within the container and should be able to change
       ;; the hostname of that container.
       (sethostname "test-container")
       (primitive-exit 0))
     #:namespaces '(user uts))))

(test-assert "call-with-container, pid namespace"
  (zero?
   (call-with-container '()
     (lambda ()
       (match (primitive-fork)
         (0
          ;; The first forked process in the new pid namespace is pid 2.
          (assert-exit (= 2 (getpid))))
         (pid
          (primitive-exit
           (match (waitpid pid)
             ((_ . status)
              (status:exit-val status)))))))
     #:namespaces '(user pid))))

(test-assert "call-with-container, mnt namespace"
  (zero?
   (call-with-container '(("none" device "/testing" "tmpfs" () #f #f))
     (lambda ()
       (assert-exit (file-exists? "/testing")))
     #:namespaces '(user mnt))))

(test-assert "call-with-container, all namespaces"
  (zero?
   (call-with-container '()
     (lambda ()
       (primitive-exit 0)))))

(test-assert "container-excursion"
  (call-with-temporary-directory
   (lambda (root)
     ;; Two pipes: One for the container to signal that the test can begin,
     ;; and one for the parent to signal to the container that the test is
     ;; over.
     (match (list (pipe) (pipe))
       (((start-in . start-out) (end-in . end-out))
        (define (container)
          (close end-out)
          (close start-in)
          ;; Signal for the test to start.
          (write 'ready start-out)
          (close start-out)
          ;; Wait for test completion.
          (read end-in)
          (close end-in))

        (define (namespaces pid)
          (let ((pid (number->string pid)))
            (map (lambda (ns)
                   (readlink (string-append "/proc/" pid "/ns/" ns)))
                 '("user" "ipc" "uts" "net" "pid" "mnt"))))

        (let* ((pid (run-container root '() %namespaces container))
               (container-namespaces (namespaces pid))
               (result
                (begin
                  (close start-out)
                  ;; Wait for container to be ready.
                  (read start-in)
                  (close start-in)
                  (container-excursion pid
                    (lambda ()
                      ;; Fork again so that the pid is within the context of
                      ;; the joined pid namespace instead of the original pid
                      ;; namespace.
                      (match (primitive-fork)
                        (0
                         ;; Check that all of the namespace identifiers are
                         ;; the same as the container process.
                         (assert-exit
                          (equal? container-namespaces
                                  (namespaces (getpid)))))
                        (fork-pid
                         (match (waitpid fork-pid)
                           ((_ . status)
                            (primitive-exit
                             (status:exit-val status)))))))))))
          (close end-in)
          ;; Stop the container.
          (write 'done end-out)
          (close end-out)
          (waitpid pid)
          (zero? result)))))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
