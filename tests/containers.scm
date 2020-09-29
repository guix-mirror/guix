;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((gnu system linux-container)
                #:select (eval/container))
  #:use-module (gnu system file-systems)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define (assert-exit x)
  (primitive-exit (if x 0 1)))

(test-begin "containers")

;; Skip these tests unless user namespaces are available and the setgroups
;; file (introduced in Linux 3.19 to address a security issue) exists.
(define (skip-if-unsupported)
  (unless (and (user-namespace-supported?)
               (unprivileged-user-namespace-supported?)
               (setgroups-supported?))
    (test-skip 1)))

(skip-if-unsupported)
(test-assert "call-with-container, exit with 0 when there is no error"
  (zero?
   (call-with-container '() (const #t) #:namespaces '(user))))

(skip-if-unsupported)
(test-assert "call-with-container, user namespace"
  (zero?
   (call-with-container '()
     (lambda ()
       ;; The user is root within the new user namespace.
       (assert-exit (and (zero? (getuid)) (zero? (getgid)))))
     #:namespaces '(user))))

(skip-if-unsupported)
(test-assert "call-with-container, user namespace, guest UID/GID"
  (zero?
   (call-with-container '()
     (lambda ()
       (assert-exit (and (= 42 (getuid)) (= 77 (getgid)))))
     #:guest-uid 42
     #:guest-gid 77
     #:namespaces '(user))))

(skip-if-unsupported)
(test-assert "call-with-container, uts namespace"
  (zero?
   (call-with-container '()
     (lambda ()
       ;; The user is root within the container and should be able to change
       ;; the hostname of that container.
       (sethostname "test-container")
       (primitive-exit 0))
     #:namespaces '(user uts))))

(skip-if-unsupported)
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

(skip-if-unsupported)
(test-assert "call-with-container, mnt namespace"
  (zero?
   (call-with-container (list (file-system
                                (device "none")
                                (mount-point "/testing")
                                (type "tmpfs")
                                (check? #f)))
     (lambda ()
       (assert-exit (file-exists? "/testing")))
     #:namespaces '(user mnt))))

(skip-if-unsupported)
(test-equal "call-with-container, mnt namespace, wrong bind mount"
  `(system-error ,ENOENT)
  ;; An exception should be raised; see <http://bugs.gnu.org/23306>.
  (catch 'system-error
    (lambda ()
      (call-with-container (list (file-system
                                   (device "/does-not-exist")
                                   (mount-point "/foo")
                                   (type "none")
                                   (flags '(bind-mount))
                                   (check? #f)))
        (const #t)
        #:namespaces '(user mnt)))
    (lambda args
      (list 'system-error (system-error-errno args)))))

(skip-if-unsupported)
(test-assert "call-with-container, all namespaces"
  (zero?
   (call-with-container '()
     (lambda ()
       (primitive-exit 0)))))

(skip-if-unsupported)
(test-assert "call-with-container, mnt namespace, root permissions"
  (zero?
   (call-with-container '()
     (lambda ()
       (assert-exit (= #o755 (stat:perms (lstat "/")))))
     #:namespaces '(user mnt))))

(skip-if-unsupported)
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

        (let* ((pid (run-container root '() %namespaces 1 container))
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

(skip-if-unsupported)
(test-equal "container-excursion, same namespaces"
  42
  ;; The parent and child are in the same namespaces.  'container-excursion'
  ;; should notice that and avoid calling 'setns' since that would fail.
  (container-excursion (getpid)
    (lambda ()
      (primitive-exit 42))))

(skip-if-unsupported)
(test-assert "container-excursion*"
  (call-with-temporary-directory
   (lambda (root)
     (define (namespaces pid)
       (let ((pid (number->string pid)))
         (map (lambda (ns)
                (readlink (string-append "/proc/" pid "/ns/" ns)))
              '("user" "ipc" "uts" "net" "pid" "mnt"))))

     (let* ((pid    (run-container root '()
                                   %namespaces 1
                                   (lambda ()
                                     (sleep 100))))
            (expected (namespaces pid))
            (result (container-excursion* pid
                      (lambda ()
                        (namespaces 1)))))
       (kill pid SIGKILL)
       (equal? result expected)))))

(skip-if-unsupported)
(test-equal "container-excursion*, same namespaces"
  42
  (container-excursion* (getpid)
    (lambda ()
      (* 6 7))))

(skip-if-unsupported)
(test-equal "eval/container, exit status"
  42
  (let* ((store  (open-connection-for-tests))
         (status (run-with-store store
                   (eval/container #~(exit 42)))))
    (close-connection store)
    (status:exit-val status)))

(skip-if-unsupported)
(test-assert "eval/container, writable user mapping"
  (call-with-temporary-directory
   (lambda (directory)
     (define store
       (open-connection-for-tests))
     (define result
       (string-append directory "/r"))
     (define requisites*
       (store-lift requisites))

     (call-with-output-file result (const #t))
     (run-with-store store
       (mlet %store-monad ((status (eval/container
                                    #~(begin
                                        (use-modules (ice-9 ftw))
                                        (call-with-output-file "/result"
                                          (lambda (port)
                                            (write (scandir #$(%store-prefix))
                                                   port))))
                                    #:mappings
                                    (list (file-system-mapping
                                           (source result)
                                           (target "/result")
                                           (writable? #t)))))
                           (reqs   (requisites*
                                    (list (derivation->output-path
                                           (%guile-for-build))))))
         (close-connection store)
         (return (and (zero? (pk 'status status))
                      (lset= string=? (cons* "." ".." (map basename reqs))
                             (pk (call-with-input-file result read))))))))))

(skip-if-unsupported)
(test-assert "eval/container, non-empty load path"
  (call-with-temporary-directory
   (lambda (directory)
     (define store
       (open-connection-for-tests))
     (define result
       (string-append directory "/r"))
     (define requisites*
       (store-lift requisites))

     (mkdir result)
     (run-with-store store
       (mlet %store-monad ((status (eval/container
                                    (with-imported-modules '((guix build utils))
                                      #~(begin
                                          (use-modules (guix build utils))
                                          (mkdir-p "/result/a/b/c")))
                                    #:mappings
                                    (list (file-system-mapping
                                           (source result)
                                           (target "/result")
                                           (writable? #t))))))
         (close-connection store)
         (return (and (zero? status)
                      (file-is-directory?
                       (string-append result "/a/b/c")))))))))

(test-end)
