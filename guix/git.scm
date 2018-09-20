;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix git)
  #:use-module (git)
  #:use-module (git object)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (%repository-cache-directory
            update-cached-checkout
            latest-repository-commit))

(define %repository-cache-directory
  (make-parameter (string-append (cache-directory #:ensure? #f)
                                 "/checkouts")))

(define-syntax-rule (with-libgit2 thunk ...)
  (begin
    ;; XXX: The right thing to do would be to call (libgit2-shutdown) here,
    ;; but pointer finalizers used in guile-git may be called after shutdown,
    ;; resulting in a segfault. Hence, let's skip shutdown call for now.
    (libgit2-init!)
    thunk ...))

(define* (url-cache-directory url
                              #:optional (cache-directory
                                          (%repository-cache-directory)))
  "Return the directory associated to URL in %repository-cache-directory."
  (string-append
   cache-directory "/"
   (bytevector->base32-string (sha256 (string->utf8 url)))))

(define (clone* url directory)
  "Clone git repository at URL into DIRECTORY.  Upon failure,
make sure no empty directory is left behind."
  (with-throw-handler #t
    (lambda ()
      (mkdir-p directory)

      ;; Note: Explicitly pass options to work around the invalid default
      ;; value in Guile-Git: <https://bugs.gnu.org/29238>.
      (if (module-defined? (resolve-interface '(git))
                           'clone-init-options)
          (clone url directory (clone-init-options))
          (clone url directory)))
    (lambda _
      (false-if-exception (rmdir directory)))))

(define (url+commit->name url sha1)
  "Return the string \"<REPO-NAME>-<SHA1:7>\" where REPO-NAME is the name of
the git repository, extracted from URL and SHA1:7 the seven first digits
of SHA1 string."
  (string-append
   (string-replace-substring
    (last (string-split url #\/)) ".git" "")
   "-" (string-take sha1 7)))

(define (switch-to-ref repository ref)
  "Switch to REPOSITORY's branch, commit or tag specified by REF.  Return the
OID (roughly the commit hash) corresponding to REF."
  (define obj
    (match ref
      (('branch . branch)
       (let ((oid (reference-target
                   (branch-lookup repository branch BRANCH-REMOTE))))
         (object-lookup repository oid)))
      (('commit . commit)
       (let ((len (string-length commit)))
         ;; 'object-lookup-prefix' appeared in Guile-Git in Mar. 2018, so we
         ;; can't be sure it's available.  Furthermore, 'string->oid' used to
         ;; read out-of-bounds when passed a string shorter than 40 chars,
         ;; which is why we delay calls to it below.
         (if (< len 40)
             (if (module-defined? (resolve-interface '(git object))
                                  'object-lookup-prefix)
                 (object-lookup-prefix repository (string->oid commit) len)
                 (raise (condition
                         (&message
                          (message "long Git object ID is required")))))
             (object-lookup repository (string->oid commit)))))
      (('tag    . tag)
       (let ((oid (reference-name->oid repository
                                       (string-append "refs/tags/" tag))))
         (object-lookup repository oid)))))

  (reset repository obj RESET_HARD)
  (object-id obj))

(define* (update-cached-checkout url
                                 #:key
                                 (ref '(branch . "master"))
                                 (cache-directory
                                  (url-cache-directory
                                   url (%repository-cache-directory))))
  "Update the cached checkout of URL to REF in CACHE-DIRECTORY.  Return two
values: the cache directory name, and the SHA1 commit (a string) corresponding
to REF.

REF is pair whose key is [branch | commit | tag] and value the associated
data, respectively [<branch name> | <sha1> | <tag name>]."
  (define canonical-ref
    ;; We used to require callers to specify "origin/" for each branch, which
    ;; made little sense since the cache should be transparent to them.  So
    ;; here we append "origin/" if it's missing and otherwise keep it.
    (match ref
      (('branch . branch)
       `(branch . ,(if (string-prefix? "origin/" branch)
                       branch
                       (string-append "origin/" branch))))
      (_ ref)))

  (with-libgit2
   (let* ((cache-exists? (openable-repository? cache-directory))
          (repository    (if cache-exists?
                             (repository-open cache-directory)
                             (clone* url cache-directory))))
     ;; Only fetch remote if it has not been cloned just before.
     (when cache-exists?
       (remote-fetch (remote-lookup repository "origin")))
     (let ((oid (switch-to-ref repository canonical-ref)))

       ;; Reclaim file descriptors and memory mappings associated with
       ;; REPOSITORY as soon as possible.
       (when (module-defined? (resolve-interface '(git repository))
                              'repository-close!)
         (repository-close! repository))

       (values cache-directory (oid->string oid))))))

(define* (latest-repository-commit store url
                                   #:key
                                   (cache-directory
                                    (%repository-cache-directory))
                                   (ref '(branch . "master")))
  "Return two values: the content of the git repository at URL copied into a
store directory and the sha1 of the top level commit in this directory.  The
reference to be checkout, once the repository is fetched, is specified by REF.
REF is pair whose key is [branch | commit | tag] and value the associated
data, respectively [<branch name> | <sha1> | <tag name>].

Git repositories are kept in the cache directory specified by
%repository-cache-directory parameter."
  (define (dot-git? file stat)
    (and (string=? (basename file) ".git")
         (eq? 'directory (stat:type stat))))

  (let*-values
      (((checkout commit)
        (update-cached-checkout url
                                #:ref ref
                                #:cache-directory
                                (url-cache-directory url cache-directory)))
       ((name)
        (url+commit->name url commit)))
    (values (add-to-store store name #t "sha256" checkout
                          #:select? (negate dot-git?))
            commit)))
