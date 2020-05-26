;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-git)
  #:use-module (git)
  #:use-module (guix git)
  #:use-module (guix tests git)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;; Test the (guix git) tools.

(test-begin "git")

;; 'with-temporary-git-repository' relies on the 'git' command.
(unless (which (git-command)) (test-skip 1))
(test-assert "commit-difference, linear history"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "first commit")
        (add "b.txt" "B")
        (commit "second commit")
        (add "c.txt" "C")
        (commit "third commit")
        (add "d.txt" "D")
        (commit "fourth commit"))
    (with-repository directory repository
      (let ((commit1 (find-commit repository "first"))
            (commit2 (find-commit repository "second"))
            (commit3 (find-commit repository "third"))
            (commit4 (find-commit repository "fourth")))
        (and (lset= eq? (commit-difference commit4 commit1)
                    (list commit2 commit3 commit4))
             (lset= eq? (commit-difference commit4 commit2)
                    (list commit3 commit4))
             (equal? (commit-difference commit3 commit2)
                     (list commit3))

             ;; COMMIT4 is not an ancestor of COMMIT1 so we should get the
             ;; empty list.
             (null? (commit-difference commit1 commit4)))))))

(unless (which (git-command)) (test-skip 1))
(test-assert "commit-difference, fork"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "first commit")
        (branch "devel")
        (checkout "devel")
        (add "devel/1.txt" "1")
        (commit "first devel commit")
        (add "devel/2.txt" "2")
        (commit "second devel commit")
        (checkout "master")
        (add "b.txt" "B")
        (commit "second commit")
        (add "c.txt" "C")
        (commit "third commit")
        (merge "devel" "merge")
        (add "d.txt" "D")
        (commit "fourth commit"))
    (with-repository directory repository
      (let ((master1 (find-commit repository "first commit"))
            (master2 (find-commit repository "second commit"))
            (master3 (find-commit repository "third commit"))
            (master4 (find-commit repository "fourth commit"))
            (devel1  (find-commit repository "first devel"))
            (devel2  (find-commit repository "second devel"))
            (merge   (find-commit repository "merge")))
        (and (equal? (commit-difference master4 merge)
                     (list master4))
             (lset= eq? (commit-difference master3 master1)
                    (list master3 master2))
             (lset= eq? (commit-difference devel2 master1)
                    (list devel2 devel1))

             ;; The merge occurred between MASTER2 and MASTER4 so here we
             ;; expect to see all the commits from the "devel" branch in
             ;; addition to those on "master".
             (lset= eq? (commit-difference master4 master2)
                    (list master4 merge master3 devel1 devel2)))))))

(unless (which (git-command)) (test-skip 1))
(test-assert "commit-difference, excluded commits"
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "first commit")
        (add "b.txt" "B")
        (commit "second commit")
        (add "c.txt" "C")
        (commit "third commit")
        (add "d.txt" "D")
        (commit "fourth commit")
        (add "e.txt" "E")
        (commit "fifth commit"))
    (with-repository directory repository
      (let ((commit1 (find-commit repository "first"))
            (commit2 (find-commit repository "second"))
            (commit3 (find-commit repository "third"))
            (commit4 (find-commit repository "fourth"))
            (commit5 (find-commit repository "fifth")))
        (and (lset= eq? (commit-difference commit4 commit1 (list commit2))
                    (list commit3 commit4))
             (lset= eq? (commit-difference commit4 commit1 (list commit3))
                    (list commit4))
             (lset= eq? (commit-difference commit4 commit1 (list commit5))
                    (list commit2 commit3 commit4)))))))

(unless (which (git-command)) (test-skip 1))
(test-equal "commit-relation"
  '(self                                          ;master3 master3
    ancestor                                      ;master1 master3
    descendant                                    ;master3 master1
    unrelated                                     ;master2 branch1
    unrelated                                     ;branch1 master2
    ancestor                                      ;branch1 merge
    descendant                                    ;merge branch1
    ancestor                                      ;master1 merge
    descendant)                                   ;merge master1
  (with-temporary-git-repository directory
      '((add "a.txt" "A")
        (commit "first commit")
        (branch "hack")
        (checkout "hack")
        (add "1.txt" "1")
        (commit "branch commit")
        (checkout "master")
        (add "b.txt" "B")
        (commit "second commit")
        (add "c.txt" "C")
        (commit "third commit")
        (merge "hack" "merge"))
    (with-repository directory repository
      (let ((master1 (find-commit repository "first"))
            (master2 (find-commit repository "second"))
            (master3 (find-commit repository "third"))
            (branch1 (find-commit repository "branch"))
            (merge   (find-commit repository "merge")))
        (list (commit-relation master3 master3)
              (commit-relation master1 master3)
              (commit-relation master3 master1)
              (commit-relation master2 branch1)
              (commit-relation branch1 master2)
              (commit-relation branch1 merge)
              (commit-relation merge branch1)
              (commit-relation master1 merge)
              (commit-relation merge master1))))))

(test-end "git")
