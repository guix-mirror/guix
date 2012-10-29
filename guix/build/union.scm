;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix build union)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (tree-union
            union-build))

;;; Commentary:
;;;
;;; Build a directory that is the union of a set of directories, using
;;; symbolic links.
;;;
;;; Code:

(define (tree-union trees)
  "Return a tree that is the union of the trees listed in TREES.  Each
tree has the form (PARENT LEAVES ...) or just LEAF, where each leaf is
itself a tree. "
  (let loop ((trees trees))
    (match trees
      (()                                         ; nothing left
       '())
      (_
       (let ((dirs   (filter pair? trees))
             (leaves (remove pair? trees)))
         `(,@leaves
           ,@(fold (lambda (dir result)
                     (cons `(,dir
                             ,@(loop
                                (concatenate
                                 (filter-map (match-lambda
                                              ((head children ...)
                                               (and (equal? head dir)
                                                    children)))
                                             dirs))))
                           result))
                   '()
                   (delete-duplicates (map car dirs)))))))))

(define* (union-build output directories)
  "Build in the OUTPUT directory a symlink tree that is the union of all
the DIRECTORIES."
  (define (file-tree dir)
    ;; Return the contents of DIR as a tree.
    (match (file-system-fold (const #t)
                             (lambda (file stat result) ; leaf
                               (match result
                                 (((siblings ...) rest ...)
                                  `((,file ,@siblings) ,@rest))))
                             (lambda (dir stat result)  ; down
                               `(() ,@result))
                             (lambda (dir stat result)  ; up
                               (match result
                                 (((leaves ...) (siblings ...) rest ...)
                                  `(((,(basename dir) ,@leaves) ,@siblings)
                                    ,@rest))))
                             (const #f)                 ; skip
                             (lambda (file stat errno result)
                               (format (current-error-port) "union-build: ~a: ~a~%"
                                       file (strerror errno)))
                             '(())
                             dir)
      (((tree)) tree)
      (()       #f)))

  (define tree-leaves
    ;; Return the leaves of the given tree.
    (match-lambda
     (((? string?) leaves ...)
      leaves)))

  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF)

  (mkdir output)
  (let loop ((tree (tree-union (append-map (compose tree-leaves file-tree)
                                           directories)))
             (dir  '()))
    (match tree
      ((? string?)
       ;; A leaf: create a symlink.
       (let* ((dir    (string-join dir "/"))
              (target (string-append output "/" dir "/" (basename tree))))
         (format (current-error-port) "`~a' ~~> `~a'~%"
                 tree target)
         (symlink tree target)))
      (((? string? subdir) leaves ...)
       ;; A sub-directory: create it in OUTPUT, and iterate over LEAVES.
       (let ((dir (string-join dir "/")))
         (mkdir (string-append output "/" dir "/" subdir)))
       (for-each (cute loop <> `(,@dir ,subdir))
                 leaves))
      ((leaves ...)
       ;; A series of leaves: iterate over them.
       (for-each (cut loop <> dir) leaves)))))

;;; union.scm ends here
