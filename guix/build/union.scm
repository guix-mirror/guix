;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build union)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (tree-union
            delete-duplicate-leaves
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

(define* (delete-duplicate-leaves tree
                                  #:optional
                                  (leaf=? equal?)
                                  (delete-duplicates (match-lambda
                                                      ((head _ ...) head))))
  "Delete duplicate leaves from TREE.  Two leaves are considered equal
when LEAF=? applied to them returns #t.  Each collision (list of leaves
that are LEAF=?) is passed to DELETE-DUPLICATES, which must return a
single leaf."
  (let loop ((tree tree))
    (match tree
      ((dir children ...)
       (let ((dirs   (filter pair? children))
             (leaves (remove pair? children)))
         (define collisions
           (fold (lambda (leaf result)
                   (define same?
                     (cut leaf=? leaf <>))

                   (if (any (cut find same? <>) result)
                       result
                       (match (filter same? leaves)
                         ((_)
                          result)
                         ((collision ...)
                          (cons collision result)))))
                 '()
                 leaves))

         (define non-collisions
           (filter (lambda (leaf)
                     (match (filter (cut leaf=? leaf <>) leaves)
                       ((_) #t)
                       ((_ _ ..1) #f)))
                   leaves))

         `(,dir
           ,@non-collisions
           ,@(map delete-duplicates collisions)
           ,@(map loop dirs))))
      (leaf leaf))))

(define* (union-build output directories)
  "Build in the OUTPUT directory a symlink tree that is the union of all
the DIRECTORIES."
  (define (file-tree dir)
    ;; Return the contents of DIR as a tree.

    (define (others-have-it? subdir)
      ;; Return #t if other elements of DIRECTORIES have SUBDIR.
      (let ((subdir (substring subdir (string-length dir))))
        (any (lambda (other)
               (and (not (string=? other dir))
                    (file-exists? (string-append other "/" subdir))))
             directories)))

    (match (file-system-fold (lambda (subdir stat result) ; enter?
                               ;; No need to traverse DIR since there's
                               ;; nothing to union it with.  Thus, we avoid
                               ;; creating a gazillon symlinks (think
                               ;; share/emacs/24.3, share/texmf, etc.)
                               (or (string=? subdir dir)
                                   (others-have-it? subdir)))
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
                             (lambda (dir stat result)  ; skip
                               ;; DIR is not available elsewhere, so treat it
                               ;; as a leaf.
                               (match result
                                 (((siblings ...) rest ...)
                                  `((,dir ,@siblings) ,@rest))))
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

  (define (leaf=? a b)
    (equal? (basename a) (basename b)))

  (define (resolve-collision leaves)
    ;; LEAVES all have the same basename, so choose one of them.
    (match (delete-duplicates leaves string=?)
      ((one-and-the-same)
       ;; LEAVES all actually point to the same file, so nothing to worry
       ;; about.
       one-and-the-same)
      ((and lst (head _ ...))
       ;; A real collision.
       (format (current-error-port) "warning: collision encountered: ~{~a ~}~%"
               lst)

       ;; TODO: Implement smarter strategies.
       (format (current-error-port) "warning: arbitrarily choosing ~a~%"
               head)
       head)))

  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF)

  (mkdir output)
  (let loop ((tree (delete-duplicate-leaves
                    (cons "."
                          (tree-union
                           (append-map (compose tree-leaves file-tree)
                                       (delete-duplicates directories))))
                    leaf=?
                    resolve-collision))
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
       (unless (string=? subdir ".")
         (let ((dir (string-join dir "/")))
           (mkdir (string-append output "/" dir "/" subdir))))
       (for-each (cute loop <> `(,@dir ,subdir))
                 leaves))
      ((leaves ...)
       ;; A series of leaves: iterate over them.
       (for-each (cut loop <> dir) leaves)))))

;;; union.scm ends here
