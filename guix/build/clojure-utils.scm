;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

(define-module (guix build clojure-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-26)
  #:export (@*
            @@*
            define-with-docs

            %doc-regex
            install-doc

            %source-dirs
            %test-dirs
            %compile-dir
            package-name->jar-names
            %main-class
            %omit-source?
            %aot-include
            %aot-exclude
            %tests?
            %test-include
            %test-exclude

            %clojure-regex
            canonicalize-relative-path
            find-files*
            file-sans-extension
            relative-path->clojure-lib-string
            find-clojure-libs
            compiled-from?
            include-list\exclude-list
            eval-with-clojure
            create-jar))

(define-syntax-rule (@* module name)
  "Like (@ MODULE NAME), but resolves at run time."
  (module-ref (resolve-interface 'module) 'name))

(define-syntax-rule (@@* module name)
  "Like (@@ MODULE NAME), but resolves at run time."
  (module-ref (resolve-module 'module) 'name))

(define-syntax-rule (define-with-docs name docs val)
  "Create top-level variable named NAME with doc string DOCS and value VAL."
  (begin (define name val)
         (set-object-property! name 'documentation docs)))

(define-with-docs %doc-regex
  "Default regex for matching the base name of top-level documentation files."
  (format #f
          "(~a)|(\\.(html|markdown|md|txt)$)"
          (@@ (guix build guile-build-system)
              %documentation-file-regexp)))

(define* (install-doc #:key
                      doc-dirs
                      (doc-regex %doc-regex)
                      outputs
                      #:allow-other-keys)
  "Install the following to the default documentation directory:

1. Top-level files with base name matching DOC-REGEX.
2. All files (recursively) inside DOC-DIRS.

DOC-REGEX can be compiled or uncompiled."
  (let* ((out (assoc-ref outputs "out"))
         (doc (assoc-ref outputs "doc"))
         (name-ver (strip-store-file-name out))
         (dest-dir (string-append (or doc out) "/share/doc/" name-ver "/"))
         (doc-regex* (if (string? doc-regex)
                         (make-regexp doc-regex)
                         doc-regex)))
    (for-each (cut install-file <> dest-dir)
              (remove (compose file-exists?
                               (cut string-append dest-dir <>))
                      (scandir "./" (cut regexp-exec doc-regex* <>))))
    (for-each (cut copy-recursively <> dest-dir)
              doc-dirs)
    #t))

(define-with-docs %source-dirs
  "A default list of source directories."
  '("src/"))

(define-with-docs %test-dirs
  "A default list of test directories."
  '("test/"))

(define-with-docs %compile-dir
  "Default directory for holding class files."
  "classes/")

(define (package-name->jar-names name)
  "Given NAME, a package name like \"foo-0.9.1b\",
return the list of default jar names: (\"foo-0.9.1b.jar\" \"foo.jar\")."
  (map (cut string-append <> ".jar")
       (list name
             (receive (base-name _)
                 (package-name->name+version name)
               base-name))))

(define-with-docs %main-class
  "Default name for main class.  It should be a symbol or #f."
  #f)

(define-with-docs %omit-source?
  "Include source in jars by default."
  #f)

(define-with-docs %aot-include
  "A default list of symbols deciding what to compile.  Note that the exclude
list has priority over the include list.  The special keyword #:all represents
all libraries found under the source directories."
  '(#:all))

(define-with-docs %aot-exclude
  "A default list of symbols deciding what not to compile.
See the doc string of '%aot-include' for more details."
  '())

(define-with-docs %tests?
  "Enable tests by default."
  #t)

(define-with-docs %test-include
  "A default list of symbols deciding what tests to include.  Note that the
exclude list has priority over the include list.  The special keyword #:all
represents all tests found under the test directories."
  '(#:all))

(define-with-docs %test-exclude
  "A default list of symbols deciding what tests to exclude.
See the doc string of '%test-include' for more details."
  '())

(define-with-docs %clojure-regex
  "Default regex for matching the base name of clojure source files."
  "\\.cljc?$")

(define-with-docs canonicalize-relative-path
  "Like 'canonicalize-path', but for relative paths.
Canonicalizations requiring the path to exist are omitted."
  (let ((remove.. (lambda (ls)
                    (fold-right (match-lambda*
                                  (((and comp (not "..")) (".." comps ...))
                                   comps)
                                  ((comp (comps ...))
                                   (cons comp comps)))
                                '()
                                ls))))
    (compose (match-lambda
               (() ".")
               (ls (string-join ls "/")))
             remove..
             (cut remove (cut member <> '("" ".")) <>)
             (cut string-split <> #\/))))

(define (find-files* base-dir . args)
  "Similar to 'find-files', but with BASE-DIR stripped and result
canonicalized."
  (map canonicalize-relative-path
       (with-directory-excursion base-dir
         (apply find-files "./" args))))

;;; FIXME: should be moved to (guix build utils)
(define-with-docs file-sans-extension
  "Strip extension from path, if any."
  (@@ (guix build guile-build-system)
      file-sans-extension))

(define (relative-path->clojure-lib-string path)
  "Convert PATH to a clojure library string."
  (string-map (match-lambda
                (#\/ #\.)
                (#\_ #\-)
                (chr chr))
              (file-sans-extension path)))

(define* (find-clojure-libs base-dir
                            #:key (clojure-regex %clojure-regex))
  "Return the list of clojure libraries found under BASE-DIR.

CLOJURE-REGEX can be compiled or uncompiled."
  (map (compose string->symbol
                relative-path->clojure-lib-string)
       (find-files* base-dir clojure-regex)))

(define (compiled-from? class lib)
  "Given class file CLASS and clojure library symbol LIB, decide if CLASS
results from compiling LIB."
  (string-prefix? (symbol->string lib)
                  (relative-path->clojure-lib-string class)))

(define* (include-list\exclude-list include-list exclude-list
                                    #:key all-list)
  "Given INCLUDE-LIST and EXCLUDE-LIST, replace all occurrences of #:all by
slicing ALL-LIST into them and compute their list difference."
  (define (replace-#:all ls all-ls)
    (append-map (match-lambda
                  (#:all all-ls)
                  (x (list x)))
                ls))
  (let ((include-list* (replace-#:all include-list all-list))
        (exclude-list* (replace-#:all exclude-list all-list)))
    (lset-difference equal? include-list* exclude-list*)))

(define (eval-with-clojure expr extra-paths)
  "Evaluate EXPR with clojure.

EXPR must be a s-expression writable by guile and readable by clojure.
For examples, '(require '[clojure.string]) will not work,
because the guile writer converts brackets to parentheses.

EXTRA-PATHS is a list of paths which will be appended to $CLASSPATH."
  (let* ((classpath (getenv "CLASSPATH"))
         (classpath* (string-join (cons classpath extra-paths) ":")))
    (invoke "java"
            "-classpath" classpath*
            "clojure.main"
            "--eval" (object->string expr))))

(define* (create-jar output-jar dir-files-alist
                     #:key
                     (verbose? #t)
                     (compress? #f)
                     (main-class %main-class))
  "Given DIR-FILES-ALIST, an alist of the form: ((DIR . FILES) ...)
Create jar named OUTPUT-JAR from FILES with DIR stripped."
  (let ((grouped-options (string-append "c"
                                        (if verbose? "v" "")
                                        "f"
                                        (if compress? "" "0")
                                        (if main-class "e" ""))))
    (apply invoke `("jar"
                    ,grouped-options
                    ,output-jar
                    ,@(if main-class (list (symbol->string main-class)) '())
                    ,@(append-map (match-lambda
                                    ((dir . files)
                                     (append-map (lambda (file)
                                                   `("-C" ,dir ,file))
                                                 files)))
                                  dir-files-alist)))))
