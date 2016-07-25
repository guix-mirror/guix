;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Pjotr Prins <pjotr.public01@thebird.nl>
;;; Copyright © 2015, 2016 Ben Woodcroft <donttrustben@gmail.com>
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

(define-module (guix build ruby-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            ruby-build
            gem-home))

;; Commentary:
;;
;; Builder-side code of the standard Ruby package build procedure.
;;
;; Code:

(define (first-matching-file pattern)
  "Return the first file name that matches PATTERN in the current working
directory."
  (match (find-files "." pattern)
    ((file-name . _) file-name)
    (() (error "No files matching pattern: " pattern))))

(define gnu:unpack (assq-ref gnu:%standard-phases 'unpack))

(define (gem-archive? file-name)
  (string-match "^.*\\.gem$" file-name))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack the gem SOURCE and enter the resulting directory."
  (if (gem-archive? source)
      (and (zero? (system* "gem" "unpack" source))
           ;; The unpacked gem directory is named the same as the archive,
           ;; sans the ".gem" extension.  It is renamed to simply "gem" in an
           ;; effort to keep file names shorter to avoid UNIX-domain socket
           ;; file names and shebangs that exceed the system's fixed maximum
           ;; length when running test suites.
           (let ((dir (match:substring (string-match "^(.*)\\.gem$"
                                                     (basename source))
                                       1)))
             (rename-file dir "gem")
             (chdir "gem")
             #t))
      ;; Use GNU unpack strategy for things that aren't gem archives.
      (gnu:unpack #:source source)))

(define (first-gemspec)
  (first-matching-file "\\.gemspec$"))

(define* (replace-git-ls-files #:key source #:allow-other-keys)
  "Many gemspec files downloaded from outside rubygems.org use `git ls-files`
to list of the files to be included in the built gem.  However, since this
operation is not deterministic, we replace it with `find`."
  (when (not (gem-archive? source))
    (let ((gemspec (first-gemspec)))
      (substitute* gemspec
        (("`git ls-files`") "`find . -type f |sort`"))))
  #t)

(define* (extract-gemspec #:key source #:allow-other-keys)
  "Remove the original gemspec, if present, and replace it with a new one.
This avoids issues with upstream gemspecs requiring tools such as git to
generate the files list."
  (when (gem-archive? source)
    (let ((gemspec (or (false-if-exception (first-gemspec))
                       ;; Make new gemspec if one wasn't shipped.
                       ".gemspec")))

      (when (file-exists? gemspec) (delete-file gemspec))

      ;; Extract gemspec from source gem.
      (let ((pipe (open-pipe* OPEN_READ "gem" "spec" "--ruby" source)))
        (dynamic-wind
          (const #t)
          (lambda ()
            (call-with-output-file gemspec
              (lambda (out)
                ;; 'gem spec' writes to stdout, but 'gem build' only reads
                ;; gemspecs from a file, so we redirect the output to a file.
                (while (not (eof-object? (peek-char pipe)))
                  (write-char (read-char pipe) out))))
            #t)
          (lambda ()
            (close-pipe pipe)))))))

(define* (build #:key source #:allow-other-keys)
  "Build a new gem using the gemspec from the SOURCE gem."

  ;; Build a new gem from the current working directory.  This also allows any
  ;; dynamic patching done in previous phases to be present in the installed
  ;; gem.
  (zero? (system* "gem" "build" (first-gemspec))))

(define* (check #:key tests? test-target #:allow-other-keys)
  "Run the gem's test suite rake task TEST-TARGET.  Skip the tests if TESTS?
is #f."
  (if tests?
      (zero? (system* "rake" test-target))
      #t))

(define* (install #:key inputs outputs (gem-flags '())
                  #:allow-other-keys)
  "Install the gem archive SOURCE to the output store item.  Additional
GEM-FLAGS are passed to the 'gem' invokation, if present."
  (let* ((ruby-version
          (match:substring (string-match "ruby-(.*)\\.[0-9]$"
                                         (assoc-ref inputs "ruby"))
                           1))
         (out (assoc-ref outputs "out"))
         (gem-home (string-append out "/lib/ruby/gems/" ruby-version ".0"))
         (gem-file (first-matching-file "\\.gem$"))
         (gem-file-basename (basename gem-file))
         (gem-name (substring gem-file-basename
                              0
                              (- (string-length gem-file-basename) 4)))
         (gem-directory (string-append gem-home "/gems/" gem-name)))
    (setenv "GEM_HOME" gem-home)
    (mkdir-p gem-home)
    (and (apply system* "gem" "install" gem-file
                "--local" "--ignore-dependencies"
                ;; Executables should go into /bin, not /lib/ruby/gems.
                "--bindir" (string-append out "/bin")
                gem-flags)
         (begin
           ;; Remove the cached gem file as this is unnecessary and contains
           ;; timestamped files rendering builds not reproducible.
           (let ((cached-gem (string-append gem-home "/cache/" gem-file)))
             (log-file-deletion cached-gem)
             (delete-file cached-gem))
           ;; For gems with native extensions, several Makefile-related files
           ;; are created that contain timestamps or other elements making
           ;; them not reproducible.  They are unnecessary so we remove them.
           (if (file-exists? (string-append gem-directory "/ext"))
               (begin
                 (for-each (lambda (file)
                             (log-file-deletion file)
                             (delete-file file))
                           (append
                            (find-files (string-append gem-home "/doc")
                                        "page-Makefile.ri")
                            (find-files (string-append gem-home "/extensions")
                                        "gem_make.out")
                            (find-files (string-append gem-directory "/ext")
                                        "Makefile")))))
           #t))))

(define (log-file-deletion file)
  (display (string-append "deleting '" file "' for reproducibility\n")))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'configure)
    (replace 'unpack unpack)
    (add-before 'build 'extract-gemspec extract-gemspec)
    (add-after 'extract-gemspec 'replace-git-ls-files replace-git-ls-files)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)))

(define* (ruby-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

(define (gem-home store-path ruby-version)
  "Return a string to the gem home directory in the store given a STORE-PATH
and the RUBY-VERSION used to build that ruby package"
  (string-append
   store-path
   "/lib/ruby/gems/"
   (regexp-substitute #f
                      (string-match "^[0-9]+\\.[0-9]+" ruby-version)
                      0 ".0")))
