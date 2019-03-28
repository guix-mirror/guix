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
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            ruby-build))

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
      (begin
        (invoke "gem" "unpack" source)
        ;; The unpacked gem directory is named the same as the archive,
        ;; sans the ".gem" extension.  It is renamed to simply "gem" in an
        ;; effort to keep file names shorter to avoid UNIX-domain socket
        ;; file names and shebangs that exceed the system's fixed maximum
        ;; length when running test suites.
        (let ((dir (match:substring (string-match "^(.*)\\.gem$"
                                                  (basename source))
                                    1)))
          (rename-file dir "gem")
          (chdir "gem"))
        #t)
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
        (("`git ls-files`") "`find . -type f |sort`")
        (("`git ls-files -z`") "`find . -type f -print0 |sort -z`"))))
  #t)

(define* (extract-gemspec #:key source #:allow-other-keys)
  "Remove the original gemspec, if present, and replace it with a new one.
This avoids issues with upstream gemspecs requiring tools such as git to
generate the files list."
  (if (gem-archive? source)
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
              (close-pipe pipe)))))
      (display "extract-gemspec: skipping as source is not a gem archive\n"))
  #t)

(define* (build #:key source #:allow-other-keys)
  "Build a new gem using the gemspec from the SOURCE gem."

  ;; Build a new gem from the current working directory.  This also allows any
  ;; dynamic patching done in previous phases to be present in the installed
  ;; gem.
  (invoke "gem" "build" (first-gemspec)))

(define* (check #:key tests? test-target #:allow-other-keys)
  "Run the gem's test suite rake task TEST-TARGET.  Skip the tests if TESTS?
is #f."
  (if tests?
      (invoke "rake" test-target)
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
         (vendor-dir (string-append out "/lib/ruby/vendor_ruby"))
         (gem-file (first-matching-file "\\.gem$"))
         (gem-file-basename (basename gem-file))
         (gem-name (substring gem-file-basename
                              0
                              (- (string-length gem-file-basename) 4)))
         (gem-dir (string-append vendor-dir "/gems/" gem-name)))
    (setenv "GEM_VENDOR" vendor-dir)

    (or (apply invoke "gem" "install" gem-file
               "--verbose"
               "--local" "--ignore-dependencies" "--vendor"
               ;; Executables should go into /bin, not
               ;; /lib/ruby/gems.
               "--bindir" (string-append out "/bin")
               gem-flags)
        (begin
          (let ((failed-output-dir (string-append (getcwd) "/out")))
            (mkdir failed-output-dir)
            (copy-recursively out failed-output-dir))
          (error "installation failed")))

    ;; Remove the cached gem file as this is unnecessary and contains
    ;; timestamped files rendering builds not reproducible.
    (let ((cached-gem (string-append vendor-dir "/cache/" gem-file)))
      (log-file-deletion cached-gem)
      (delete-file cached-gem))

    ;; For gems with native extensions, several Makefile-related files
    ;; are created that contain timestamps or other elements making
    ;; them not reproducible.  They are unnecessary so we remove them.
    (when (file-exists? (string-append gem-dir "/ext"))
      (for-each (lambda (file)
                  (log-file-deletion file)
                  (delete-file file))
                (append
                 (find-files (string-append vendor-dir "/doc")
                             "page-Makefile.ri")
                 (find-files (string-append vendor-dir "/extensions")
                             "gem_make.out")
                 (find-files (string-append gem-dir "/ext")
                             "Makefile"))))

    #t))

(define* (wrap-ruby-program prog #:key (gem-clear-paths #t) #:rest vars)
  "Make a wrapper for PROG.  VARS should look like this:

  '(VARIABLE DELIMITER POSITION LIST-OF-DIRECTORIES)

where DELIMITER is optional.  ':' will be used if DELIMITER is not given.

For example, this command:

  (wrap-ruby-program \"foo\"
                '(\"PATH\" \":\" = (\"/gnu/.../bar/bin\"))
                '(\"CERT_PATH\" suffix (\"/gnu/.../baz/certs\"
                                        \"/qux/certs\")))

will copy 'foo' to '.real/fool' and create the file 'foo' with the following
contents:

  #!location/of/bin/ruby
  ENV['PATH'] = \"/gnu/.../bar/bin\"
  ENV['CERT_PATH'] = (ENV.key?('CERT_PATH') ? (ENV['CERT_PATH'] + ':') : '') + '/gnu/.../baz/certs:/qux/certs'
  load location/of/.real/foo

This is useful for scripts that expect particular programs to be in $PATH, for
programs that expect particular gems to be in the GEM_PATH.

This is preferable to wrap-program, which uses a bash script, as this prevents
ruby scripts from being executed with @command{ruby -S ...}.

If PROG has previously been wrapped by 'wrap-ruby-program', the wrapper is
extended with definitions for VARS."
  (define wrapped-file
    (string-append (dirname prog) "/.real/" (basename prog)))

  (define already-wrapped?
    (file-exists? wrapped-file))

  (define (last-line port)
    ;; Return the last line read from PORT and leave PORT's cursor right
    ;; before it.
    (let loop ((previous-line-offset 0)
               (previous-line "")
               (position (seek port 0 SEEK_CUR)))
      (match (read-line port 'concat)
        ((? eof-object?)
         (seek port previous-line-offset SEEK_SET)
         previous-line)
        ((? string? line)
         (loop position line (+ (string-length line) position))))))

  (define (export-variable lst)
    ;; Return a string that exports an environment variable.
    (match lst
      ((var sep '= rest)
       (format #f "ENV['~a'] = '~a'"
               var (string-join rest sep)))
      ((var sep 'prefix rest)
       (format #f "ENV['~a'] = '~a' + (ENV.key?('~a') ? ('~a' + ENV['~a']) : '')"
               var (string-join rest sep) var sep var))
      ((var sep 'suffix rest)
       (format #f "ENV['~a'] = (ENV.key?('~a') ? (ENV['~a'] + '~a') : '') + '~a'"
               var var var sep (string-join rest sep)))
      ((var '= rest)
       (format #f "ENV['~a'] = '~a'"
               var (string-join rest ":")))
      ((var 'prefix rest)
       (format #f "ENV['~a'] = '~a' + (ENV.key?('~a') ? (':' + ENV['~a']) : '')"
               var (string-join rest ":") var var))
      ((var 'suffix rest)
       (format #f "ENV['~a'] = (ENV.key?('~a') ? (ENV['~a'] + ':') : '') + '~a'"
               var var var (string-join rest ":")))))

  (if already-wrapped?

      ;; PROG is already a wrapper: add the new "export VAR=VALUE" lines just
      ;; before the last line.
      (let* ((port (open-file prog "r+"))
             (last (last-line port)))
        (for-each (lambda (var)
                    (display (export-variable var) port)
                    (newline port))
                  vars)
        (display last port)
        (close-port port))

      ;; PROG is not wrapped yet: create a shell script that sets VARS.
      (let ((prog-tmp (string-append wrapped-file "-tmp")))
        (mkdir-p (dirname prog-tmp))
        (link prog wrapped-file)

        (call-with-output-file prog-tmp
          (lambda (port)
            (format port
                    "#!~a~%~a~%~a~%load '~a'~%"
                    (which "ruby")
                    (string-join (map export-variable vars) "\n")
                    ;; This ensures that if the GEM_PATH has been changed,
                    ;; then that change will be noticed.
                    (if gem-clear-paths "Gem.clear_paths" "")
                    (canonicalize-path wrapped-file))))

        (chmod prog-tmp #o755)
        (rename-file prog-tmp prog))))

(define* (wrap #:key inputs outputs #:allow-other-keys)
  (define (list-of-files dir)
    (map (cut string-append dir "/" <>)
         (or (scandir dir (lambda (f)
                            (let ((s (stat (string-append dir "/" f))))
                              (eq? 'regular (stat:type s)))))
             '())))

  (define bindirs
    (append-map (match-lambda
                 ((_ . dir)
                  (list (string-append dir "/bin")
                        (string-append dir "/sbin"))))
                outputs))

  (let* ((out  (assoc-ref outputs "out"))
         (var `("GEM_PATH" prefix
                (,(string-append out "/lib/ruby/vendor_ruby")
                 ,(getenv "GEM_PATH")))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-ruby-program <> var)
                            files)))
              bindirs))
  #t)

(define (log-file-deletion file)
  (display (string-append "deleting '" file "' for reproducibility\n")))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'unpack unpack)
    (add-before 'build 'extract-gemspec extract-gemspec)
    (add-after 'extract-gemspec 'replace-git-ls-files replace-git-ls-files)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'wrap wrap)))

(define* (ruby-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
