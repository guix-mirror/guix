;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix build minetest-build-system)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 exceptions)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((guix build copy-build-system) #:prefix copy:)
  #:export (%standard-phases
            mod-install-plan minimise-png read-mod-name check))

;; (guix build copy-build-system) does not export 'install'.
(define copy:install
  (assoc-ref copy:%standard-phases 'install))

(define (mod-install-plan mod-name)
  `(("." ,(string-append "share/minetest/mods/" mod-name)
     ;; Only install files that will actually be used at run time.
     ;; This can save a little disk space.
     ;;
     ;; See <https://github.com/minetest/minetest/blob/master/doc/lua_api.txt>
     ;; for an incomple list of files that can be found in mods.
     #:include ("mod.conf" "modpack.conf" "settingtypes.txt" "depends.txt"
                "description.txt" "config.txt" "_config.txt")
     #:include-regexp (".lua$" ".png$" ".ogg$" ".obj$" ".b3d$" ".tr$"
                       ".mts$"))))

(define* (guess-mod-name #:key inputs #:allow-other-keys)
  "Try to determine the name of the mod or modpack that is being built.
If it is unknown, make an educated guess."
  ;; Minetest doesn't care about the directory names in "share/minetest/mods"
  ;; so there is no technical problem if the directory names don't match
  ;; the mod names.  The directory can appear in the GUI if the modpack
  ;; doesn't have the 'name' set though, so try to make a guess.
  (define (guess)
    (let* ((source (assoc-ref inputs "source"))
           ;; Don't retain a reference to the store.
           (file-name (strip-store-file-name source))
           ;; The "minetest-" prefix is not informative, so strip it.
           (file-name (if (string-prefix? "minetest-" file-name)
                          (substring file-name (string-length "minetest-"))
                          file-name))
           ;; Strip "-checkout" suffixes of git checkouts.
           (file-name (if (string-suffix? "-checkout" file-name)
                          (substring file-name
                                     0
                                     (- (string-length file-name)
                                        (string-length "-checkout")))
                          file-name))
           (first-dot (string-index file-name #\.))
           ;; If the source code is in an archive (.tar.gz, .zip, ...),
           ;; strip the extension.
           (file-name (if first-dot
                          (substring file-name 0 first-dot)
                          file-name)))
      (format (current-error-port)
              "warning: the modpack ~a did not set 'name' in 'modpack.conf'~%"
              file-name)
      file-name))
  (cond ((file-exists? "mod.conf")
         ;; Mods must have 'name' set in "mod.conf", so don't guess.
         (read-mod-name "mod.conf"))
        ((file-exists? "modpack.conf")
         ;; While it is recommended to have 'name' set in 'modpack.conf',
         ;; it is optional, so guess a name if necessary.
         (read-mod-name "modpack.conf" guess))
        (#t (guess))))

(define* (install #:key inputs #:allow-other-keys #:rest arguments)
  (apply copy:install
         #:install-plan (mod-install-plan (apply guess-mod-name arguments))
         arguments))

(define %png-magic-bytes
  ;; Magic bytes of PNG images, see ‘5.2 PNG signatures’ in
  ;; ‘Portable Network Graphics (PNG) Specification (Second Edition)’
  ;; on <https://www.w3.org/TR/PNG/>.
  #vu8(137 80 78 71 13 10 26 10))

(define png-file?
  ((@@ (guix build utils) file-header-match) %png-magic-bytes))

(define* (minimise-png #:key inputs native-inputs #:allow-other-keys)
  "Minimise PNG images found in the working directory."
  (define optipng (which "optipng"))
  (define (optimise image)
    (format #t "Optimising ~a~%" image)
    (make-file-writable (dirname image))
    (make-file-writable image)
    (define old-size (stat:size (stat image)))
    ;; The mod "technic" has a file "technic_music_player_top.png" that
    ;; actually is a JPEG file, see
    ;; <https://github.com/minetest-mods/technic/issues/590>.
    (if (png-file? image)
        (invoke optipng "-o4" "-quiet" image)
        (format #t "warning: skipping ~a because it's not actually a PNG image~%"
                image))
    (define new-size (stat:size (stat image)))
    (values old-size new-size))
  (define files (find-files "." ".png$"))
  (let loop ((total-old-size 0)
             (total-new-size 0)
             (images (find-files "." ".png$")))
    (cond ((pair? images)
           (receive (old-size new-size)
               (optimise (car images))
             (loop (+ total-old-size old-size)
                   (+ total-new-size new-size)
                   (cdr images))))
          ((= total-old-size 0)
           (format #t "There were no PNG images to minimise."))
          (#t
           (format #t "Minimisation reduced size of images by ~,2f% (~,2f MiB to ~,2f MiB)~%"
                   (* 100.0 (- 1 (/ total-new-size total-old-size)))
                   (/ total-old-size (expt 1024 2))
                   (/ total-new-size (expt 1024 2)))))))

(define name-regexp (make-regexp "^name[ ]*=(.+)$"))

(define* (read-mod-name mod.conf #:optional not-found)
  "Read the name of a mod from MOD.CONF.  If MOD.CONF
does not have a name field and NOT-FOUND is #false, raise an
error.  If NOT-FOUND is TRUE, call NOT-FOUND instead."
  (call-with-input-file mod.conf
    (lambda (port)
      (let loop ()
        (define line (read-line port))
        (if (eof-object? line)
            (if not-found
                (not-found)
                (error "~a does not have a 'name' field" mod.conf))
            (let ((match (regexp-exec name-regexp line)))
              (if (regexp-match? match)
                  (string-trim-both (match:substring match 1) #\ )
                  (loop))))))))

(define* (check #:key outputs tests? #:allow-other-keys)
  "Test whether the mod loads.  The mod must first be installed first."
  (define (all-mod-names directories)
    (append-map
     (lambda (directory)
       (map read-mod-name (find-files directory "mod.conf")))
     directories))
  (when tests?
    (mkdir "guix_testworld")
    ;; Add the mod to the mod search path, such that Minetest can find it.
    (setenv "MINETEST_MOD_PATH"
            (list->search-path-as-string
             (cons
              (string-append (assoc-ref outputs "out") "/share/minetest/mods")
              (search-path-as-string->list
               (or (getenv "MINETEST_MOD_PATH") "")))
             ":"))
    (with-directory-excursion "guix_testworld"
      (setenv "HOME" (getcwd))
      ;; Create a world in which all mods are loaded.
      (call-with-output-file "world.mt"
        (lambda (port)
          (display
           "gameid = minetest
world_name = guix_testworld
backend = sqlite3
player_backend = sqlite3
auth_backend = sqlite3
" port)
          (for-each
           (lambda (mod)
             (format port "load_mod_~a = true~%" mod))
           (all-mod-names (search-path-as-string->list
                           (getenv "MINETEST_MOD_PATH"))))))
      (receive (port pid)
          ((@@ (guix build utils) open-pipe-with-stderr)
           "xvfb-run" "--" "minetest" "--info" "--world" "." "--go")
        (format #t "Started Minetest with all mods loaded for testing~%")
        ;; Scan the output for error messages.
        ;; When the player has joined the server, stop minetest.
        (define (error? line)
          (and (string? line)
               (string-contains line ": ERROR[")))
        (define (stop? line)
          (and (string? line)
               (string-contains line "ACTION[Server]: singleplayer [127.0.0.1] joins game.")))
        (let loop ((has-errors? #f))
          (match `(,(read-line port) ,has-errors?)
            (((? error? line) _)
             (display line)
             (newline)
             (loop #t))
            (((? stop?) #f)
             (kill pid SIGINT)
             (close-port port)
             (waitpid pid))
            (((? eof-object?) #f)
             (error "minetest didn't start"))
            (((or (? stop?) (? eof-object?)) #t)
             (error "minetest raised an error"))
            (((? string? line) has-error?)
             (display line)
             (newline)
             (loop has-error?))))))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (add-before 'build 'minimise-png minimise-png)
    (delete 'build)
    (delete 'check)
    (replace 'install install)
    ;; The 'check' phase requires the mod to be installed,
    ;; so move the 'check' phase after the 'install' phase.
    (add-after 'install 'check check)))

;;; minetest-build-system.scm ends here
