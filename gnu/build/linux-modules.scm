;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu build linux-modules)
  #:use-module (guix elf)
  #:use-module (guix glob)
  #:use-module (guix build syscalls)
  #:use-module ((guix build utils) #:select (find-files))
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (dot-ko
            ensure-dot-ko
            module-aliases
            module-dependencies
            module-soft-dependencies
            normalize-module-name
            file-name->module-name
            find-module-file
            recursive-module-dependencies
            modules-loaded
            module-loaded?
            load-linux-module*

            current-module-debugging-port

            device-module-aliases
            known-module-aliases
            matching-modules))

;;; Commentary:
;;;
;;; Tools to deal with Linux kernel modules.
;;;
;;; Code:

(define current-module-debugging-port
  (make-parameter (%make-void-port "w")))

(define (section-contents elf section)
  "Return the contents of SECTION in ELF as a bytevector."
  (let ((contents (make-bytevector (elf-section-size section))))
    (bytevector-copy! (elf-bytes elf) (elf-section-offset section)
                      contents 0
                      (elf-section-size section))
    contents))

(define %not-nul
  (char-set-complement (char-set #\nul)))

(define (nul-separated-string->list str)
  "Split STR at occurrences of the NUL character and return the resulting
string list."
  (string-tokenize str %not-nul))

(define (key=value->pair str)
  "Assuming STR has the form \"KEY=VALUE\", return a pair like (KEY
. \"VALUE\")."
  (let ((= (string-index str #\=)))
    (cons (string->symbol (string-take str =))
          (string-drop str (+ 1 =)))))

(define (modinfo-section-contents file)
  "Return the contents of the '.modinfo' section of FILE as a list of
key/value pairs.."
  (let* ((bv      (call-with-input-file file get-bytevector-all))
         (elf     (parse-elf bv))
         (section (elf-section-by-name elf ".modinfo"))
         (modinfo (section-contents elf section)))
    (map key=value->pair
         (nul-separated-string->list (utf8->string modinfo)))))

(define %not-comma
  (char-set-complement (char-set #\,)))

(define (module-dependencies file)
  "Return the list of modules that FILE depends on.  The returned list
contains module names, not actual file names."
  (let ((info (modinfo-section-contents file)))
    (match (assq 'depends info)
      (('depends . what)
       (string-tokenize what %not-comma)))))

(define not-softdep-whitespace
  (char-set-complement (char-set #\space #\tab)))

(define (module-soft-dependencies file)
  "Return the list of modules that can be preloaded, and then the list of
modules that can be postloaded, of the soft dependencies of module FILE."
  ;; TEXT: "pre: baz blubb foo post: bax bar"
  (define (parse-softdep text)
    (let loop ((value '())
               (tokens (string-tokenize text not-softdep-whitespace))
               (section #f))
      (match tokens
       ((token rest ...)
        (if (string=? (string-take-right token 1) ":") ; section
            (loop value rest (string-trim-both (string-drop-right token 1)))
            (loop (cons (cons section token) value) rest section)))
       (()
        value))))

  ;; Note: Multiple 'softdep sections are allowed.
  (let* ((info (modinfo-section-contents file))
         (entries (concatenate
                   (filter-map (match-lambda
                                (('softdep . value)
                                 (parse-softdep value))
                                (_ #f))
                               (modinfo-section-contents file)))))
    (let-values (((pres posts)
                  (partition (match-lambda
                              (("pre" . _) #t)
                              (("post" . _) #f))
                             entries)))
      (values (map (match-lambda
                    ((_ . value) value))
                   pres)
              (map (match-lambda
                    ((_ . value) value))
                   posts)))))

(define (module-aliases file)
  "Return the list of aliases of module FILE."
  (let ((info (modinfo-section-contents file)))
    (filter-map (match-lambda
                 (('alias . value)
                  value)
                 (_ #f))
                (modinfo-section-contents file))))

(define dot-ko
  (cut string-append <> ".ko"))

(define (ensure-dot-ko name)
  "Return NAME with a '.ko' prefix appended, unless it already has it."
  (if (string-suffix? ".ko" name)
      name
      (dot-ko name)))

(define (normalize-module-name module)
  "Return the \"canonical\" name for MODULE, replacing hyphens with
underscores."
  ;; See 'modname_normalize' in libkmod.
  (string-map (lambda (chr)
                (case chr
                  ((#\-) #\_)
                  (else chr)))
              module))

(define (file-name->module-name file)
  "Return the module name corresponding to FILE, stripping the trailing '.ko'
and normalizing it."
  (normalize-module-name (basename file ".ko")))

(define (find-module-file directory module)
  "Lookup module NAME under DIRECTORY, and return its absolute file name.
NAME can be a file name with or without '.ko', or it can be a module name.
Raise an error if it could not be found.

Module names can differ from file names in interesting ways; for instance,
module names usually (always?) use underscores as the inter-word separator,
whereas file names often, but not always, use hyphens.  Examples:
\"usb-storage.ko\", \"serpent_generic.ko\"."
  (define names
    ;; List of possible file names.  XXX: It would of course be cleaner to
    ;; have a database that maps module names to file names and vice versa,
    ;; but everyone seems to be doing hacks like this one.  Oh well!
    (map ensure-dot-ko
         (delete-duplicates
          (list module
                (normalize-module-name module)
                (string-map (lambda (chr) ;converse of 'normalize-module-name'
                              (case chr
                                ((#\_) #\-)
                                (else chr)))
                            module)))))

  (match (find-files directory
                     (lambda (file stat)
                       (member (basename file) names)))
    ((file)
     file)
    (()
     (error "kernel module not found" module directory))
    ((_ ...)
     (error "several modules by that name" module directory))))

(define* (recursive-module-dependencies files
                                        #:key (lookup-module dot-ko))
  "Return the topologically-sorted list of file names of the modules depended
on by FILES, recursively.  File names of modules are determined by applying
LOOKUP-MODULE to the module name."
  (let loop ((files   files)
             (result  '())
             (visited vlist-null))
    (match files
      (()
       (delete-duplicates (reverse result)))
      ((head . tail)
       (let* ((visited? (vhash-assoc head visited))
              (deps     (if visited?
                            '()
                            (map lookup-module (module-dependencies head))))
              (visited  (if visited?
                            visited
                            (vhash-cons head #t visited))))
         (loop (append deps tail)
               (append result deps) visited))))))

(define %not-newline
  (char-set-complement (char-set #\newline)))

(define (modules-loaded)
  "Return the list of names of currently loaded Linux modules."
  (let* ((contents (call-with-input-file "/proc/modules"
                     get-string-all))
         (lines    (string-tokenize contents %not-newline)))
    (match (map string-tokenize lines)
      (((modules . _) ...)
       modules))))

(define (module-black-list)
  "Return the black list of modules that must not be loaded.  This black list
is specified using 'modprobe.blacklist=MODULE1,MODULE2,...' on the kernel
command line; it is honored by libkmod for users that pass
'KMOD_PROBE_APPLY_BLACKLIST', which includes 'modprobe --use-blacklist' and
udev."
  (define parameter
    "modprobe.blacklist=")

  (let ((command (call-with-input-file "/proc/cmdline"
                   get-string-all)))
    (append-map (lambda (arg)
                  (if (string-prefix? parameter arg)
                      (string-tokenize (string-drop arg (string-length parameter))
                                       %not-comma)
                      '()))
                (string-tokenize command))))

(define (module-loaded? module)
  "Return #t if MODULE is already loaded.  MODULE must be a Linux module name,
not a file name."
  (member module (modules-loaded)))

(define* (load-linux-module* file
                             #:key
                             (recursive? #t)
                             (lookup-module dot-ko)
                             (black-list (module-black-list)))
  "Load Linux module from FILE, the name of a '.ko' file; return true on
success, false otherwise.  When RECURSIVE? is true, load its dependencies
first (à la 'modprobe'.)  The actual files containing modules depended on are
obtained by calling LOOKUP-MODULE with the module name.  Modules whose name
appears in BLACK-LIST are not loaded."
  (define (black-listed? module)
    (let ((result (member module black-list)))
      (when result
        (format (current-module-debugging-port)
                "not loading module '~a' because it's black-listed~%"
                module))
      result))

  (define (load-dependencies file)
    (let ((dependencies (module-dependencies file)))
      (every (cut load-linux-module* <>
                  #:lookup-module lookup-module
                  #:black-list black-list)
             (map lookup-module dependencies))))

  (and (not (black-listed? (file-name->module-name file)))
       (or (not recursive?)
           (load-dependencies file))
       (let ((fd #f))
         (format (current-module-debugging-port)
                 "loading Linux module from '~a'...~%" file)

         (catch 'system-error
           (lambda ()
             (set! fd (open-fdes file O_RDONLY))
             (load-linux-module/fd fd)
             (close-fdes fd)
             #t)
           (lambda args
             ;; If this module was already loaded and we're in modprobe style, ignore
             ;; the error.
             (when fd (close-fdes fd))
             (or (and recursive? (= EEXIST (system-error-errno args)))
                 (apply throw args)))))))


;;;
;;; Device modules.
;;;

;; Copied from (guix utils).  FIXME: Factorize.
(define (readlink* file)
  "Call 'readlink' until the result is not a symlink."
  (define %max-symlink-depth 50)

  (let loop ((file  file)
             (depth 0))
    (define (absolute target)
      (if (absolute-file-name? target)
          target
          (string-append (dirname file) "/" target)))

    (if (>= depth %max-symlink-depth)
        file
        (call-with-values
            (lambda ()
              (catch 'system-error
                (lambda ()
                  (values #t (readlink file)))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (if (or (= errno EINVAL))
                        (values #f file)
                        (apply throw args))))))
          (lambda (success? target)
            (if success?
                (loop (absolute target) (+ depth 1))
                file))))))

;; See 'major' and 'minor' in <sys/sysmacros.h>.

(define (stat->device-major st)
  (ash (logand #xfff00 (stat:rdev st)) -8))

(define (stat->device-minor st)
  (logand #xff (stat:rdev st)))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (read-uevent port)
  "Read a /sys 'uevent' file from PORT and return an alist where each car is a
key such as 'MAJOR or 'DEVTYPE and each cdr is the corresponding value."
  (let loop ((result '()))
    (match (read-line port)
      ((? eof-object?)
       (reverse result))
      (line
       (loop (cons (key=value->pair line) result))))))

(define (device-module-aliases device)
  "Return the list of module aliases required by DEVICE, a /dev file name, as
in this example:

  (device-module-aliases \"/dev/sda\")
  => (\"scsi:t-0x00\" \"pci:v00008086d00009D03sv0000103Csd000080FAbc01sc06i01\")

The modules corresponding to these aliases can then be found using
'matching-modules'."
  ;; The approach is adapted from
  ;; <https://unix.stackexchange.com/questions/97676/how-to-find-the-driver-module-associated-with-a-device-on-linux>.
  (let* ((st        (stat device))
         (type      (stat:type st))
         (major     (stat->device-major st))
         (minor     (stat->device-minor st))
         (sys-name  (string-append "/sys/dev/"
                                   (case type
                                     ((block-special) "block")
                                     ((char-special)  "char")
                                     (else (symbol->string type)))
                                   "/" (number->string major) ":"
                                   (number->string minor)))
         (directory (canonicalize-path (readlink* sys-name))))
    (let loop ((components (string-tokenize directory %not-slash))
               (aliases    '()))
      (match components
        (("sys" "devices" _)
         (reverse aliases))
        ((head ... _)
         (let ((uevent (string-append (string-join components "/" 'prefix)
                                      "/uevent")))
           (if (file-exists? uevent)
               (let ((props (call-with-input-file uevent read-uevent)))
                 (match (assq-ref props 'MODALIAS)
                   (#f    (loop head aliases))
                   (alias (loop head (cons alias aliases)))))
               (loop head aliases))))))))

(define (read-module-aliases port)
  "Read from PORT data in the Linux 'modules.alias' file format.  Return a
list of alias/module pairs where each alias is a glob pattern as like the
result of:

  (string->compiled-sglob \"scsi:t-0x01*\")

and each module is a module name like \"snd_hda_intel\"."
  (define (comment? str)
    (string-prefix? "#" str))

  (define (tokenize str)
    ;; Lines have the form "alias ALIAS MODULE", where ALIAS can contain
    ;; whitespace.  This is why we don't use 'string-tokenize'.
    (let* ((str   (string-trim-both str))
           (left  (string-index str #\space))
           (right (string-rindex str #\space)))
      (list (string-take str left)
            (string-trim-both (substring str left right))
            (string-trim-both (string-drop str right)))))

  (let loop ((aliases '()))
    (match (read-line port)
      ((? eof-object?)
       (reverse aliases))
      ((? comment?)
       (loop aliases))
      (line
       (match (tokenize line)
         (("alias" alias module)
          (loop (alist-cons (string->compiled-sglob alias) module
                            aliases)))
         (()                                      ;empty line
          (loop aliases)))))))

(define (current-kernel-directory)
  "Return the directory of the currently running Linux kernel."
  (string-append (or (getenv "LINUX_MODULE_DIRECTORY")
                     "/run/booted-system/kernel/lib/modules")
                 "/" (utsname:release (uname))))

(define (current-alias-file)
  "Return the absolute file name of the default 'modules.alias' file."
  (string-append (current-kernel-directory) "/modules.alias"))

(define* (known-module-aliases #:optional (alias-file (current-alias-file)))
  "Return the list of alias/module pairs read from ALIAS-FILE.  Each alias is
actually a pattern."
  (call-with-input-file alias-file read-module-aliases))

(define* (matching-modules alias
                           #:optional (known-aliases (known-module-aliases)))
  "Return the list of modules that match ALIAS according to KNOWN-ALIASES.
ALIAS is a string like \"scsi:t-0x00\" as returned by
'device-module-aliases'."
  (filter-map (match-lambda
                ((pattern . module)
                 (and (glob-match? pattern alias)
                      module)))
              known-aliases))

;;; linux-modules.scm ends here
