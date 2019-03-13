;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017, 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((guix modules) #:hide (file-name->module-name))
  #:use-module (guix i18n)
  #:use-module ((guix utils)
                #:select (source-properties->location
                          &fix-hint
                          &error-location))
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system uuid)
  #:autoload   (gnu build file-systems) (find-partition-by-luks-uuid)
  #:autoload   (gnu build linux-modules)
                 (device-module-aliases matching-modules known-module-aliases
                  normalize-module-name file-name->module-name)
  #:autoload   (gnu packages cryptsetup) (cryptsetup-static)
  #:autoload   (gnu packages linux) (mdadm-static)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (mapped-device
            mapped-device?
            mapped-device-source
            mapped-device-target
            mapped-device-type
            mapped-device-location

            mapped-device-kind
            mapped-device-kind?
            mapped-device-kind-open
            mapped-device-kind-close
            mapped-device-kind-check

            device-mapping-service-type
            device-mapping-service

            check-device-initrd-modules           ;XXX: needs a better place

            luks-device-mapping
            raid-device-mapping))

;;; Commentary:
;;;
;;; This module supports "device mapping", a concept implemented by Linux's
;;; device-mapper.
;;;
;;; Code:

(define-record-type* <mapped-device> mapped-device
  make-mapped-device
  mapped-device?
  (source    mapped-device-source)                ;string | list of strings
  (target    mapped-device-target)                ;string
  (type      mapped-device-type)                  ;<mapped-device-kind>
  (location  mapped-device-location
             (default (current-source-location)) (innate)))

(define-record-type* <mapped-device-type> mapped-device-kind
  make-mapped-device-kind
  mapped-device-kind?
  (open      mapped-device-kind-open)             ;source target -> gexp
  (close     mapped-device-kind-close             ;source target -> gexp
             (default (const #~(const #f))))
  (check     mapped-device-kind-check             ;source -> Boolean
             (default (const #t))))


;;;
;;; Device mapping as a Shepherd service.
;;;

(define device-mapping-service-type
  (shepherd-service-type
   'device-mapping
   (match-lambda
     (($ <mapped-device> source target
                         ($ <mapped-device-type> open close))
      (shepherd-service
       (provision (list (symbol-append 'device-mapping- (string->symbol target))))
       (requirement '(udev))
       (documentation "Map a device node using Linux's device mapper.")
       (start #~(lambda () #$(open source target)))
       (stop #~(lambda _ (not #$(close source target))))
       (respawn? #f))))))

(define (device-mapping-service mapped-device)
  "Return a service that sets up @var{mapped-device}."
  (service device-mapping-service-type mapped-device))


;;;
;;; Static checks.
;;;

(define (check-device-initrd-modules device linux-modules location)
  "Raise an error if DEVICE needs modules beyond LINUX-MODULES to operate.
DEVICE must be a \"/dev\" file name."
  (define aliases
    ;; Attempt to load 'modules.alias' from the current kernel, assuming we're
    ;; on Guix System, and assuming that corresponds to the kernel we'll be
    ;; installing.  Skip the whole thing if that file cannot be read.
    (catch 'system-error
      (lambda ()
        (known-module-aliases))
      (const #f)))

  (when aliases
    (let* ((modules  (delete-duplicates
                      (append-map (cut matching-modules <> aliases)
                                  (device-module-aliases device))))

           ;; Module names (not file names) are supposed to use underscores
           ;; instead of hyphens.  MODULES is a list of module names, whereas
           ;; LINUX-MODULES is file names without '.ko', so normalize them.
           (provided (map file-name->module-name linux-modules))
           (missing  (remove (cut member <> provided) modules)))
      (unless (null? missing)
        ;; Note: What we suggest here is a list of module names (e.g.,
        ;; "usb_storage"), not file names (e.g., "usb-storage.ko").  This is
        ;; OK because we have machinery that accepts both the hyphen and the
        ;; underscore version.
        (raise (condition
                (&message
                 (message (format #f (G_ "you may need these modules \
in the initrd for ~a:~{ ~a~}")
                                  device missing)))
                (&fix-hint
                 (hint (format #f (G_ "Try adding them to the
@code{initrd-modules} field of your @code{operating-system} declaration, along
these lines:

@example
 (operating-system
   ;; @dots{}
   (initrd-modules (append (list~{ ~s~})
                           %base-initrd-modules)))
@end example

If you think this diagnostic is inaccurate, use the @option{--skip-checks}
option of @command{guix system}.\n")
                               missing)))
                (&error-location
                 (location (source-properties->location location)))))))))


;;;
;;; Common device mappings.
;;;

(define (open-luks-device source target)
  "Return a gexp that maps SOURCE to TARGET as a LUKS device, using
'cryptsetup'."
  (with-imported-modules (source-module-closure
                          '((gnu build file-systems)))
    #~(let ((source #$(if (uuid? source)
                          (uuid-bytevector source)
                          source)))
        ;; XXX: 'use-modules' should be at the top level.
        (use-modules (rnrs bytevectors)           ;bytevector?
                     ((gnu build file-systems)
                      #:select (find-partition-by-luks-uuid)))

        ;; Use 'cryptsetup-static', not 'cryptsetup', to avoid pulling the
        ;; whole world inside the initrd (for when we're in an initrd).
        (zero? (system* #$(file-append cryptsetup-static "/sbin/cryptsetup")
                        "open" "--type" "luks"

                        ;; Note: We cannot use the "UUID=source" syntax here
                        ;; because 'cryptsetup' implements it by searching the
                        ;; udev-populated /dev/disk/by-id directory but udev may
                        ;; be unavailable at the time we run this.
                        (if (bytevector? source)
                            (or (let loop ((tries-left 10))
                                  (and (positive? tries-left)
                                       (or (find-partition-by-luks-uuid source)
                                           ;; If the underlying partition is
                                           ;; not found, try again after
                                           ;; waiting a second, up to ten
                                           ;; times.  FIXME: This should be
                                           ;; dealt with in a more robust way.
                                           (begin (sleep 1)
                                                  (loop (- tries-left 1))))))
                                (error "LUKS partition not found" source))
                            source)

                        #$target)))))

(define (close-luks-device source target)
  "Return a gexp that closes TARGET, a LUKS device."
  #~(zero? (system* #$(file-append cryptsetup-static "/sbin/cryptsetup")
                    "close" #$target)))

(define* (check-luks-device md #:key
                            needed-for-boot?
                            (initrd-modules '())
                            #:allow-other-keys
                            #:rest rest)
  "Ensure the source of MD is valid."
  (let ((source   (mapped-device-source md))
        (location (mapped-device-location md)))
    (or (not (zero? (getuid)))
        (if (uuid? source)
            (match (find-partition-by-luks-uuid (uuid-bytevector source))
              (#f
               (raise (condition
                       (&message
                        (message (format #f (G_ "no LUKS partition with UUID '~a'")
                                         (uuid->string source))))
                       (&error-location
                        (location (source-properties->location
                                   (mapped-device-location md)))))))
              ((? string? device)
               (check-device-initrd-modules device initrd-modules location)))
            (check-device-initrd-modules source initrd-modules location)))))

(define luks-device-mapping
  ;; The type of LUKS mapped devices.
  (mapped-device-kind
   (open open-luks-device)
   (close close-luks-device)
   (check check-luks-device)))

(define (open-raid-device sources target)
  "Return a gexp that assembles SOURCES (a list of devices) to the RAID device
TARGET (e.g., \"/dev/md0\"), using 'mdadm'."
  #~(let ((sources '#$sources)

          ;; XXX: We're not at the top level here.  We could use a
          ;; non-top-level 'use-modules' form but that doesn't work when the
          ;; code is eval'd, like the Shepherd does.
          (every   (@ (srfi srfi-1) every))
          (format  (@ (ice-9 format) format)))
      (let loop ((attempts 0))
        (unless (every file-exists? sources)
          (when (> attempts 20)
            (error "RAID devices did not show up; bailing out"
                   sources))

          (format #t "waiting for RAID source devices~{ ~a~}...~%"
                  sources)
          (sleep 1)
          (loop (+ 1 attempts))))

      ;; Use 'mdadm-static' rather than 'mdadm' to avoid pulling its whole
      ;; closure (80 MiB) in the initrd when a RAID device is needed for boot.
      (zero? (apply system* #$(file-append mdadm-static "/sbin/mdadm")
                    "--assemble" #$target sources))))

(define (close-raid-device sources target)
  "Return a gexp that stops the RAID device TARGET."
  #~(zero? (system* #$(file-append mdadm-static "/sbin/mdadm")
                    "--stop" #$target)))

(define raid-device-mapping
  ;; The type of RAID mapped devices.
  (mapped-device-kind
   (open open-raid-device)
   (close close-raid-device)))

;;; mapped-devices.scm ends here
