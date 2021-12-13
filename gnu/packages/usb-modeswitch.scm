;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages usb-modeswitch)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config))

(define-public usb-modeswitch-data
  (package
    (name "usb-modeswitch-data")
    (version "20191128")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.draisberghof.de/usb_modeswitch/"
                    "usb-modeswitch-data-" version ".tar.bz2"))
              (sha256
               (base32
                "1ygahl3r26r38ai8yyblq9nhf3v5i6n6r6672p5wf88wg5h9n0rz"))))
    (build-system trivial-build-system)
    (native-inputs (list tar bzip2))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (tar (assoc-ref %build-inputs "tar"))
                (bzip2 (assoc-ref %build-inputs "bzip2"))
                (files (string-append "usb-modeswitch-data-"
                                      ,(package-version this-package)))
                (share-dir (string-append %output "/share"))
                (doc-dir (string-append share-dir "/doc/"))
                (license-dir (string-append doc-dir
                                            (strip-store-file-name %output)))
                (udev-dir (string-append %output "/udev")))
           (copy-file source "data.tar.bz2")
           (invoke (string-append bzip2 "/bin/bzip2") "-d" "data.tar.bz2")
           (invoke (string-append tar "/bin/tar") "xvf" "data.tar")
           (copy-recursively (string-append files "/usb_modeswitch.d")
                             (string-append share-dir "/usb_modeswitch.d"))
           (install-file (string-append files "/40-usb_modeswitch.rules")
                         udev-dir)
           (install-file (string-append files "/COPYING") license-dir)))))
    (home-page "https://www.draisberghof.de/usb_modeswitch/")
    (synopsis "Data package for USB_ModeSwitch")
    (description "This package contains data about devices and a UDEV rules
file for use with USB_ModeSwitch.")
    (license license:gpl2+)))

(define-public usb-modeswitch
  (package
    (name "usb-modeswitch")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.draisberghof.de/usb_modeswitch/"
                    "usb-modeswitch-" version ".tar.bz2"))
              (sha256
               (base32
                "0d7s8p92r36danjd15y1zaznf6rbk17kxyg9367nabz56vhxk5ai"))))
    (native-inputs (list pkg-config))
    (inputs (list libusb jimtcl usb-modeswitch-data))
    (outputs '("out" "dispatcher"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                    ; does not support `make check`
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)          ; no configure script
         (replace 'install
           (lambda* (#:key source outputs inputs #:allow-other-keys)
             (let* ((source (assoc-ref inputs "source"))
                    (jimtcl (assoc-ref inputs "jimtcl"))
                    (data (assoc-ref inputs "usb-modeswitch-data"))
                    (out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1"))
                    (dispatcher-out (assoc-ref outputs "dispatcher"))
                    (udev (string-append dispatcher-out "/lib/udev"))
                    (etc (string-append dispatcher-out "/etc"))
                    (dispatcher-bin (string-append dispatcher-out "/bin"))
                    (dispatcher-man1 (string-append dispatcher-out
                                                    "/share/man/man1")))
               (begin
                 ;; Users can install the default output and
                 ;; usb-modeswitch-data and then modeswitch their USB device
                 ;; by running e.g.:
                 ;;
                 ;;   sudo usb_modeswitch -c \
                 ;;   ~/.guix-profile/share/usb_modeswitch.d/12d1\:14fe \
                 ;;   -v 0x12d1 -p 0x14fe
                 ;;
                 ;; But it is simpler to use the usb-modeswitch-service-type
                 ;; that installs a UDEV rules file which invokes a shell
                 ;; script in lib/udev (also called `usb_modeswitch' like the
                 ;; main binary) which, in turn, invokes the program
                 ;; `usb_modeswitch_dispatcher'.  Normal users should not
                 ;; invoke this dispatcher directly, so it is a separate output.
                 (install-file "usb_modeswitch" bin)
                 (install-file "usb_modeswitch.conf" etc)
                 (install-file "usb_modeswitch.1" man1)
                 (install-file "usb_modeswitch_dispatcher.1" dispatcher-man1)

                 (substitute* "usb_modeswitch.sh"
                   (("PATH=") "PATH=$PATH:") ; we do not want hardcoded FHS path
                   (("init_path=") "init_path=/does/not/exist")) ; no /sbin/init
                 (rename-file "usb_modeswitch.sh" "usb_modeswitch")
                 (install-file "usb_modeswitch" udev)

                 (rename-file "usb_modeswitch_dispatcher.tcl" "usb_modeswitch_dispatcher")
                 (substitute* "usb_modeswitch_dispatcher"
                   (("/usr/bin/tclsh")
                    (string-append jimtcl "/bin/jimsh"))
                   (("/usr/sbin") bin)
                   (("/usr/share/usb_modeswitch")
                    (string-append data "/share/usb_modeswitch.d")))
                 (install-file "usb_modeswitch_dispatcher"
                               dispatcher-bin)
                 #t)))))))
    (home-page "https://www.draisberghof.de/usb_modeswitch/")
    (synopsis "Mode switching tool for controlling `multi-mode' USB devices")
    (description "USB_ModeSwitch is a mode switching tool for controlling USB
devices with multiple @dfn{modes}.  When plugged in for the first time many
USB devices (primarily high-speed WAN modems) act like a flash storage
containing installers for Windows drivers.  USB_ModeSwitch replays the
sequence the Windows drivers would send to switch their mode from storage to
modem (or whatever the thing is supposed to do).")
    (license (list license:gpl2+ ;"this program" according to home page
                   license:bsd-2)))) ;dispatcher.c
