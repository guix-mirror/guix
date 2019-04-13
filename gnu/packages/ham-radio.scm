;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages ham-radio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public rtl-sdr
  (package
    (name "rtl-sdr")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cgit.osmocom.org/rtl-sdr/snapshot/rtl-sdr-"
                           version ".tar.xz"))
       (sha256
        (base32
         "08awca3v28sa4lxym4r81pzf0la0j86wbmpyhv3xd53an9gkpjy9"))))
    (build-system cmake-build-system)
    (inputs
     `(("libusb" ,libusb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags '("-DDETACH_KERNEL_DRIVER=ON")
       #:tests? #f)) ; No tests
    (home-page "https://osmocom.org/projects/sdr/wiki/rtl-sdr")
    (synopsis "Software defined radio driver for Realtek RTL2832U")
    (description "DVB-T dongles based on the Realtek RTL2832U can be used as a
cheap software defined radio, since the chip allows transferring the raw I/Q
samples to the host.  @code{rtl-sdr} provides drivers for this purpose.")
    (license license:gpl2+)))

(define-public chirp
  (package
    (name "chirp")
    (version "20181205")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://trac.chirp.danplanet.com/chirp_daily/daily-"
                           version "/chirp-daily-" version ".tar.gz"))
       (sha256
        (base32
         "1cp280b95j39xaxs50zn55jigg7pyfpm9n098hmsyxrplqn8z43c"))))
    (build-system python-build-system)
    (inputs
     `(("python2-libxml2" ,python2-libxml2)
       ("python2-pygtk" ,python2-pygtk)
       ("python2-pyserial" ,python2-pyserial)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://chirp.danplanet.com")
    (synopsis "Cross-radio programming tool")
    (description "Chirp is a cross-radio programming tool.  It supports a
growing list of radios across several manufacturers and allows transferring of
memory contents between them.")
    (license (list license:gpl3+
                   license:lgpl3+)))) ; chirp/elib_intl.py

(define-public aptdec
  (package
    (name "aptdec")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csete/aptdec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hf0zb51qc6fyhdjxyij1n3vgwnw3cwksc3r11szbhkml14qjnzk"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("libsndfile" ,libsndfile)))
    (arguments
     `(#:make-flags (list "CC=gcc")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "atpdec" (string-append out "/bin")))
             #t)))))
    (home-page "https://github.com/csete/aptdec")
    (synopsis "NOAA Automatic Picture Transmission (APT) decoder")
    (description "Aptdec decodes Automatic Picture Transmission (APT) images.
These are medium resolution images of the Earth transmitted by, among other
satellites, the POES NOAA weather satellite series.  These transmissions are
on a frequency of 137 MHz.  They can be received using an inexpensive antenna
and a dedicated receiver.")
    (license license:gpl2+)))
