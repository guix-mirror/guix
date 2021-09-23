;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services pm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:export (tlp-service-type
            tlp-configuration

            thermald-configuration
            thermald-service-type))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split
                  (string-upcase
                   (if (string-suffix? "?" str)
                       (substring str 0 (1- (string-length str)))
                       str))
                  #\-)
                 "_")))

(define (serialize-field field-name val)
  (format #t "~a=~a\n" (uglify-field-name field-name) val))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "1" "0")))
(define-maybe boolean)

(define (serialize-string field-name val)
  (serialize-field field-name val))
(define-maybe string)

(define (space-separated-string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\space))))
                val)))
(define (serialize-space-separated-string-list field-name val)
  (serialize-field field-name
                   (format #f "~s"
                           (string-join val " "))))
(define-maybe space-separated-string-list)

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))
(define (serialize-non-negative-integer field-name val)
  (serialize-field field-name val))
(define-maybe non-negative-integer)

(define (on-off-boolean? val)
  (boolean? val))
(define (serialize-on-off-boolean field-name val)
  (serialize-field field-name (if val "on" "off")))
(define-maybe on-off-boolean)

(define (y-n-boolean? val)
  (boolean? val))
(define (serialize-y-n-boolean field-name val)
  (serialize-field field-name (if val "Y" "N")))

(define-configuration tlp-configuration
  (tlp
   (file-like tlp)
   "The TLP package.")

  (tlp-enable?
   (boolean #t)
   "Set to true if you wish to enable TLP.")

  (tlp-default-mode
   (string "AC")
   "Default mode when no power supply can be detected.  Alternatives are
AC and BAT.")

  (disk-idle-secs-on-ac
   (non-negative-integer 0)
   "Number of seconds Linux kernel has to wait after the disk goes idle,
before syncing on AC.")

  (disk-idle-secs-on-bat
   (non-negative-integer 2)
   "Same as @code{disk-idle-ac} but on BAT mode.")

  (max-lost-work-secs-on-ac
   (non-negative-integer 15)
   "Dirty pages flushing periodicity, expressed in seconds.")

  (max-lost-work-secs-on-bat
   (non-negative-integer 60)
   "Same as @code{max-lost-work-secs-on-ac} but on BAT mode.")

  (cpu-scaling-governor-on-ac
   (maybe-space-separated-string-list 'disabled)
   "CPU frequency scaling governor on AC mode.  With intel_pstate
driver, alternatives are powersave and performance.  With acpi-cpufreq driver,
alternatives are ondemand, powersave, performance and conservative.")

  (cpu-scaling-governor-on-bat
   (maybe-space-separated-string-list 'disabled)
   "Same as @code{cpu-scaling-governor-on-ac} but on BAT mode.")

  (cpu-scaling-min-freq-on-ac
   (maybe-non-negative-integer 'disabled)
   "Set the min available frequency for the scaling governor on AC.")

  (cpu-scaling-max-freq-on-ac
   (maybe-non-negative-integer 'disabled)
   "Set the max available frequency for the scaling governor on AC.")

  (cpu-scaling-min-freq-on-bat
   (maybe-non-negative-integer 'disabled)
   "Set the min available frequency for the scaling governor on BAT.")

  (cpu-scaling-max-freq-on-bat
   (maybe-non-negative-integer 'disabled)
   "Set the max available frequency for the scaling governor on BAT.")

  (cpu-min-perf-on-ac
   (maybe-non-negative-integer 'disabled)
   "Limit the min P-state to control the power dissipation of the CPU,
in AC mode.  Values are stated as a percentage of the available performance.")

  (cpu-max-perf-on-ac
   (maybe-non-negative-integer 'disabled)
   "Limit the max P-state to control the power dissipation of the CPU,
in AC mode.  Values are stated as a percentage of the available performance.")

  (cpu-min-perf-on-bat
   (maybe-non-negative-integer 'disabled)
   "Same as @code{cpu-min-perf-on-ac} on BAT mode.")

  (cpu-max-perf-on-bat
   (maybe-non-negative-integer 'disabled)
   "Same as @code{cpu-max-perf-on-ac} on BAT mode.")

  (cpu-boost-on-ac?
   (maybe-boolean 'disabled)
   "Enable CPU turbo boost feature on AC mode.")

  (cpu-boost-on-bat?
   (maybe-boolean 'disabled)
   "Same as @code{cpu-boost-on-ac?} on BAT mode.")

  (sched-powersave-on-ac?
   (boolean #f)
   "Allow Linux kernel to minimize the number of CPU cores/hyper-threads
used under light load conditions.")

  (sched-powersave-on-bat?
   (boolean #t)
   "Same as @code{sched-powersave-on-ac?} but on BAT mode.")

  (nmi-watchdog?
   (boolean #f)
   "Enable Linux kernel NMI watchdog.")

  (phc-controls
   (maybe-string 'disabled)
   "For Linux kernels with PHC patch applied, change CPU voltages.
An example value would be @samp{\"F:V F:V F:V F:V\"}.")

  (energy-perf-policy-on-ac
   (string "performance")
   "Set CPU performance versus energy saving policy on AC.  Alternatives are
performance, normal, powersave.")

  (energy-perf-policy-on-bat
   (string "powersave")
   "Same as @code{energy-perf-policy-ac} but on BAT mode.")

  (disks-devices
   (space-separated-string-list '("sda"))
   "Hard disk devices.")

  (disk-apm-level-on-ac
   (space-separated-string-list '("254" "254"))
   "Hard disk advanced power management level.")

  (disk-apm-level-on-bat
   (space-separated-string-list '("128" "128"))
   "Same as @code{disk-apm-bat} but on BAT mode.")

  (disk-spindown-timeout-on-ac
   (maybe-space-separated-string-list 'disabled)
   "Hard disk spin down timeout.  One value has to be specified for
each declared hard disk.")

  (disk-spindown-timeout-on-bat
   (maybe-space-separated-string-list 'disabled)
   "Same as @code{disk-spindown-timeout-on-ac} but on BAT mode.")

  (disk-iosched
   (maybe-space-separated-string-list 'disabled)
   "Select IO scheduler for disk devices.  One value has to be specified
for each declared hard disk.  Example alternatives are cfq, deadline and noop.")

  (sata-linkpwr-on-ac
   (string "max_performance")
   "SATA aggressive link power management (ALPM) level.  Alternatives are
min_power, medium_power, max_performance.")

  (sata-linkpwr-on-bat
   (string "min_power")
   "Same as @code{sata-linkpwr-ac} but on BAT mode.")

  (sata-linkpwr-blacklist
   (maybe-string 'disabled)
   "Exclude specified SATA host devices for link power management.")

  (ahci-runtime-pm-on-ac?
   (maybe-on-off-boolean 'disabled)
   "Enable Runtime Power Management for AHCI controller and disks
on AC mode.")

  (ahci-runtime-pm-on-bat?
   (maybe-on-off-boolean 'disabled)
   "Same as @code{ahci-runtime-pm-on-ac} on BAT mode.")

  (ahci-runtime-pm-timeout
   (non-negative-integer 15)
   "Seconds of inactivity before disk is suspended.")

  (pcie-aspm-on-ac
   (string "performance")
   "PCI Express Active State Power Management level.  Alternatives are
default, performance, powersave.")

  (pcie-aspm-on-bat
   (string "powersave")
   "Same as @code{pcie-aspm-ac} but on BAT mode.")

  (start-charge-thresh-bat0
   (maybe-non-negative-integer 'disabled)
   "Percentage when battery 0 should begin charging.")

  (stop-charge-thresh-bat0
   (maybe-non-negative-integer 'disabled)
   "Percentage when battery 0 should stop charging.")

  (start-charge-thresh-bat1
   (maybe-non-negative-integer 'disabled)
   "Percentage when battery 1 should begin charging.")

  (stop-charge-thresh-bat1
   (maybe-non-negative-integer 'disabled)
   "Percentage when battery 1 should stop charging.")

  (radeon-power-profile-on-ac
   (string "high")
   "Radeon graphics clock speed level.  Alternatives are
low, mid, high, auto, default.")

  (radeon-power-profile-on-bat
   (string "low")
   "Same as @code{radeon-power-ac} but on BAT mode.")

  (radeon-dpm-state-on-ac
   (string "performance")
   "Radeon dynamic power management method (DPM).  Alternatives are
battery, performance.")

  (radeon-dpm-state-on-bat
   (string "battery")
   "Same as @code{radeon-dpm-state-ac} but on BAT mode.")

  (radeon-dpm-perf-level-on-ac
   (string "auto")
   "Radeon DPM performance level.  Alternatives are
auto, low, high.")

  (radeon-dpm-perf-level-on-bat
   (string "auto")
   "Same as @code{radeon-dpm-perf-ac} but on BAT mode.")

  (wifi-pwr-on-ac?
   (on-off-boolean #f)
   "Wifi power saving mode.")

  (wifi-pwr-on-bat?
   (on-off-boolean #t)
   "Same as @code{wifi-power-ac?} but on BAT mode.")

  (wol-disable?
   (y-n-boolean #t)
   "Disable wake on LAN.")

  (sound-power-save-on-ac
   (non-negative-integer 0)
   "Timeout duration in seconds before activating audio power saving
 on Intel HDA and AC97 devices.  A value of 0 disables power saving.")

  (sound-power-save-on-bat
   (non-negative-integer 1)
   "Same as @code{sound-powersave-ac} but on BAT mode.")

  (sound-power-save-controller?
   (y-n-boolean #t)
   "Disable controller in powersaving mode on Intel HDA devices.")

  (bay-poweroff-on-bat?
   (boolean #f)
   "Enable optical drive in UltraBay/MediaBay on BAT mode.
Drive can be powered on again by releasing (and reinserting) the eject lever
or by pressing the disc eject button on newer models.")

  (bay-device
   (string "sr0")
   "Name of the optical drive device to power off.")

  (runtime-pm-on-ac
   (string "on")
   "Runtime Power Management for PCI(e) bus devices.  Alternatives are
on and auto.")

  (runtime-pm-on-bat
   (string "auto")
   "Same as @code{runtime-pm-ac} but on BAT mode.")

  (runtime-pm-all?
   (boolean #t)
   "Runtime Power Management for all PCI(e) bus devices, except
blacklisted ones.")

  (runtime-pm-blacklist
   (maybe-space-separated-string-list 'disabled)
   "Exclude specified PCI(e) device addresses from Runtime Power Management.")

  (runtime-pm-driver-blacklist
   (space-separated-string-list '("radeon" "nouveau"))
   "Exclude PCI(e) devices assigned to the specified drivers from
Runtime Power Management.")

  (usb-autosuspend?
   (boolean #t)
   "Enable USB autosuspend feature.")

  (usb-blacklist
   (maybe-string 'disabled)
   "Exclude specified devices from USB autosuspend.")

  (usb-blacklist-wwan?
   (boolean #t)
   "Exclude WWAN devices from USB autosuspend.")

  (usb-whitelist
   (maybe-string 'disabled)
   "Include specified devices into USB autosuspend, even if they are
already excluded by the driver or via @code{usb-blacklist-wwan?}.")

  (usb-autosuspend-disable-on-shutdown?
   (maybe-boolean 'disabled)
   "Enable USB autosuspend before shutdown.")

  (restore-device-state-on-startup?
   (boolean #f)
   "Restore radio device state (bluetooth, wifi, wwan) from previous
shutdown on system startup."))


(define (tlp-shepherd-service config)
  (let* ((tlp-bin (file-append
                   (tlp-configuration-tlp config) "/bin/tlp"))
         (tlp-action (lambda args
                       #~(lambda _
                           (zero? (system* #$tlp-bin #$@args))))))
    (list (shepherd-service
           (documentation "Run TLP script.")
           (provision '(tlp))
           (requirement '(user-processes))
           (start (tlp-action "init" "start"))
           (stop  (tlp-action "init" "stop"))))))

(define (tlp-activation config)
  (let* ((config-str (with-output-to-string
                       (lambda ()
                         (serialize-configuration
                          config
                          tlp-configuration-fields))))
         (config-file (plain-file "tlp" config-str)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (copy-file #$config-file "/etc/tlp.conf")))))

(define tlp-service-type
  (service-type
   (name 'tlp)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        tlp-shepherd-service)
     (service-extension udev-service-type
                        (compose list tlp-configuration-tlp))
     (service-extension activation-service-type
                        tlp-activation)))
   (default-value (tlp-configuration))
   (description "Run TLP, a power management tool.")))

(define (generate-tlp-documentation)
  (generate-documentation
   `((tlp-configuration ,tlp-configuration-fields))
   'tlp-configuration))



;;;
;;; thermald
;;;
;;; This service implements cpu scaling.  Helps prevent overheating!

(define-record-type* <thermald-configuration>
  thermald-configuration make-thermald-configuration
  thermald-configuration?
  (ignore-cpuid-check? thermald-ignore-cpuid-check?    ;boolean
                       (default #f))
  (thermald            thermald-thermald               ;file-like
                       (default thermald)))

(define (thermald-shepherd-service config)
  (list
   (shepherd-service
    (provision '(thermald))
    (documentation "Run thermald cpu frequency scaling.")
    (start #~(make-forkexec-constructor
              '(#$(file-append (thermald-thermald config) "/sbin/thermald")
                "--no-daemon"
                #$@(if (thermald-ignore-cpuid-check? config)
                       '("--ignore-cpuid-check")
                       '()))))
    (stop #~(make-kill-destructor)))))

(define thermald-service-type
  (service-type
   (name 'thermald)
   (extensions (list (service-extension shepherd-root-service-type
                                        thermald-shepherd-service)))
   (default-value (thermald-configuration))
   (description "Run thermald, a CPU frequency scaling service that helps
prevent overheating.")))
