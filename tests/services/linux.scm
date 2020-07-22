;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (tests services linux)
  #:use-module (ice-9 match)
  #:use-module (gnu packages linux)
  #:use-module (gnu services linux)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services linux) module.

(test-begin "linux-services")


;;;
;;; Early OOM daemon.
;;;

(define earlyoom-configuration->command-line-args
  (@@ (gnu services linux) earlyoom-configuration->command-line-args))

(define %earlyoom-configuration-sample
  (earlyoom-configuration
   (minimum-available-memory 10)
   (minimum-free-swap 20)
   (prefer-regexp "icecat")
   (avoid-regexp "guix-daemon")
   (memory-report-interval 60)
   (ignore-positive-oom-score-adj? #f)
   (run-with-higher-priority? #t)
   (show-debug-messages? #f)
   (send-notification-command "python \"/some/path/notify-all-users.py\"")))

(test-equal "earlyoom-configuration->command-line-args"
  (list (file-append earlyoom "/bin/earlyoom")
        "-m" "10" "-s" "20" "--prefer" "icecat"
        "--avoid" "guix-daemon" "-r" "60" "-p"
        "-N" "python \"/some/path/notify-all-users.py\"")
  (earlyoom-configuration->command-line-args %earlyoom-configuration-sample))


;;;
;;; Zram swap device.
;;;

(define zram-device-configuration->udev-string
  (@@ (gnu services linux) zram-device-configuration->udev-string))

(define %zram-swap-device-test-1
  (zram-device-configuration
    (size "2G")
    (compression-algorithm 'zstd)
    (memory-limit "1G")
    (priority 42)))

(test-equal "zram-swap-device-test-1"
  "KERNEL==\"zram0\", ATTR{comp_algorithm}=\"zstd\" ATTR{disksize}=\"2G\" ATTR{mem_limit}=\"1G\" RUN+=\"/run/current-system/profile/sbin/mkswap /dev/zram0\" RUN+=\"/run/current-system/profile/sbin/swapon --priority 42 /dev/zram0\"\n"
  (zram-device-configuration->udev-string %zram-swap-device-test-1))

(define %zram-swap-device-test-2
  (zram-device-configuration
    (size 1048576)  ; 1M
    (compression-algorithm 'lz4)))

(test-equal "zram-swap-device-test-2"
  "KERNEL==\"zram0\", ATTR{comp_algorithm}=\"lz4\" ATTR{disksize}=\"1048576\" RUN+=\"/run/current-system/profile/sbin/mkswap /dev/zram0\" RUN+=\"/run/current-system/profile/sbin/swapon /dev/zram0\"\n"
  (zram-device-configuration->udev-string %zram-swap-device-test-2))

(define %zram-swap-device-test-3
  (zram-device-configuration
    (memory-limit (* 512 1000))))

(test-equal "zram-swap-device-test-3"
  "KERNEL==\"zram0\", ATTR{comp_algorithm}=\"lzo\" ATTR{disksize}=\"1G\" ATTR{mem_limit}=\"512000\" RUN+=\"/run/current-system/profile/sbin/mkswap /dev/zram0\" RUN+=\"/run/current-system/profile/sbin/swapon /dev/zram0\"\n"
  (zram-device-configuration->udev-string %zram-swap-device-test-3))

(test-end "linux-services")
