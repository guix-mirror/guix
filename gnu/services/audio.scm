;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
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

(define-module (gnu services audio)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages mpd)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (mpd-configuration
            mpd-configuration?
            mpd-service-type))

;;; Commentary:
;;;
;;; Audio related services
;;;
;;; Code:

(define-record-type* <mpd-configuration>
  mpd-configuration make-mpd-configuration
  mpd-configuration?
  (user         mpd-configuration-user
                (default "mpd"))
  (music-dir    mpd-configuration-music-dir
                (default "~/Music"))
  (playlist-dir mpd-configuration-playlist-dir
                (default "~/.mpd/playlists"))
  (db-file      mpd-configuration-db-file
                (default "~/.mpd/tag_cache"))
  (state-file   mpd-configuration-state-file
                (default "~/.mpd/state"))
  (sticker-file mpd-configuration-sticker-file
                (default "~/.mpd/sticker.sql"))
  (port         mpd-configuration-port
                (default "6600"))
  (address      mpd-configuration-address
                (default "any")))

(define (mpd-config->file config)
  (apply
   mixed-text-file "mpd.conf"
   "audio_output {\n"
   "  type \"pulse\"\n"
   "  name \"MPD\"\n"
   "}\n"
   "pid_file \"" (mpd-file-name config "pid") "\"\n"
   (map (match-lambda
          ((config-name config-val)
           (string-append config-name " \"" (config-val config) "\"\n")))
        `(("user" ,mpd-configuration-user)
          ("music_directory" ,mpd-configuration-music-dir)
          ("playlist_directory" ,mpd-configuration-playlist-dir)
          ("db_file" ,mpd-configuration-db-file)
          ("state_file" ,mpd-configuration-state-file)
          ("sticker_file" ,mpd-configuration-sticker-file)
          ("port" ,mpd-configuration-port)
          ("bind_to_address" ,mpd-configuration-address)))))

(define (mpd-file-name config file)
  "Return a path in /var/run/mpd/ that is writable
   by @code{user} from @code{config}."
  (string-append "/var/run/mpd/"
                 (mpd-configuration-user config)
                 "/" file))

(define (mpd-shepherd-service config)
  (shepherd-service
   (documentation "Run the MPD (Music Player Daemon)")
   (provision '(mpd))
   (start #~(make-forkexec-constructor
             (list #$(file-append mpd "/bin/mpd")
                   "--no-daemon"
                   #$(mpd-config->file config))
             #:pid-file #$(mpd-file-name config "pid")
             #:log-file #$(mpd-file-name config "log")))
   (stop  #~(make-kill-destructor))))

(define (mpd-service-activation config)
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw #$(mpd-configuration-user config)))

        (let ((directory #$(mpd-file-name config "")))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))))))

(define mpd-service-type
  (service-type
   (name 'mpd)
   (description "Run the Music Player Daemon (MPD).")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list mpd-shepherd-service))
          (service-extension activation-service-type
                             mpd-service-activation)))
   (default-value (mpd-configuration))))
