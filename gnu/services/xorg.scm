;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
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

(define-module (gnu services xorg)
  #:use-module (gnu artwork)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system pam)
  #:use-module (gnu services dbus)
  #:use-module ((gnu packages base) #:select (canonical-package))
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages gnustep)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (xorg-configuration-file
            %default-xorg-modules
            %default-xorg-fonts
            xorg-wrapper
            xorg-start-command
            xinitrc

            %default-slim-theme
            %default-slim-theme-name

            slim-configuration
            slim-configuration?
            slim-configuration-slim
            slim-configuration-allow-empty-passwords?
            slim-configuration-auto-login?
            slim-configuration-default-user
            slim-configuration-theme
            slim-configuration-theme-name
            slim-configuration-xauth
            slim-configuration-shepherd
            slim-configuration-auto-login-session
            slim-configuration-startx

            slim-service-type
            slim-service

            screen-locker
            screen-locker?
            screen-locker-service-type
            screen-locker-service

            gdm-configuration
            gdm-service-type
            gdm-service))

;;; Commentary:
;;;
;;; Services that relate to the X Window System.
;;;
;;; Code:

(define %default-xorg-modules
  ;; Default list of modules loaded by the server.  When multiple drivers
  ;; match, the first one in the list is loaded.
  (list xf86-video-vesa
        xf86-video-fbdev
        xf86-video-amdgpu
        xf86-video-ati
        xf86-video-cirrus
        xf86-video-intel
        xf86-video-mach64
        xf86-video-nouveau
        xf86-video-nv
        xf86-video-sis

        ;; Libinput is the new thing and is recommended over evdev/synaptics:
        ;; <http://who-t.blogspot.fr/2015/01/xf86-input-libinput-compatibility-with.html>.
        xf86-input-libinput

        xf86-input-evdev
        xf86-input-keyboard
        xf86-input-mouse
        xf86-input-synaptics))

(define %default-xorg-fonts
  ;; Default list of fonts available to the X server.
  (list (file-append font-alias "/share/fonts/X11/75dpi")
        (file-append font-alias "/share/fonts/X11/100dpi")
        (file-append font-alias "/share/fonts/X11/misc")
        (file-append font-alias "/share/fonts/X11/cyrillic")
        (file-append font-misc-misc               ;default fonts for xterm
                     "/share/fonts/X11/misc")
        (file-append font-adobe75dpi "/share/fonts/X11/75dpi")))

(define* (xorg-configuration-file #:key
                                  (modules %default-xorg-modules)
                                  (fonts %default-xorg-fonts)
                                  (drivers '()) (resolutions '())
                                  (extra-config '()))
  "Return a configuration file for the Xorg server containing search paths for
all the common drivers.

@var{modules} must be a list of @dfn{module packages} loaded by the Xorg
server---e.g., @code{xf86-video-vesa}, @code{xf86-input-keyboard}, and so on.
@var{fonts} must be a list of font directories to add to the server's
@dfn{font path}.

@var{drivers} must be either the empty list, in which case Xorg chooses a
graphics driver automatically, or a list of driver names that will be tried in
this order---e.g., @code{(\"modesetting\" \"vesa\")}.

Likewise, when @var{resolutions} is the empty list, Xorg chooses an
appropriate screen resolution; otherwise, it must be a list of
resolutions---e.g., @code{((1024 768) (640 480))}.

Last, @var{extra-config} is a list of strings or objects appended to the
configuration file.  It is used to pass extra text to be
added verbatim to the configuration file."
  (define all-modules
    ;; 'xorg-server' provides 'fbdevhw.so' etc.
    (append modules (list xorg-server)))

  (define build
    #~(begin
        (use-modules (ice-9 match)
                     (srfi srfi-1)
                     (srfi srfi-26))

        (call-with-output-file #$output
          (lambda (port)
            (define drivers
              '#$drivers)

            (define (device-section driver)
              (string-append "
Section \"Device\"
  Identifier \"device-" driver "\"
  Driver \"" driver "\"
EndSection"))

            (define (screen-section driver resolutions)
              (string-append "
Section \"Screen\"
  Identifier \"screen-" driver "\"
  Device \"device-" driver "\"
  SubSection \"Display\"
    Modes "
  (string-join (map (match-lambda
                      ((x y)
                       (string-append "\"" (number->string x)
                                      "x" (number->string y) "\"")))
                    resolutions)) "
  EndSubSection
EndSection"))

            (define (expand modules)
              ;; Append to MODULES the relevant /lib/xorg/modules
              ;; sub-directories.
              (append-map (lambda (module)
                            (filter-map (lambda (directory)
                                          (let ((full (string-append module
                                                                     directory)))
                                            (and (file-exists? full)
                                                 full)))
                                        '("/lib/xorg/modules/drivers"
                                          "/lib/xorg/modules/input"
                                          "/lib/xorg/modules/multimedia"
                                          "/lib/xorg/modules/extensions")))
                          modules))

            (display "Section \"Files\"\n" port)
            (for-each (lambda (font)
                        (format port "  FontPath \"~a\"~%" font))
                      '#$fonts)
            (for-each (lambda (module)
                        (format port
                                "  ModulePath \"~a\"~%"
                                module))
                      (append (expand '#$all-modules)

                              ;; For fbdevhw.so and so on.
                              (list #$(file-append xorg-server
                                                   "/lib/xorg/modules"))))
            (display "EndSection\n" port)
            (display "
Section \"ServerFlags\"
  Option \"AllowMouseOpenFail\" \"on\"
EndSection\n" port)

            (display (string-join (map device-section drivers) "\n")
                     port)
            (newline port)
            (display (string-join
                      (map (cut screen-section <> '#$resolutions)
                           drivers)
                      "\n")
                     port)
            (newline port)

            (for-each (lambda (config)
                        (display config port))
                      '#$extra-config)))))

  (computed-file "xserver.conf" build))


(define (xorg-configuration-directory modules)
  "Return a directory that contains the @code{.conf} files for X.org that
includes the @code{share/X11/xorg.conf.d} directories of each package listed
in @var{modules}."
  (with-imported-modules '((guix build utils))
    (computed-file "xorg.conf.d"
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define files
                         (append-map (lambda (module)
                                       (find-files (string-append
                                                    module
                                                    "/share/X11/xorg.conf.d")
                                                   "\\.conf$"))
                                     (list #$@modules)))

                       (mkdir #$output)
                       (for-each (lambda (file)
                                   (symlink file
                                            (string-append #$output "/"
                                                           (basename file))))
                                 files)
                       #t))))

(define* (xorg-wrapper #:key
                       (guile (canonical-package guile-2.0))
                       (modules %default-xorg-modules)
                       (configuration-file (xorg-configuration-file
                                            #:modules modules))
                       (xorg-server xorg-server))
  "Return a derivation that builds a @var{guile} script to start the X server
from @var{xorg-server}.  @var{configuration-file} is the server configuration
file or a derivation that builds it; when omitted, the result of
@code{xorg-configuration-file} is used.  The resulting script should be used
in place of @code{/usr/bin/X}."
  (define exp
    ;; Write a small wrapper around the X server.
    #~(begin
        (setenv "XORG_DRI_DRIVER_PATH" (string-append #$mesa "/lib/dri"))
        (setenv "XKB_BINDIR" (string-append #$xkbcomp "/bin"))

        (let ((X (string-append #$xorg-server "/bin/X")))
          (apply execl X X
                 "-xkbdir" (string-append #$xkeyboard-config "/share/X11/xkb")
                 "-config" #$configuration-file
                 "-configdir" #$(xorg-configuration-directory modules)
                 (cdr (command-line))))))

  (program-file "X-wrapper" exp))

(define* (xorg-start-command #:key
                             (guile (canonical-package guile-2.0))
                             (modules %default-xorg-modules)
                             (fonts %default-xorg-fonts)
                             (configuration-file
                              (xorg-configuration-file #:modules modules
                                                       #:fonts fonts))
                             (xorg-server xorg-server))
  "Return a @code{startx} script in which @var{modules}, a list of X module
packages, and @var{fonts}, a list of X font directories, are available.  See
@code{xorg-wrapper} for more details on the arguments.  The result should be
used in place of @code{startx}."
  (define X
    (xorg-wrapper #:guile guile
                  #:configuration-file configuration-file
                  #:modules modules
                  #:xorg-server xorg-server))
  (define exp
    ;; Write a small wrapper around the X server.
    #~(apply execl #$X #$X ;; Second #$X is for argv[0].
             "-logverbose" "-verbose" "-nolisten" "tcp" "-terminate"
             (cdr (command-line))))

  (program-file "startx" exp))

(define* (xinitrc #:key
                  (guile (canonical-package guile-2.0))
                  fallback-session)
  "Return a system-wide xinitrc script that starts the specified X session,
which should be passed to this script as the first argument.  If not, the
@var{fallback-session} will be used or, if @var{fallback-session} is false, a
desktop session from the system or user profile will be used."
  (define builder
    #~(begin
        (use-modules (ice-9 match)
                     (ice-9 regex)
                     (ice-9 ftw)
                     (ice-9 rdelim)
                     (srfi srfi-1)
                     (srfi srfi-26))

        (define (close-all-fdes)
          ;; Close all the open file descriptors except 0 to 2.
          (let loop ((fd 3))
            (when (< fd 4096)               ;FIXME: use sysconf + _SC_OPEN_MAX
              (false-if-exception (close-fdes fd))
              (loop (+ 1 fd)))))

        (define (exec-from-login-shell command . args)
          ;; Run COMMAND from a login shell so that it gets to see the same
          ;; environment variables that one gets when logging in on a tty, for
          ;; instance.
          (let* ((pw    (getpw (getuid)))
                 (shell (passwd:shell pw)))
            ;; Close any open file descriptors.  This is all the more
            ;; important that SLiM itself exec's us directly without closing
            ;; its own file descriptors!
            (close-all-fdes)

            ;; The '--login' option is supported at least by Bash and zsh.
            (execl shell shell "--login" "-c"
                   (string-join (cons command args)))))

        (define system-profile
          "/run/current-system/profile")

        (define user-profile
          (and=> (getpw (getuid))
                 (lambda (pw)
                   (string-append (passwd:dir pw) "/.guix-profile"))))

        (define (xsession-command desktop-file)
          ;; Read from DESKTOP-FILE its X session command and return it as a
          ;; list.
          (define exec-regexp
            (make-regexp "^[[:blank:]]*Exec=(.*)$"))

          (call-with-input-file desktop-file
            (lambda (port)
              (let loop ()
                (match (read-line port)
                  ((? eof-object?) #f)
                  ((= (cut regexp-exec exec-regexp <>) result)
                   (if result
                       (string-tokenize (match:substring result 1))
                       (loop))))))))

        (define (find-session profile)
          ;; Return an X session command from PROFILE or #f if none was found.
          (let ((directory (string-append profile "/share/xsessions")))
            (match (scandir directory
                            (cut string-suffix? ".desktop" <>))
              ((or () #f)
               #f)
              ((sessions ...)
               (any xsession-command
                    (map (cut string-append directory "/" <>)
                         sessions))))))

        (let* ((home          (getenv "HOME"))
               (xsession-file (string-append home "/.xsession"))
               (session       (match (command-line)
                                ((_)
                                 #$(if fallback-session
                                       #~(list #$fallback-session)
                                       #f))
                                ((_ x ..1)
                                 x))))
          (if (file-exists? xsession-file)
              ;; Run ~/.xsession when it exists.
              (apply exec-from-login-shell xsession-file
                     (or session '()))
              ;; Otherwise, start the specified session or a fallback.
              (apply exec-from-login-shell
                     (or session
                         (find-session user-profile)
                         (find-session system-profile)))))))

  (program-file "xinitrc" builder))


;;;
;;; SLiM log-in manager.
;;;

(define %default-slim-theme
  ;; Theme based on work by Felipe López.
  (file-append %artwork-repository "/slim"))

(define %default-slim-theme-name
  ;; This must be the name of the sub-directory in %DEFAULT-SLIM-THEME that
  ;; contains the actual theme files.
  "0.x")

(define-record-type* <slim-configuration>
  slim-configuration make-slim-configuration
  slim-configuration?
  (slim slim-configuration-slim
        (default slim))
  (allow-empty-passwords? slim-configuration-allow-empty-passwords?
                          (default #t))
  (auto-login? slim-configuration-auto-login?
               (default #f))
  (default-user slim-configuration-default-user
                (default ""))
  (theme slim-configuration-theme
         (default %default-slim-theme))
  (theme-name slim-configuration-theme-name
              (default %default-slim-theme-name))
  (xauth slim-configuration-xauth
         (default xauth))
  (shepherd slim-configuration-shepherd
            (default shepherd))
  (auto-login-session slim-configuration-auto-login-session
                      (default #f))
  (startx slim-configuration-startx
          (default (xorg-start-command)))
  (sessreg slim-configuration-sessreg
           (default sessreg)))

(define (slim-pam-service config)
  "Return a PAM service for @command{slim}."
  (list (unix-pam-service
         "slim"
         #:allow-empty-passwords?
         (slim-configuration-allow-empty-passwords? config))))

(define (slim-shepherd-service config)
  (define slim.cfg
    (let ((xinitrc (xinitrc #:fallback-session
                            (slim-configuration-auto-login-session config)))
          (slim    (slim-configuration-slim config))
          (xauth   (slim-configuration-xauth config))
          (startx  (slim-configuration-startx config))
          (shepherd   (slim-configuration-shepherd config))
          (theme-name (slim-configuration-theme-name config))
          (sessreg (slim-configuration-sessreg config)))
      (mixed-text-file "slim.cfg"  "
default_path /run/current-system/profile/bin
default_xserver " startx "
xserver_arguments :0 vt7
xauth_path " xauth "/bin/xauth
authfile /var/run/slim.auth

# The login command.  '%session' is replaced by the chosen session name, one
# of the names specified in the 'sessions' setting: 'wmaker', 'xfce', etc.
login_cmd  exec " xinitrc " %session
sessiondir /run/current-system/profile/share/xsessions
session_msg session (F1 to change):
sessionstart_cmd " sessreg "/bin/sessreg -a -l $DISPLAY %user
sessionstop_cmd " sessreg "/bin/sessreg -d -l $DISPLAY %user

halt_cmd " shepherd "/sbin/halt
reboot_cmd " shepherd "/sbin/reboot\n"
(if (slim-configuration-auto-login? config)
    (string-append "auto_login yes\ndefault_user "
                   (slim-configuration-default-user config) "\n")
    "")
(if theme-name
    (string-append "current_theme " theme-name "\n")
    ""))))

  (define theme
    (slim-configuration-theme config))

  (list (shepherd-service
         (documentation "Xorg display server")
         (provision '(xorg-server))
         (requirement '(user-processes host-name udev))
         (start
          #~(lambda ()
              ;; A stale lock file can prevent SLiM from starting, so remove it to
              ;; be on the safe side.
              (false-if-exception (delete-file "/var/run/slim.lock"))

              (fork+exec-command
               (list (string-append #$slim "/bin/slim") "-nodaemon")
               #:environment-variables
               (list (string-append "SLIM_CFGFILE=" #$slim.cfg)
                     #$@(if theme
                            (list #~(string-append "SLIM_THEMESDIR=" #$theme))
                            #~())))))
         (stop #~(make-kill-destructor))
         (respawn? #t))))

(define slim-service-type
  (service-type (name 'slim)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          slim-shepherd-service)
                       (service-extension pam-root-service-type
                                          slim-pam-service)

                       ;; Unconditionally add xterm to the system profile, to
                       ;; avoid bad surprises.
                       (service-extension profile-service-type
                                          (const (list xterm)))))
                (default-value (slim-configuration))))

(define-deprecated (slim-service #:key (slim slim)
                                 (allow-empty-passwords? #t) auto-login?
                                 (default-user "")
                                 (theme %default-slim-theme)
                                 (theme-name %default-slim-theme-name)
                                 (xauth xauth) (shepherd shepherd)
                                 (auto-login-session #f)
                                 (startx (xorg-start-command)))
  slim-service-type
  "Return a service that spawns the SLiM graphical login manager, which in
turn starts the X display server with @var{startx}, a command as returned by
@code{xorg-start-command}.

@cindex X session

SLiM automatically looks for session types described by the @file{.desktop}
files in @file{/run/current-system/profile/share/xsessions} and allows users
to choose a session from the log-in screen using @kbd{F1}.  Packages such as
@var{xfce}, @var{sawfish}, and @var{ratpoison} provide @file{.desktop} files;
adding them to the system-wide set of packages automatically makes them
available at the log-in screen.

In addition, @file{~/.xsession} files are honored.  When available,
@file{~/.xsession} must be an executable that starts a window manager
and/or other X clients.

When @var{allow-empty-passwords?} is true, allow logins with an empty
password.  When @var{auto-login?} is true, log in automatically as
@var{default-user} with @var{auto-login-session}.

If @var{theme} is @code{#f}, the use the default log-in theme; otherwise
@var{theme} must be a gexp denoting the name of a directory containing the
theme to use.  In that case, @var{theme-name} specifies the name of the
theme."
  (service slim-service-type
           (slim-configuration
            (slim slim)
            (allow-empty-passwords? allow-empty-passwords?)
            (auto-login? auto-login?) (default-user default-user)
            (theme theme) (theme-name theme-name)
            (xauth xauth) (shepherd shepherd)
            (auto-login-session auto-login-session)
            (startx startx))))


;;;
;;; Screen lockers & co.
;;;

(define-record-type <screen-locker>
  (screen-locker name program empty?)
  screen-locker?
  (name    screen-locker-name)                     ;string
  (program screen-locker-program)                  ;gexp
  (empty?  screen-locker-allows-empty-passwords?)) ;Boolean

(define screen-locker-pam-services
  (match-lambda
    (($ <screen-locker> name _ empty?)
     (list (unix-pam-service name
                             #:allow-empty-passwords? empty?)))))

(define screen-locker-setuid-programs
  (compose list screen-locker-program))

(define screen-locker-service-type
  (service-type (name 'screen-locker)
                (extensions
                 (list (service-extension pam-root-service-type
                                          screen-locker-pam-services)
                       (service-extension setuid-program-service-type
                                          screen-locker-setuid-programs)))))

(define* (screen-locker-service package
                                #:optional
                                (program (package-name package))
                                #:key allow-empty-passwords?)
  "Add @var{package}, a package for a screen locker or screen saver whose
command is @var{program}, to the set of setuid programs and add a PAM entry
for it.  For example:

@lisp
 (screen-locker-service xlockmore \"xlock\")
@end lisp

makes the good ol' XlockMore usable."
  (service screen-locker-service-type
           (screen-locker program
                          (file-append package "/bin/" program)
                          allow-empty-passwords?)))

(define %gdm-accounts
  (list (user-group (name "gdm") (system? #t))
        (user-account
         (name "gdm")
         (group "gdm")
         (system? #t)
         (comment "GNOME Display Manager user")
         (home-directory "/var/lib/gdm")
         (shell (file-append shadow "/sbin/nologin")))))

(define-record-type* <gdm-configuration>
  gdm-configuration make-gdm-configuration
  gdm-configuration?
  (gdm gdm-configuration-gdm (default gdm))
  (allow-empty-passwords? gdm-configuration-allow-empty-passwords? (default #t))
  (auto-login? gdm-configuration-auto-login? (default #f))
  (default-user gdm-configuration-default-user (default #f))
  (x-server gdm-configuration-x-server
            (default (xorg-wrapper))))

(define (gdm-configuration-file config)
  (mixed-text-file "gdm-custom.conf"
                   "[daemon]\n"
                   "#User=gdm\n"
                   "#Group=gdm\n"
                   (if (gdm-configuration-auto-login? config)
                       (string-append
                        "AutomaticLoginEnable=true\n"
                        "AutomaticLogin="
                        (or (gdm-configuration-default-user config)
                            (error "missing default user for auto-login"))
                        "\n")
                       (string-append
                        "AutomaticLoginEnable=false\n"
                        "#AutomaticLogin=\n"))
                   "#TimedLoginEnable=false\n"
                   "#TimedLogin=\n"
                   "#TimedLoginDelay=0\n"
                   "#InitialSetupEnable=true\n"
                   ;; Enable me once X is working.
                   "WaylandEnable=false\n"
                   "\n"
                   "[debug]\n"
                   "#Enable=true\n"
                   "\n"
                   "[security]\n"
                   "#DisallowTCP=true\n"
                   "#AllowRemoteAutoLogin=false\n"))

(define (gdm-pam-service config)
  "Return a PAM service for @command{gdm}."
  (list
   (pam-service
    (inherit (unix-pam-service "gdm-autologin"))
    (auth (list (pam-entry
                 (control "[success=ok default=1]")
                 (module (file-append (gdm-configuration-gdm config)
                                      "/lib/security/pam_gdm.so")))
                (pam-entry
                 (control "sufficient")
                 (module "pam_permit.so")))))
   (pam-service
    (inherit (unix-pam-service "gdm-launch-environment"))
    (auth (list (pam-entry
                 (control "required")
                 (module "pam_permit.so")))))
   (unix-pam-service "gdm-password"
                     #:allow-empty-passwords?
                     (gdm-configuration-allow-empty-passwords? config))))

(define (gdm-shepherd-service config)
  (list (shepherd-service
         (documentation "Xorg display server (GDM)")
         (provision '(xorg-server))
         (requirement '(dbus-system user-processes host-name udev))
         (start #~(lambda ()
                    (fork+exec-command
                     (list #$(file-append (gdm-configuration-gdm config)
                                          "/bin/gdm"))
                     #:environment-variables
                     (list (string-append
                            "GDM_CUSTOM_CONF="
                            #$(gdm-configuration-file config))
                           (string-append
                            "GDM_X_SERVER="
                            #$(gdm-configuration-x-server config))
                           ;; XXX: GDM requires access to a handful of
                           ;; programs and components from Gnome (gnome-shell,
                           ;; dbus, and gnome-session among others). The
                           ;; following variables only work provided Gnome is
                           ;; installed.
                           "XDG_DATA_DIRS=/run/current-system/profile/share"
                           "PATH=/run/current-system/profile/bin"))))
         (stop #~(make-kill-destructor))
         (respawn? #t))))

(define gdm-service-type
  (service-type (name 'gdm)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          gdm-shepherd-service)
                       (service-extension account-service-type
                                          (const %gdm-accounts))
                       (service-extension pam-root-service-type
                                          gdm-pam-service)
                       (service-extension dbus-root-service-type
                                          (compose list
                                                   gdm-configuration-gdm))))
                (default-value (gdm-configuration))
                (description
                 "Run the GNOME Desktop Manager (GDM), a program that allows
you to log in in a graphical session, whether or not you use GNOME.")))

;; This service isn't working yet; it gets as far as starting to run the
;; greeter from gnome-shell but doesn't get any further.  It is here because
;; it doesn't hurt anyone and perhaps it inspires someone to fix it :)
(define-deprecated (gdm-service #:key (gdm gdm)
                                (allow-empty-passwords? #t)
                                (x-server (xorg-wrapper)))
  gdm-service-type
  "Return a service that spawns the GDM graphical login manager, which in turn
starts the X display server with @var{X}, a command as returned by
@code{xorg-wrapper}.

@cindex X session

GDM automatically looks for session types described by the @file{.desktop}
files in @file{/run/current-system/profile/share/xsessions} and allows users
to choose a session from the log-in screen using @kbd{F1}.  Packages such as
@var{xfce}, @var{sawfish}, and @var{ratpoison} provide @file{.desktop} files;
adding them to the system-wide set of packages automatically makes them
available at the log-in screen.

In addition, @file{~/.xsession} files are honored.  When available,
@file{~/.xsession} must be an executable that starts a window manager
and/or other X clients.

When @var{allow-empty-passwords?} is true, allow logins with an empty
password."
  (service gdm-service-type
           (gdm-configuration
            (gdm gdm)
            (allow-empty-passwords? allow-empty-passwords?)
            (x-server x-server))))

;;; xorg.scm ends here
