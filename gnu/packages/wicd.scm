;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Pierre-Antoine Rault <par@rigelk.eu>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages wicd)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public wicd
  (package
    (name "wicd")
    (version "1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/wicd/"
                           (version-major+minor version) "/" version
                           "/+download/wicd-" version ".tar.gz"))
       (sha256
        (base32 "0qpbwwsrqdp40mm3a8djpn2d055rxxspdhwijwsdnws700a9d637"))
       (patches (search-patches
                 "wicd-bitrate-none-fix.patch"
                 "wicd-get-selected-profile-fix.patch"
                 "wicd-urwid-1.3.patch"
                 "wicd-wpa2-ttls.patch"))))
    (build-system python-build-system)
    (native-inputs `(("gettext" ,gettext-minimal)))
    (inputs `(("dbus-glib" ,dbus-glib)
              ("python2-dbus" ,python2-dbus)
              ("python2-pygtk" ,python2-pygtk)
              ("python2-urwid" ,python2-urwid)
              ("python2-babel" ,python2-babel)
              ("wireless-tools" ,wireless-tools)
              ("wpa-supplicant" ,wpa-supplicant)
              ("net-tools" ,net-tools)
              ("isc-dhcp" ,isc-dhcp)
              ("iproute" ,iproute)
              ("hicolor-icon-theme" ,hicolor-icon-theme)))
    (arguments
     `(#:python ,python-2
       #:tests? #f                      ; test suite requires networking
       ;; wicd directly extends distutils command classes,
       ;; we can't easily make setup.py use setuptools.
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python")))
               (define (which* cmd)
                 (cond ((string=? cmd "ping")
                        "/run/setuid-programs/ping")
                       ((which cmd)
                        => identity)
                       (else
                        (format (current-error-port)
                                "WARNING: Unable to find absolute path for ~s~%"
                                cmd)
                        #f)))
               (substitute* "setup.py"
                 ;; The handling of unrecognized distros in setup.py is
                 ;; broken.  Work around the problem.
                 (("\\('init=', " all)
                  (string-append "#" all))
                 ;; Inhibit attempts to install in /var or /etc.
                 (("\\(wpath\\.(log|etc|networks|.*scripts), " all)
                  (string-append "#" all)))

               ;; Patch references to subprograms with absolute pathnames.
               (substitute* "wicd/wnettools.py"
                 (("(misc\\.Run\\(\\[?[\"'])([^\"' ]*)" all pre cmd)
                  (string-append pre (which* cmd)))
                 (("(self\\._find_program_path|misc\\.find_path)\\([\"']([^\"']*)[\"']\\)"
                   all dummy cmd)
                  (let ((pathname (which* cmd)))
                    (if pathname
                        (string-append "'" pathname "'")
                        "None")))
                 (("([\"'])(ifconfig|route|wpa_cli|wpa_supplicant|iwconfig|iwpriv|iwlist|ping)"
                   all open-quote cmd)
                  (string-append open-quote (which* cmd))))

               ;; setup.py cannot cope without LANG
               (setenv "LANG" "C")

               (let ((params
                      (list
                       (string-append "--python=" python "/bin/python")
                       "--no-install-init"
                       "--no-install-docs"
                       "--no-install-acpi"
                       "--no-install-pmutils"
                       "--no-install-kde"
                       "--no-install-gnome-shell-extensions"

                       ;; Don't pass --distro= despite setup.py's complaints.
                       ;; Guix isn't recognised, and if it ever would be we'd
                       ;; rather ask upstream to follow standards instead.

                       "--wicdgroup=netdev"
                       "--loggroup=root"
                       "--logperms=0640"

                       ;; XXX setup.py configure asks us to pass --init=,
                       ;; but if we do it says "no such option 'init'".
                       ;; (string-append "--init=" out "/etc/init.d")

                       (string-append "--initfile=" out "/etc/init.d/wicd")
                       (string-append "--lib=" out "/lib/wicd")
                       (string-append "--share=" out "/share/wicd")

                       "--etc=/etc/wicd"
                       "--scripts=/etc/wicd/scripts"
                       "--pmutils=/etc/pm-utils/sleep.d"

                       (string-append "--encryption="
                                      out "/etc/encryption/templates")
                       (string-append "--bin=" out "/bin")
                       (string-append "--sbin=" out "/sbin")
                       (string-append "--daemon=" out "/share/wicd/daemon")
                       (string-append "--backends=" out "/share/wicd/backends")
                       (string-append "--curses=" out "/share/wicd/curses")
                       (string-append "--gtk=" out "/share/wicd/gtk")
                       (string-append "--cli=" out "/share/wicd/cli")
                       (string-append "--gnome-shell-extensions="
                                      out "/share/gnome-shell-extensions")
                       (string-append "--icons=" out "/share/icons/hicolor")
                       (string-append "--pixmaps=" out "/share/pixmaps")
                       (string-append "--images=" out "/share/icons")
                       (string-append "--dbus=" out "/etc/dbus-1/system.d")
                       (string-append "--dbus-service="
                                      out "/share/dbus-1/system-services")
                       (string-append "--systemd=" out "/lib/systemd/system")
                       (string-append "--logrotate=" out "/etc/logrotate.d")
                       (string-append "--desktop=" out "/share/applications")
                       (string-append "--translations=" out "/share/locale")
                       (string-append "--autostart=" out "/etc/xdg/autostart")
                       (string-append "--docdir=" out "/share/doc/wicd")
                       (string-append "--mandir=" out "/share/man")
                       (string-append "--kdedir=" out "/share/autostart"))))
                 (format #t
                         "running ~s with command ~s and parameters ~s~%"
                         "python setup.py" "configure" params)
                 (apply invoke "python" "setup.py" "configure" params)))))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; wicd's installer tries to put dhclient.conf.template.default
               ;; in /etc/wicd/other, which is not available in the build
               ;; environment, so here we install it manually in the output
               ;; directory.
               (let ((dest-dir (string-append out "/etc/wicd"))
                     (name "dhclient.conf.template.default"))
                 (install-file (string-append "other/" name) dest-dir))

               ;; Copy index.theme from hicolor-icon-theme.  This is needed to
               ;; allow wicd-gtk to find its icons.
               (let ((hicolor (assoc-ref inputs "hicolor-icon-theme"))
                     (name "/share/icons/hicolor/index.theme"))
                 (install-file (string-append hicolor name)
                               (string-append out "/share/icons/hicolor")))
               #t))))))
    (synopsis "Network connection manager")
    (description "Wicd is a network manager that aims to simplify wired and
wireless networking.")
    (home-page "https://launchpad.net/wicd")
    (license gpl2+)))
