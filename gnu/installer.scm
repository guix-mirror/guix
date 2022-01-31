;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
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

(define-module (gnu installer)
  #:use-module (guix discovery)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix ui)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu installer utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system locale)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (installer-program))

(define module-to-import?
  ;; Return true for modules that should be imported.  For (gnu system …) and
  ;; (gnu packages …) modules, we simply add the whole 'guix' package via
  ;; 'with-extensions' (to avoid having to rebuild it all), which is why these
  ;; modules are excluded here.
  (match-lambda
    (('guix 'config) #f)
    (('gnu 'installer _ ...) #t)
    (('gnu 'build _ ...) #t)
    (('guix 'build _ ...) #t)
    (_ #f)))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define* (build-compiled-file name locale-builder)
  "Return a file-like object that evaluates the gexp LOCALE-BUILDER and store
its result in the scheme file NAME. The derivation will also build a compiled
version of this file."
  (define set-utf8-locale
    #~(begin
        (setenv "LOCPATH"
                #$(file-append glibc-utf8-locales "/lib/locale/"
                               (version-major+minor
                                (package-version glibc-utf8-locales))))
        (setlocale LC_ALL "en_US.utf8")))

  (define builder
    (with-extensions (list guile-json-3)
      (with-imported-modules `(,@(source-module-closure
                                  '((gnu installer locale))
                                  #:select? not-config?)
                               ((guix config) => ,(make-config.scm)))
        #~(begin
            (use-modules (gnu installer locale))

            ;; The locale files contain non-ASCII characters.
            #$set-utf8-locale

            (mkdir #$output)
            (let ((locale-file
                   (string-append #$output "/" #$name ".scm"))
                  (locale-compiled-file
                   (string-append #$output "/" #$name ".go")))
              (call-with-output-file locale-file
                (lambda (port)
                  (write #$locale-builder port)))
              (compile-file locale-file
                            #:output-file locale-compiled-file))))))
  (computed-file name builder))

(define apply-locale
  ;; Install the specified locale.
  (with-imported-modules (source-module-closure '((gnu services herd)))
    #~(lambda (locale)
        (false-if-exception
         (setlocale LC_ALL locale))

        ;; Restart the documentation viewer so it displays the manual in
        ;; language that corresponds to LOCALE.  Make sure that nothing is
        ;; printed on the console.
        (parameterize ((shepherd-message-port
                        (%make-void-port "w")))
          (stop-service 'term-tty2)
          (start-service 'term-tty2 (list locale))))))

(define* (compute-locale-step #:key
                              locales-name
                              iso639-languages-name
                              iso3166-territories-name)
  "Return a gexp that run the locale-page of INSTALLER, and install the
selected locale. The list of locales, languages and territories passed to
locale-page are computed in derivations named respectively LOCALES-NAME,
ISO639-LANGUAGES-NAME and ISO3166-TERRITORIES-NAME. Those lists are compiled,
so that when the installer is run, all the lengthy operations have already
been performed at build time."
  (define (compiled-file-loader file name)
    #~(load-compiled
       (string-append #$file "/" #$name ".go")))

  (let* ((supported-locales #~(supported-locales->locales
                               #+(glibc-supported-locales)))
         (iso-codes #~(string-append #$iso-codes "/share/iso-codes/json/"))
         (iso639-3 #~(string-append #$iso-codes "iso_639-3.json"))
         (iso639-5 #~(string-append #$iso-codes "iso_639-5.json"))
         (iso3166 #~(string-append #$iso-codes "iso_3166-1.json"))
         (locales-file (build-compiled-file
                        locales-name
                        #~`(quote ,#$supported-locales)))
         (iso639-file (build-compiled-file
                       iso639-languages-name
                       #~`(quote ,(iso639->iso639-languages
                                   #$supported-locales
                                   #$iso639-3 #$iso639-5))))
         (iso3166-file (build-compiled-file
                        iso3166-territories-name
                        #~`(quote ,(iso3166->iso3166-territories #$iso3166))))
         (locales-loader (compiled-file-loader locales-file
                                               locales-name))
         (iso639-loader (compiled-file-loader iso639-file
                                              iso639-languages-name))
         (iso3166-loader (compiled-file-loader iso3166-file
                                               iso3166-territories-name)))
    #~(lambda (current-installer)
        (let ((result
               ((installer-locale-page current-installer)
                #:supported-locales #$locales-loader
                #:iso639-languages #$iso639-loader
                #:iso3166-territories #$iso3166-loader)))
          (#$apply-locale result)
          result))))

(define apply-keymap
  ;; Apply the specified keymap. Use the default keyboard model.
  #~(match-lambda
      ((layout variant options)
       (kmscon-update-keymap (default-keyboard-model)
                             layout variant options))))

(define* (compute-keymap-step context)
  "Return a gexp that runs the keymap-page of INSTALLER and install the
selected keymap."
  #~(lambda (current-installer)
      (let ((result
             (call-with-values
                 (lambda ()
                   (xkb-rules->models+layouts
                    (string-append #$xkeyboard-config
                                   "/share/X11/xkb/rules/base.xml")))
               (lambda (models layouts)
                 ((installer-keymap-page current-installer)
                  layouts '#$context)))))
        (and result (#$apply-keymap result))
        result)))

(define (installer-steps)
  (let ((locale-step (compute-locale-step
                      #:locales-name "locales"
                      #:iso639-languages-name "iso639-languages"
                      #:iso3166-territories-name "iso3166-territories"))
        (timezone-data #~(string-append #$tzdata
                                        "/share/zoneinfo/zone.tab")))
    #~(lambda (current-installer)
        ((installer-parameters-menu current-installer)
         (lambda ()
           ((installer-parameters-page current-installer)
            (lambda _
              (#$(compute-keymap-step 'param)
               current-installer)))))
        (list
         ;; Ask the user to choose a locale among those supported by
         ;; the glibc.  Install the selected locale right away, so that
         ;; the user may benefit from any available translation for the
         ;; installer messages.
         (installer-step
          (id 'locale)
          (description (G_ "Locale"))
          (compute (lambda _
                     (#$locale-step current-installer)))
          (configuration-formatter locale->configuration))

         ;; Welcome the user and ask them to choose between manual
         ;; installation and graphical install.
         (installer-step
          (id 'welcome)
          (compute (lambda _
                     ((installer-welcome-page current-installer)
                      #$(local-file "installer/aux-files/logo.txt")))))

         ;; Ask the user to select a timezone under glibc format.
         (installer-step
          (id 'timezone)
          (description (G_ "Timezone"))
          (compute (lambda _
                     ((installer-timezone-page current-installer)
                      #$timezone-data)))
          (configuration-formatter posix-tz->configuration))

         ;; The installer runs in a kmscon virtual terminal where loadkeys
         ;; won't work. kmscon uses libxkbcommon as a backend for keyboard
         ;; input. It is possible to update kmscon current keymap by sending
         ;; it a keyboard model, layout, variant and options, in a somehow
         ;; similar way as what is done with setxkbmap utility.
         ;;
         ;; So ask for a keyboard model, layout and variant to update the
         ;; current kmscon keymap.  For non-Latin layouts, we add an
         ;; appropriate second layout and toggle via Alt+Shift.
         (installer-step
          (id 'keymap)
          (description (G_ "Keyboard mapping selection"))
          (compute (lambda _
                     (#$(compute-keymap-step 'default)
                      current-installer)))
          (configuration-formatter keyboard-layout->configuration))

         ;; Ask the user to input a hostname for the system.
         (installer-step
          (id 'hostname)
          (description (G_ "Hostname"))
          (compute (lambda _
                     ((installer-hostname-page current-installer))))
          (configuration-formatter hostname->configuration))

         ;; Provide an interface above connmanctl, so that the user can select
         ;; a network susceptible to acces Internet.
         (installer-step
          (id 'network)
          (description (G_ "Network selection"))
          (compute (lambda _
                     ((installer-network-page current-installer)))))

         ;; Ask whether to enable substitute server discovery.
         (installer-step
          (id 'substitutes)
          (description (G_ "Substitute server discovery"))
          (compute (lambda _
                     ((installer-substitutes-page current-installer)))))

         ;; Prompt for users (name, group and home directory).
         (installer-step
          (id 'user)
          (description (G_ "User creation"))
          (compute (lambda _
                     ((installer-user-page current-installer))))
          (configuration-formatter users->configuration))

         ;; Ask the user to choose one or many desktop environment(s).
         (installer-step
          (id 'services)
          (description (G_ "Services"))
          (compute (lambda _
                     ((installer-services-page current-installer))))
          (configuration-formatter system-services->configuration))

         ;; Run a partitioning tool allowing the user to modify
         ;; partition tables, partitions and their mount points.
         ;; Do this last so the user has something to boot if any
         ;; of the previous steps didn't go as expected.
         (installer-step
          (id 'partition)
          (description (G_ "Partitioning"))
          (compute (lambda _
                     ((installer-partition-page current-installer))))
          (configuration-formatter user-partitions->configuration))

         (installer-step
          (id 'final)
          (description (G_ "Configuration file"))
          (compute
           (lambda (result prev-steps)
             ((installer-final-page current-installer)
              result prev-steps))))))))

(define (installer-program)
  "Return a file-like object that runs the given INSTALLER."
  (define init-gettext
    ;; Initialize gettext support, so that installer messages can be
    ;; translated.
    #~(begin
        (bindtextdomain "guix" (string-append #$guix "/share/locale"))
        (textdomain "guix")
        (setlocale LC_ALL "")))

  (define set-installer-path
    ;; Add the specified binary to PATH for later use by the installer.
    #~(let* ((inputs
              '#$(list bash ;start subshells
                       connman ;call connmanctl
                       cryptsetup
                       dosfstools ;mkfs.fat
                       e2fsprogs ;mkfs.ext4
                       lvm2-static ;dmsetup
                       btrfs-progs
                       jfsutils ;jfs_mkfs
                       ntfs-3g ;mkfs.ntfs
                       xfsprogs ;mkfs.xfs
                       kbd ;chvt
                       util-linux ;mkwap
                       nano
                       shadow
                       tar ;dump
                       gzip ;dump
                       coreutils)))
        (with-output-to-port (%make-void-port "w")
          (lambda ()
            (set-path-environment-variable "PATH" '("bin" "sbin") inputs)))))

  (define steps (installer-steps))
  (define modules
    (scheme-modules*
     (string-append (current-source-directory) "/..")
     "gnu/installer"))

  (define installer-builder
    ;; Note: Include GUIX as an extension to get all the (gnu system …), (gnu
    ;; packages …), etc. modules.
    (with-extensions (list guile-gcrypt guile-newt
                           guile-parted guile-bytestructures
                           guile-json-3 guile-git guile-webutils
                           guix gnutls)
      (with-imported-modules `(,@(source-module-closure
                                  `(,@modules
                                    (gnu services herd)
                                    (guix build utils))
                                  #:select? module-to-import?)
                               ((guix config) => ,(make-config.scm)))
        #~(begin
            (use-modules (gnu installer record)
                         (gnu installer keymap)
                         (gnu installer steps)
                         (gnu installer dump)
                         (gnu installer final)
                         (gnu installer hostname)
                         (gnu installer locale)
                         (gnu installer parted)
                         (gnu installer services)
                         (gnu installer timezone)
                         (gnu installer user)
                         (gnu installer utils)
                         (gnu installer newt)
                         ((gnu installer newt keymap)
                          #:select (keyboard-layout->configuration))
                         (gnu services herd)
                         (guix i18n)
                         (guix build utils)
                         ((system repl debug)
                          #:select (terminal-width))
                         (ice-9 match)
                         (ice-9 textual-ports))

            ;; Initialize gettext support so that installers can use
            ;; (guix i18n) module.
            #$init-gettext

            ;; Add some binaries used by the installers to PATH.
            #$set-installer-path

            ;; Arrange for language and territory name translations to be
            ;; available.  We need them at run time, not just compile time,
            ;; because some territories have several corresponding languages
            ;; (e.g., "French" is always displayed as "français", but
            ;; "Belgium" could be translated to Dutch, French, or German.)
            (bindtextdomain "iso_639-3"           ;languages
                            #+(file-append iso-codes "/share/locale"))
            (bindtextdomain "iso_3166-1"          ;territories
                            #+(file-append iso-codes "/share/locale"))

            ;; Likewise for XKB keyboard layout names.
            (bindtextdomain "xkeyboard-config"
                            #+(file-append xkeyboard-config "/share/locale"))

            ;; Initialize 'terminal-width' in (system repl debug)
            ;; to a large-enough value to make backtrace more
            ;; verbose.
            (terminal-width 200)

            (define current-installer newt-installer)
            (define steps (#$steps current-installer))

            (dynamic-wind
              (installer-init current-installer)
              (lambda ()
                (parameterize
                    ((run-command-in-installer
                      (installer-run-command current-installer)))
                  (catch #t
                    (lambda ()
                      (define results
                        (run-installer-steps
                         #:rewind-strategy 'menu
                         #:menu-proc (installer-menu-page current-installer)
                         #:steps steps))

                      (match (result-step results 'final)
                        ('success
                         ;; We did it!  Let's reboot!
                         (sync)
                         (stop-service 'root))
                        (_
                         ;; The installation failed, exit so that it is
                         ;; restarted by login.
                         #f)))
                    (const #f)
                    (lambda (key . args)
                      (installer-log-line "crashing due to uncaught exception: ~s ~s"
                                          key args)
                      (define dump-dir
                        (prepare-dump key args #:result %current-result))
                      (define action
                        ((installer-exit-error current-installer)
                         (get-string-all
                          (open-input-file
                           (string-append dump-dir "/installer-backtrace")))))
                      (match action
                        ('dump
                         (let* ((dump-files
                                 ((installer-dump-page current-installer)
                                  dump-dir))
                                (dump-archive
                                 (make-dump dump-dir dump-files)))
                           ((installer-report-page current-installer)
                            dump-archive)))
                        (_ #f))
                      (exit 1)))))

              (installer-exit current-installer))))))

  (program-file
   "installer"
   #~(begin
       ;; Set the default locale to install unicode support.  For
       ;; some reason, unicode support is not correctly installed
       ;; when calling this in 'installer-builder'.
       (setenv "LANG" "en_US.UTF-8")
       (execl #$(program-file "installer-real" installer-builder
                              #:guile guile-3.0-latest)
              "installer-real"))))
