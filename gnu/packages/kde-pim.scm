;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages kde-pim)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-internet)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages search)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml))

(define-public akonadi
  (package
    (name "akonadi")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-" version ".tar.xz"))
       (sha256
        (base32 "0kkn7lh3akkk9cdi8qdk9kqzs1cgv916mkl440x4ykqd1v8brzqb"))
       (patches (search-patches
                 "akonadi-paths.patch"
                 "akonadi-timestamps.patch"
                 "akonadi-not-relocatable.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("boost" ,boost)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ;; Do NOT add mysql or postgresql to the inputs. Otherwise the binaries
       ;; and wrapped files will refer to them, even if the user choices none
       ;; of these.  Executables are searched on $PATH then.
       ("qtbase" ,qtbase)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:tests? #f ;; TODO 135/167 tests fail
       #:configure-flags '("-DDATABASE_BACKEND=SQLITE") ; lightweight
       #:modules ((ice-9 textual-ports)
                  ,@%qt-build-system-modules)
       #:phases
       (modify-phases (@ (guix build qt-build-system) %standard-phases)
         (add-before 'configure 'add-definitions
           (lambda _
             (let ((out (assoc-ref %outputs "out"))
                   (mysql (assoc-ref %build-inputs "mysql"))
                   (pgsql (assoc-ref %build-inputs "postgresql")))
               (with-output-to-file "CMakeLists.txt.new"
                 (lambda _
                   (display
                    (string-append
                     "add_compile_definitions(\n"
                     "NIX_OUT=\"" out "\"\n"
                     ;; pin binaries for mysql backend
                     ")\n\n"))
                   (display
                    (call-with-input-file "CMakeLists.txt"
                      get-string-all))))
               (rename-file "CMakeLists.txt.new" "CMakeLists.txt"))
             #t)))))
    (home-page "https://kontact.kde.org/components/akonadi.html")
    (synopsis "Extensible cross-desktop storage service for PIM")
    (description "Akonadi is an extensible cross-desktop Personal Information
Management (PIM) storage service.  It provides a common framework for
applications to store and access mail, calendars, addressbooks, and other PIM
data.

This package contains the Akonadi PIM storage server and associated
programs.")
    (license license:fdl1.2+)))

(define-public akonadi-calendar
  (package
    (name "akonadi-calendar")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-calendar-" version ".tar.xz"))
       (sha256
        (base32 "1mq76qyd3jcngb2yfanpn7qvklzllza399fxwii0mqppp1vmnb2b"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcodecs" ,kcodecs)
       ("kcontacts" ,kcontacts)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmailtransport" ,kmailtransport)
       ("kmime" ,kmime)
       ("kpimtextedit" ,kpimtextedit)
       ("ksmtp" ,ksmtp)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f))  ;; TODO: 1/1 test fails
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Library providing calendar helpers for Akonadi items")
    (description "This library manages calendar specific actions for
collection and item views.")
    (license license:lgpl2.0+)))

(define-public akonadi-contacts
  (package
    (name "akonadi-contacts")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-contacts-" version ".tar.xz"))
       (sha256
        (base32 "0igggarnl99s5pl73dgrpha4lf7vnr000iy69vcwmqs5lxb7cyli"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("boost" ,boost)
       ("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kmime" ,kmime)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("prison" ,prison)
       ("kio" ,kio)
       ("qtbase" ,qtbase)
       ("solid" ,solid)
       ("sonnet" ,sonnet)))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi contacts access library")
    (description "Akonadi Contacts is a library that effectively bridges the
type-agnostic API of the Akonadi client libraries and the domain-specific
KContacts library.  It provides jobs, models and other helpers to make working
with contacts and addressbooks through Akonadi easier.

The library provides a complex dialog for editing contacts and several models
to list and filter contacts.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-mime
  (package
    (name "akonadi-mime")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-mime-" version ".tar.xz"))
       (sha256
        (base32 "1wd776ia3z22a79biq04y4m83n8xpvfmyg8bcsslr7lmc3avdg8w"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("libxslt" ,libxslt) ;; xslt for generating interface descriptions
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("akonadi" ,akonadi)
       ("boost" ,boost)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-desktop-file-early
           (lambda _
             (let ((plugins-dir "/tmp/.local/share/akonadi/plugins/serializer"))
               (mkdir-p plugins-dir)
               (copy-file "serializers/akonadi_serializer_mail.desktop"
                          (string-append plugins-dir "/akonadi_serializer_mail.desktop")))
             #t))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (synopsis "Akonadi MIME handling library")
    (description "Akonadi Mime is a library that effectively bridges the
type-agnostic API of the Akonadi client libraries and the domain-specific
KMime library.  It provides jobs, models and other helpers to make working
with emails through Akonadi easier.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-notes
  (package
    (name "akonadi-notes")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-notes-" version ".tar.xz"))
       (sha256
        (base32 "04y293kjrmjjcbb7fkjl7hl4vrks4cjjxnvc6ibzyv81rn6cdhh2"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("kcodecs" ,kcodecs)
       ("ki18n" ,ki18n)
       ("kmime" ,kmime)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi notes access library")
    (description "Akonadi Notes is a library that effectively bridges the
type-agnostic API of the Akonadi client libraries and the domain-specific
KMime library.  It provides a helper class for note attachments and for
wrapping notes into KMime::Message objects.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public akonadi-search
  (package
    (name "akonadi-search")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/akonadi-search-" version ".tar.xz"))
       (sha256
        (base32 "1h5p44y244gzf7ndzw7afrvq9c76ybp8ddvg82p3lzjh02rrvd50"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)

       ;; For tests.
       ("dbus" ,dbus)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcmutils" ,kcmutils)
       ("kcontacts" ,kcontacts)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("krunner" ,krunner)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("xapian" ,xapian)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-test
                    (lambda _
                      ;; FIXME: This test fails because it fails to establish
                      ;; a socket connection, seemingly due to failure during
                      ;; DBus communication.  See also 'korganizer'.
                      (substitute* "agent/autotests/CMakeLists.txt"
                        ((".*schedulertest\\.cpp.*")
                         ""))
                      #t))
                  (replace 'check
                    (lambda _
                      (invoke "dbus-launch" "ctest"))))))
    (home-page "https://api.kde.org/kdepim/akonadi/html/index.html")
    (synopsis "Akonadi search library")
    (description "This package provides a library used to search in the
Akonadi PIM data server.  It uses Xapian for indexing and querying.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kincidenceeditor
  (package
    (name "kincidenceeditor")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/incidenceeditor-" version ".tar.xz"))
       (sha256
        (base32 "1xpp5lw60mvpjsjsxmicfa5y2d68wnb9vm4yb1krwkihm852ziny"))))
    (properties `((upstream-name . "incidenceeditor")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-calendar" ,akonadi-calendar)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalendarsupport" ,kcalendarsupport)
       ("kcalutils" ,kcalutils)
       ("kcodecs" ,kcodecs)
       ("kcontacts" ,kcontacts)
       ("kdbusaddons" ,kdbusaddons)
       ("kdepim-apps-libs" ,kdepim-apps-libs)
       ("kdiagram" ,kdiagram)
       ("keventviews" ,keventviews)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kldap" ,kldap)
       ("kmailtransport" ,kmailtransport)
       ("kmime" ,kmime)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallat" ,kwallet)
       ("libkdepim" ,libkdepim)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/incidenceeditor")
    (synopsis "KDE PIM library for editing incidences")
    (description "This library provides an incidence editor for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kaddressbook
  (package
    (name "kaddressbook")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kaddressbook-" version ".tar.xz"))
       (sha256
        (base32 "1vpdhdj87ai2sxjn2jk3mh6bzfr1n3yzydnkgv7nc8v1m2fdawap"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-search" ,akonadi-search)
       ("boost" ,boost)
       ("gpgme" ,gpgme)
       ("grantlee" ,grantlee)
       ("grantleetheme" ,grantleetheme)
       ("kcalendarcore" ,kcalendarcore)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kcontacts" ,kcontacts)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdepim-apps-libs" ,kdepim-apps-libs)
       ("kdoctools" ,kdoctools)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("kontactinterface" ,kontactinterface)
       ("kparts" ,kparts)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("libkleo" ,libkleo)
       ("oxygen-icons" ,oxygen-icons) ; default icon set, required for tests
       ("prison" ,prison)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)))
    (home-page "https://kontact.kde.org/components/kaddressbook.html")
    (synopsis "Address Book application to manage your contacts")
    (description "KAddressBook stores all the personal details of your family,
friends and other contacts.  It supports large variety of services, including
NextCloud, Kolab, Google Contacts, Microsoft Exchange (EWS) or any standard
CalDAV server.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kalarmcal
  (package
    (name "kalarmcal")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kalarmcal-" version ".tar.xz"))
       (sha256
        (base32 "0g0bm4zzzcpl2pqqf609349zagwrgj6a4ibxpgg4zf21aacdq8bi"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kholidays" ,kholidays)
       ("ki18n" ,ki18n)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f)) ;; TODO: TZ setup
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library for handling kalarm calendar data")
    (description "This library provides access to and handling of kalarm
calendar data.")
    (license  license:lgpl2.0+)))

(define-public kblog
  (package
    (name "kblog")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kblog-" version ".tar.xz"))
       (sha256
        (base32 "0pi3axs58wsz5vq6vyisz73s24q739zplwrblyvkcm16nll4mvhk"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcalendarcore" ,kcalendarcore)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kxmlrpcclient" ,kxmlrpcclient)
       ("qtbase" ,qtbase)
       ("syndication" ,syndication)))
    ;; Note: Some tests take up to 90 sec.
    (home-page "https://invent.kde.org/pim/kblog")
    (synopsis "Client-side support library for web application remote blogging
APIs")
    (description "KBlog is a library for calling functions on Blogger 1.0,
MetaWeblog, MovableType and GData compatible blogs.  It calls the APIs using
KXmlRpcClient and Syndication.  It supports asynchronous sending and fetching
of posts and, if supported on the server, multimedia files.  Almost every
modern blogging web application that provides an XML data interface supports
one of the APIs mentioned above.")
    (license license:lgpl2.0+)))

(define-public kcalendarsupport
  (package
    (name "kcalendarsupport")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/calendarsupport-" version ".tar.xz"))
       (sha256
        (base32 "1yv3hs7qw481cxw4kzbx5l8vv18bgzm1b0vj3zrlqqxwl5ac6xvy"))))
    (properties `((upstream-name . "calendarsupport")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-calendar" ,akonadi-calendar)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcompletion" ,kcompletion)
       ("kdbusaddons" ,kdbusaddons)
       ("kdepim-apps-libs" ,kdepim-apps-libs)
       ("kguiaddons" ,kguiaddons)
       ("kholidays" ,kholidays)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/stable/calendarsupport/")
    (synopsis "Calendar Support library for KDE PIM")
    (description "The Calendar Support library provides helper utilities for
calendaring applications.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kcalutils
  (package
    (name "kcalutils")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kcalutils-" version ".tar.xz"))
       (sha256
        (base32 "0v268w8vhgqxq1nwv9b9cy4h7zqgjrv19r44g3zc9w5j76ivix86"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("libxml2" ,libxml2))) ;; xmllint required for tests
    (inputs
     `(("grantlee" ,grantlee)
       ("kcalendarcore" ,kcalendarcore)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("oxygen-icons" ,oxygen-icons) ; default icon set, required for tests
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f)) ;; TODO: seem to pull in some wrong theme
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library with utility functions for the handling of calendar
data")
    (description "This library provides a utility and user interface
functions for accessing calendar data using the kcalcore API.")
    (license  license:lgpl2.0+)))

(define-public kdav
  (package
    (name "kdav")
    (version "20.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdav-" version ".tar.xz"))
       (sha256
        (base32 "0445gl4xm0h39igkxgb6vmq5iaa04wkgrgbs7nfd0zwngk8xaidn"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("qtbase" ,qtbase)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://invent.kde.org/frameworks/kdav")
    (synopsis "DAV protocol implementation with KJobs")
    (description "This is a DAV protocol implementation with KJobs.  Calendars
and todos are supported, using either GroupDAV or CalDAV, and contacts are
supported using GroupDAV or CardDAV.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kdepim-apps-libs
  (package
    (name "kdepim-apps-libs")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdepim-apps-libs-" version ".tar.xz"))
       (sha256
        (base32 "0m9qrfjs97anh9h6ibggx23ddlm1zkxjap2iyf3gf672ip01fvci"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("boost" ,boost)
       ("gpgme" ,gpgme)
       ("grantlee" ,grantlee)
       ("grantleetheme" ,grantleetheme)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("kpimcommon" ,kpimcommon)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("libkleo" ,libkleo)
       ("prison" ,prison)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/kdepim-apps-libs")
    (synopsis "KDE PIM mail related libraries and data files")
    (description "This package provides mail related libraries and data files
for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kdepim-runtime
  (package
    (name "kdepim-runtime")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kdepim-runtime-" version ".tar.xz"))
       (sha256
        (base32 "1in4x4wvgclkni72cfkw9jx35d0qd0jmfwybm3ksx5qx5sbki9gg"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("kdoctools" ,kdoctools)
       ("libxslt" ,libxslt)
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-calendar" ,akonadi-calendar)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-notes" ,akonadi-notes)
       ("boost" ,boost)
       ("cyrus-sasl" ,cyrus-sasl)
       ("kalarmcal" ,kalarmcal)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kdav" ,kdav)
       ("kholidays" ,kholidays)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmailtransport" ,kmailtransport)
       ("kmbox" ,kmbox)
       ("kmime" ,kmime)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("kross" ,kross)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwindowsystem" ,kwindowsystem)
       ("libkgapi" ,libkgapi)
       ;; TODO: libkolab
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtnetworkauth" ,qtnetworkauth)
       ("qtspeech" ,qtspeech)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (arguments
      ;; TODO: 5/45 tests fail for quite different reasons, even with
      ;; "offscreen" and dbus
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'set-paths 'extend-CPLUS_INCLUDE_PATH
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; FIXME: <Akonadi/KMime/SpecialMailCollections> is not
                      ;; found during one of the compilation steps without
                      ;; this hack.
                      (setenv "CPLUS_INCLUDE_PATH"
                              (string-append (assoc-ref inputs "akonadi-mime")
                                             "/include/KF5:"
                                             (or (getenv "CPLUS_INCLUDE_PATH") "")))
                      #t)))))
    (home-page "https://invent.kde.org/pim/kdepim-runtime")
    (synopsis "Runtime components for Akonadi KDE")
    (description "This package contains Akonadi agents written using KDE
Development Platform libraries.  Any package that uses Akonadi should probably
pull this in as a dependency.  The kres-bridges is also parts of this
package.")
    (license ;; Files vary a lot regarding the license. GPL2+ and LGPL2.1+
     ;; have been used in those I checked. But the archive also includes
     ;; license texts for GPL3 and AGPL3.
     (list license:gpl2+ license:lgpl2.0+))))

(define-public keventviews
  (package
    (name "keventviews")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/eventviews-" version ".tar.xz"))
       (sha256
        (base32 "0si9p95rgp7mgkzhzwyy10zrwzy1kipbhm1y96yjlc9rxi3jrc73"))))
    (properties `((upstream-name . "eventviews")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-calendar" ,akonadi-calendar)
       ("akonadi-contacts" ,akonadi-contacts)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalendarsupport" ,kcalendarsupport)
       ("kcalutils" ,kcalutils)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kdbusaddons" ,kdbusaddons)
       ("kdiagram" ,kdiagram)
       ("kguiaddons" ,kguiaddons)
       ("kholidays" ,kholidays)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("kpimtextedit" ,kpimtextedit)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/eventviews")
    (synopsis "KDE PIM library for creating events")
    (description "This library provides an event creator for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kgpg
  (package
    (name "kgpg")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kgpg-" version ".tar.xz"))
       (sha256
        (base32 "03d3gsbara7ga2cyrhafkw11qq9cj804h9vpvxl4wd2a9c90snkh"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("gnupg" ,gnupg)  ;; TODO: Remove after gpgme uses fixed path
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("boost" ,boost)
       ("gpgme" ,gpgme)
       ("karchive" ,karchive)
       ("kcodecs" ,kcodecs)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifications" ,knotifications)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/utilities/org.kde.kgpg")
    (synopsis "Graphical front end for GNU Privacy Guard")
    (description "Kgpg manages cryptographic keys for the GNU Privacy Guard,
and can encrypt, decrypt, sign, and verify files.  It features a simple editor
for applying cryptography to short pieces of text, and can also quickly apply
cryptography to the contents of the clipboard.")
    (license license:gpl2+)))

(define-public kidentitymanagement
  (package
    (name "kidentitymanagement")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kidentitymanagement-" version ".tar.xz"))
       (sha256
        (base32 "0flp9p9hlr1zfgvsy5i1nq55p7bvnhqxkxbif1lyw0cq6iblxhgr"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kemoticons" ,kemoticons)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" "/tmp/dummy-home") ;; FIXME: what is this?
             #t)))))
    (home-page "https://kontact.kde.org/")
    (synopsis "Library for shared identities between mail applications")
    (description "This library provides an API for managing user identities.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kimap
  (package
    (name "kimap")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kimap-" version ".tar.xz"))
       (sha256
        (base32 "1x22wfzqp92mn1fy2xl89k9yjfk2vgcva0fd30i9rrqj4aw2rsma"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kmime" ,kmime)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library for handling IMAP")
    (description "This library provides a job-based API for interacting with
an IMAP4rev1 server.  It manages connections, encryption and parameter quoting
and encoding, but otherwise provides quite a low-level interface to the
protocol.  This library does not implement an IMAP client; it merely makes it
easier to do so.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kldap
  (package
    (name "kldap")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kldap-" version ".tar.xz"))
       (sha256
        (base32 "0whlp586ycsx0qf0nr81avwscpq62w5js46z7vayy0dxkhrhfayr"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)))
    (propagated-inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("openldap" ,openldap)))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library for accessing LDAP")
    (description " This is a library for accessing LDAP with a convenient Qt
style C++ API.  LDAP (Lightweight Directory Access Protocol) is an application
protocol for querying and modifying directory services running over TCP/IP. ")
    (license license:lgpl2.0+)))

(define-public kleopatra
  (package
    (name "kleopatra")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kleopatra-" version ".tar.xz"))
       (sha256
        (base32 "1m50nzb2m27fkb8z3k34cv4zi2akr0fx8zn7lk5swhg49sgrip6n"))))
    (build-system qt-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("gnupg" ,gnupg)  ;; TODO: Remove after gpgme uses fixed path
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("boost" ,boost)
       ("gpgme" ,gpgme)
       ("kcmutils" ,kcmutils)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("knotifications" ,knotifications)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libassuan" ,libassuan)
       ("libkleo" ,libkleo)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "dbus-launch" "ctest" ".")
             #t)))))
    (home-page "https://kde.org/applications/utilities/org.kde.kleopatra")
    (synopsis "Certificate Manager and Unified Crypto GUI")
    (description "Kleopatra is a certificate manager and a universal crypto
GUI.  It supports managing X.509 and OpenPGP certificates in the GpgSM keybox
and retrieving certificates from LDAP servers.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kmail
  (package
    (name "kmail")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmail-" version ".tar.xz"))
       (sha256
        (base32 "06qfxzi5pasm6p5ck44sjca96dz8xzd1nndq5lqcyvcxmmnvvz3p"))
       (patches (search-patches "kmail-Fix-missing-link-libraries.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-search" ,akonadi-search)
       ("boost" ,boost)
       ("gpgme" ,gpgme)
       ("kbookmarks" ,kbookmarks)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcmutils" ,kcmutils)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdepim-apps-libs" ,kdepim-apps-libs)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kldap" ,kldap)
       ("kmailcommon" ,kmailcommon)
       ("kmailtransport" ,kmailtransport)
       ("kmessagelib" ,kmessagelib)
       ("kmime" ,kmime)
       ("kmime" ,kmime)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kontactinterface" ,kontactinterface)
       ("kparts" ,kparts)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("kservice" ,kservice)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktextwidgets" ,ktextwidgets)
       ("ktnef" ,ktnef)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libgravatar" ,libgravatar)
       ("libkdepim" ,libkdepim)
       ("libkleo" ,libkleo)
       ("libksieve" ,libksieve)
       ("oxygen-icons" ,oxygen-icons) ; default icon set, required for tests
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "dbus-launch" "ctest" ".")
             #t)))))
    (home-page "https://kontact.kde.org/components/kmail.html")
    (synopsis "Full featured graphical email client")
    (description "KMail supports multiple accounts, mail filtering and email
encryption.  The program let you configure your workflow and it has good
integration into KDE (Plasma Desktop) but is also useable with other Desktop
Environments.

KMail is the email component of Kontact, the integrated personal information
manager from KDE.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmailcommon
  (package
    (name "kmailcommon")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mailcommon-" version ".tar.xz"))
       (sha256
        (base32 "0q1k57zx1l7bnzrk1hadjxjn6r4yzz833mgsvaai9sd8qg022x2l"))))
    (properties `((upstream-name . "mailcommon")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("gnupg" ,gnupg)
       ("qttools" ,qttools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("gpgme" ,gpgme)
       ("karchive" ,karchive)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kdbusaddons" ,kdbusaddons)
       ("kdesignerplugin" ,kdesignerplugin)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kldap" ,kldap)
       ("kmailimporter" ,kmailimporter)
       ("kmailtransport" ,kmailtransport)
       ("kmessagelib" ,kmessagelib)
       ("kmime" ,kmime)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("libkleo" ,libkleo)
       ("libxslt" ,libxslt)
       ("phonon" ,phonon)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f))  ;; TODO: 4/56 tests fail, even with "offscreen" and dbus
    (home-page "https://invent.kde.org/pim/mailcommon")
    (synopsis "KDE email utility library")
    (description "The mail common library provides utility functions for
dealing with email.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmailimporter
  (package
    (name "kmailimporter")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/mailimporter-" version ".tar.xz"))
       (sha256
        (base32 "1929pw0shdzi0yvjnqhak680hjjibg8f8hqy3svyxxhiqbhfjm26"))))
    (properties `((upstream-name . "mailimporter")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("karchive" ,karchive)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kmime" ,kmime)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/mailimporter")
    (synopsis "KDE mail importer library")
    (description "This package provides libraries for importing mails other
e-mail client programs into KMail and KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmailtransport
  (package
    (name "kmailtransport")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmailtransport-" version ".tar.xz"))
       (sha256
        (base32 "1swqlgzxzlqffm119sbhszy9lr93m8lzwygr0q4raa660b6yiavm"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("cyrus-sasl" ,cyrus-sasl)
       ("kcalendarcore" ,kcalendarcore)
       ("kcmutils" ,kcmutils)
       ("kcontacts" ,kcontacts)
       ("kdbusaddons" ,kdbusaddons)
       ("kconfigwidgets" ,kconfigwidgets)
       ("ki18n" ,ki18n)
       ("kitemmodels" ,kitemmodels)
       ("kio" ,kio)
       ("kmime" ,kmime)
       ("ksmtp" ,ksmtp)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("libkgapi" ,libkgapi)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f)) ;; TODO - 3/3 tests fail, require drkonqi
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Mail transport service library")
    (description "This library provides an API and support code for managing
mail transport.")
    (license license:lgpl2.0+)))

(define-public kmbox
  (package
    (name "kmbox")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmbox-" version ".tar.xz"))
       (sha256
        (base32 "03cny38v4y1lmcrs6d34hbj9assqgf51rqryf5rdzkiaq79c1krc"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kmime" ,kmime)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library for handling mbox mailboxes")
    (description "This is a library for handling mailboxes in mbox format,
using a Qt/KMime C++ API.")
    (license license:lgpl2.0+ )))

(define-public kmessagelib
  (package
    (name "kmessagelib")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/messagelib-" version ".tar.xz"))
       (sha256
        (base32 "03vq4962bhps2j9c9i52majlbkmvg2gmr197igv8xamja1vs8hk1"))))
    (properties `((upstream-name . "messagelib")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("gnupg" ,gnupg)
       ("libxml2" ,libxml2)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-notes" ,akonadi-notes)
       ("akonadi-search" ,akonadi-search)
       ("boost" ,boost)
       ("gpgme" ,gpgme)
       ("grantlee" ,grantlee)
       ("grantleetheme" ,grantleetheme)
       ("karchive" ,karchive)
       ("kcalendarcore" ,kcalendarcore)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kdbusaddons" ,kdbusaddons)
       ("kdepim-apps-libs" ,kdepim-apps-libs)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kldap" ,kldap)
       ("kmailtransport" ,kmailtransport)
       ("kmbox" ,kmbox)
       ("kmime" ,kmime)
       ("knewstuff" ,knewstuff)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("kservice" ,kservice)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libgravatar" ,libgravatar)
       ("libkdepim" ,libkdepim)
       ("libkleo" ,libkleo)
       ("qca" ,qca)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ("qtwebkit" ,qtwebkit)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f     ;TODO many test fail for quite different reasons
       #:phases (modify-phases %standard-phases
                  (add-after 'set-paths 'extend-CPLUS_INCLUDE_PATH
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; FIXME: One of the compilation steps fail to find
                      ;; <Libkdepim/MultiplyingLine> without this hack.
                      (setenv "CPLUS_INCLUDE_PATH"
                              (string-append (assoc-ref inputs "libkdepim")
                                             "/include/KF5:"
                                             (or (getenv "CPLUS_INCLUDE_PATH") "")))
                      #t)))))
    (home-page "https://invent.kde.org/pim/messagelib")
    (synopsis "KDE PIM messaging libraries")
    (description "This package provides several libraries for messages,
e.g. a message list, a mime tree parse, a template parser and the
kwebengineviewer.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmime
  (package
    (name "kmime")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kmime-" version ".tar.xz"))
       (sha256
        (base32 "1dkdxfr1ry10qyql5sp1ai4li11f0ncf9hipg27j59y70mlyrl2r"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-case
           (lambda _
             ;; This is curious: autotests/CMakeLists.txt sets LC_TIME=C, but
             ;; the Qt locale returns different. See kmime commit 3a9651d26a.
             (substitute* "autotests/dateformattertest.cpp"
               (("(Today|Yesterday) 12:34:56" line day)
                (string-append day " 12:34 PM")))
             #t)))))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library for handling MIME data")
    (description "This library provides an API for handling MIME
data.  MIME (Multipurpose Internet Mail Extensions) is an Internet Standard
that extends the format of e-mail to support text in character sets other than
US-ASCII, non-text attachments, multi-part message bodies, and header
information in non-ASCII character sets.")
    (license license:lgpl2.0+)))

(define-public knotes
  (package
    (name "knotes")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/knotes-" version ".tar.xz"))
       (sha256
        (base32 "13h4n7fb5p6g1f5kmw6pblpd76j904psm30s3a5d3kykni57dijx"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("libxslt" ,libxslt)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-notes" ,akonadi-notes)
       ("akonadi-search" ,akonadi-search)
       ("boost" ,boost)
       ("grantlee" ,grantlee)
       ("grantleetheme" ,grantleetheme)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdnssd" ,kdnssd)
       ("kdoctools" ,kdoctools)
       ("kglobalaccel" ,kglobalaccel)
       ("kiconthemes" ,kiconthemes)
       ("kimap" ,kimap)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kmime" ,kmime)
       ("kmime" ,kmime)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kontactinterface" ,kontactinterface)
       ("kparts" ,kparts)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("oxygen-icons" ,oxygen-icons) ; default icon set, required for tests
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://kontact.kde.org/components/knotes.html")
    (synopsis "Note-taking utility")
    (description "KNotes lets you write the computer equivalent of sticky
notes.  The notes are saved automatically when you exit the program, and they
display when you open the program.

Features:
@itemize
@item Write notes in your choice of font and background color
@item Use drag and drop to email your notes
@item Can be dragged into Calendar to book a time-slot
@item Notes can be printed
@end itemize")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public kontactinterface
  (package
    (name "kontactinterface")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kontactinterface-" version ".tar.xz"))
       (sha256
        (base32 "0s1qm1wjkvbb1film94r7g88d8vgh26bm0hm6gpyqv5bazw5qx3j"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kparts" ,kparts)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Kontact interface library")
    (description " This library provides the glue necessary for
application \"Parts\" to be embedded as a Kontact component (or plugin).")
    (license license:lgpl2.0+)))

(define-public korganizer
  (package
    (name "korganizer")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/korganizer-" version ".tar.xz"))
       (sha256
        (base32 "04lz3ldrr0lpy9zpsg9ja1i9gxzlcjpqcwn3g7l4jjdky4frcr2r"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("qttools" ,qttools)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-calendar" ,akonadi-calendar)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-notes" ,akonadi-notes)
       ("akonadi-search" ,akonadi-search)
       ("boost" ,boost)
       ("kcalendarcore" ,kcalendarcore)
       ("kcalendarsupport" ,kcalendarsupport)
       ("kcalutils" ,kcalutils)
       ("kcmutils" ,kcmutils)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdepim-apps-libs" ,kdepim-apps-libs)
       ("keventviews" ,keventviews)
       ("kholidays" ,kholidays)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kimap" ,kimap)
       ("kincidenceeditor" ,kincidenceeditor)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kldap" ,kldap)
       ("kmailtransport" ,kmailtransport)
       ("kmime" ,kmime)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kontactinterface" ,kontactinterface)
       ("kparts" ,kparts)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("kservice" ,kservice)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("oxygen-icons" ,oxygen-icons) ; default icon set, required for tests
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; FIXME: This test started failing after the 20.04 update
             ;; seemingly due to DBus communication issues.
             ;; See also 'akonadi-search' for a similar test failure.
             (substitute* "src/autotests/CMakeLists.txt"
               ((".*test_advanced\\(koeventpopupmenutest\\.cpp.*")
                ""))
             #t))
         (replace 'check
           (lambda _
             (invoke "dbus-launch" "ctest" ".")
             #t)))))
    (home-page "https://kontact.kde.org/components/korganizer.html")
    (synopsis "Organizational assistant, providing calendars and other similar
functionality to help you organize your life.")
    (description "KOrganizer is the calendar and scheduling component of
Kontact.  It provides management of events and tasks, alarm notification, web
export, network transparent handling of data, group scheduling, import and
export of calendar files and more.  It is able to work together with a wide
variety of calendaring services, including NextCloud, Kolab, Google Calendar
and others.  KOrganizer is fully customizable to your needs and is an integral
part of the Kontact suite, which aims to be a complete solution for organizing
your personal data.  KOrganizer supports the two dominant standards for storing
and exchanging calendar data, vCalendar and iCalendar.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kpeoplevcard
  (package
    (name "kpeoplevcard")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.kde.org/stable/kpeoplevcard/"
                                  version "/kpeoplevcard-" version ".tar.xz"))
              (sha256
               (base32
                "1hv3fq5k0pps1wdvq9r1zjnr0nxf8qc3vwsnzh9jpvdy79ddzrcd"))))
    (build-system qt-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check-setup
                    (lambda _
                      (setenv "HOME" "/tmp")
                      #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcontacts" ,kcontacts)
       ("kpeople" ,kpeople)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/kpeoplevcard")
    (synopsis "Expose vCard contacts to KPeople")
    (description
     "This plugins adds support for vCard (also known as @acronym{VCF,
Virtual Contact File}) files to the KPeople contact management library.")
    (license license:lgpl2.1+)))

(define-public kpimcommon
  (package
    (name "kpimcommon")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/pimcommon-" version ".tar.xz"))
       (sha256
        (base32 "15lfqv5w4iwyjlvf4idykpkjgppl0ic59r4dw95qkbbjkps0nr7j"))))
    (properties `((upstream-name . "pimcommon")))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("karchive" ,karchive)
       ("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("boost" ,boost)
       ("grantlee" ,grantlee)
       ;; TODO: ("kaccounts" ,kaccounts)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kdesignerplugin" ,kdesignerplugin)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kirigami" ,kirigami) ;; run-time dependency
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kmime" ,kmime)
       ("knewstuff" ,knewstuff)
       ("kpimtextedit" ,kpimtextedit)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libkdepim" ,libkdepim)
       ("libxslt" ,libxslt)
       ("purpose" ,purpose)
       ("qtbase" ,qtbase)
       ("qtwebengine" ,qtwebengine)))
    (arguments
     `(#:tests? #f)) ;; TODO tests hang
    (home-page "https://invent.kde.org/pim/pimcommon")
    (synopsis "Common libraries for KDE PIM")
    (description "This package provides common libraries for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kpimtextedit
  (package
    (name "kpimtextedit")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kpimtextedit-" version ".tar.xz"))
       (sha256
        (base32 "0j6d4sv405c3x0ww75qsww94apidsb8aaqf59akhv96zmv0vx5wy"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("grantlee" ,grantlee)
       ("kcodecs" ,kcodecs)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdesignerplugin" ,kdesignerplugin)
       ("kemoticons" ,kemoticons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtspeech" ,qtspeech)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f)) ;; TODO - test suite hangs
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/")
    (synopsis "Library providing a textedit with PIM-specific features")
    (description "This package provides a textedit with PIM-specific features.
It also provides so-called rich text builders which can convert the formatted
text in the text edit to all kinds of markup, like HTML or BBCODE.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public ksmtp
  (package
    (name "ksmtp")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksmtp-" version ".tar.xz"))
       (sha256
        (base32 "1xyaahibm0dc3qdwiak5yqa66szxaxnylvqxi6k21ayvzn2vxbhx"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ;; TODO: does not find sasl mechs
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'Use-KDE_INSTALL_TARGETS_DEFAULT_ARGS-when-installing
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("^(install\\(.* )\\$\\{KF5_INSTALL_TARGETS_DEFAULT_ARGS\\}\\)"
                 _ prefix)
                (string-append prefix "${KDE_INSTALL_TARGETS_DEFAULT_ARGS})")))
             #t)))))
    (home-page "https://invent.kde.org/pim/ksmtp")
    (synopsis "Library for sending email through an SMTP server")
    (description "This library provides an API for handling SMTP
services.  SMTP (Simple Mail Transfer Protocol) is the most prevalent Internet
standard protocols for e-mail transmission.")
    (license license:lgpl2.0+)))

(define-public ktnef
  (package
    (name "ktnef")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktnef-" version ".tar.xz"))
       (sha256
        (base32 "0cn5p32w2kas56yyc15c22kll4hd02lvvxz2n6cz1wda8alspj19"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcalendarcore" ,kcalendarcore)
       ("kcalutils" ,kcalutils)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/ktnef/html/")
    (synopsis "Library for handling mail attachments using TNEF format")
    (description "Ktnef is a library for handling data in the TNEF
format (Transport Neutral Encapsulation Format, a proprietary format of e-mail
attachment used by Microsoft Outlook and Microsoft Exchange Server).  The API
permits access to the actual attachments, the message properties (TNEF/MAPI),
and allows one to view/extract message formatted text in Rich Text Format.")
    (license license:lgpl2.0+)))

(define-public libkdepim
  (package
    (name "libkdepim")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkdepim-" version ".tar.xz"))
       (sha256
        (base32 "0bask561laxgkgm3rxfpyxqs6jx1l9xjk058lhycq0pik6vwhdha"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("akonadi-contacts" ,akonadi-contacts)
       ("akonadi-mime" ,akonadi-mime)
       ("akonadi-search" ,akonadi-search)
       ("boost" ,boost)
       ("kcmutils" ,kcmutils)
       ("kcodecs" ,kcodecs)
       ("kcalendarcore" ,kcalendarcore)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdesignerplugin" ,kdesignerplugin)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjobwidgets" ,kjobwidgets)
       ("kldap" ,kldap)
       ("kmime" ,kmime)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/libkdepim")
    (synopsis "Libraries for common KDE PIM apps")
    (description "This package provided libraries for common KDE PIM apps.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libkgapi
  (package
    (name "libkgapi")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkgapi-" version ".tar.xz"))
       (sha256
        (base32 "0nvd5fqrvyb7c3g7rf1lxbbv38q9sqnhd6irgx7awwgw92inxky4"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("ki18n" ,ki18n)
       ("kcontacts" ,kcontacts)
       ("kcalendarcore" ,kcalendarcore)
       ("kio" ,kio)
       ("kwallet" ,kwallet)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)))
    (arguments
     `(#:tests? #f)) ;; TODO 6/48 tests fail
    (home-page "https://invent.kde.org/pim/libkgapi")
    (synopsis "Library for accessing various Google services via their public
API")
    (description "@code{LibKGAPI} is a C++ library that implements APIs for
various Google services.")
    (license license:lgpl2.0+)))

(define-public libkleo
  (package
    (name "libkleo")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkleo-" version ".tar.xz"))
       (sha256
        (base32 "0rijpmqyx4mrr7csik3vkfcra7kfywk6yz548fmq3ha8wa9ax8fv"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("gpgme" ,gpgme)
       ("kcodecs" ,kcodecs)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kitemmodels" ,kitemmodels)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kpimtextedit" ,kpimtextedit)
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/pim/libkleo")
    (synopsis "KDE PIM cryptographic library")
    (description "@code{libkleo} is a library for Kleopatra and other parts of
KDE using certificate-based crypto.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libksieve
  (package
    (name "libksieve")
    (version "20.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libksieve-" version ".tar.xz"))
       (sha256
        (base32 "04k2nkwg5vlgbr5wpvsq02wi54ljsy4ka7y3ns5x3d2gb06wp03c"))
       (patches (search-patches "libksieve-Fix-missing-link-libraries.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("akonadi" ,akonadi)
       ("cyrus-sasl" ,cyrus-sasl)
       ("karchive" ,karchive)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidentitymanagement" ,kidentitymanagement)
       ("kimap" ,kimap)
       ("kio" ,kio)
       ("kmailtransport" ,kmailtransport)
       ("kmime" ,kmime)
       ("knewstuff" ,knewstuff)
       ("kpimcommon" ,kpimcommon)
       ("kpimtextedit" ,kpimtextedit)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwindowsystem" ,kwindowsystem)
       ("libkdepim" ,libkdepim)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'substitute
           (lambda _
             ;; Disable a failing test
             ;; sieveeditorhelphtmlwidgettest fails with `sigtrap`
             (substitute*
                 "src/ksieveui/editor/webengine/autotests/CMakeLists.txt"
               (("^\\s*(add_test|ecm_mark_as_test)\\W" line)
                (string-append "# " line)))
             ;; FIXME: This test fails due to time zone problems.
             (substitute*
                 "src/ksieveui/autocreatescripts/autotests/CMakeLists.txt"
               ((".*sieveeditorgraphicalmodewidgettest\\.cpp.*")
                ""))
             #t)))))
    (home-page "https://invent.kde.org/pim/libksieve")
    (synopsis "KDE Sieve library")
    (description "Sieve is a language that can be used filter emails.  KSieve
is a Sieve parser and interpreter library for KDE.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))
