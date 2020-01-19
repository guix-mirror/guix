;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-" version ".tar.xz"))
       (sha256
        (base32 "0v7f1049wjnqxhwxr1443wc2cfbdqmf15xcwjz3j1m0vgdva9pyg"))
       (patches (search-patches
                 "akonadi-paths.patch"
                 "akonadi-timestamps.patch"
                 "akonadi-Revert-Make-installation-properly-relo.patch"))))
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
       ("kdbusaddons" ,kdbusaddons)
       ("kdesignerplugin" ,kdesignerplugin)
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-calendar-" version ".tar.xz"))
       (sha256
        (base32 "1550h08i8rjnbd9yrnhd9v3v68ingrag2bdxrbid62qvam0n5ihy"))))
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
       ("kdbusaddons" ,kdbusaddons)
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
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/akonadi/html/")
    (synopsis "Library providing calendar helpers for Akonadi items")
    (description "This library manages calendar specific actions for
collection and item views.")
    (license license:lgpl2.0+)))

(define-public akonadi-contacts
  (package
    (name "akonadi-contacts")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-contacts-" version ".tar.xz"))
       (sha256
        (base32 "1pw1s8c6dlcb103cw46p1ikvas3y8cwiwnfdny2jd3hr3rig4px9"))))
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
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/akonadi/html/")
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-mime-" version ".tar.xz"))
       (sha256
        (base32 "03q3dnhzcgmgcqvijnwi4ikg0m1zad2l679bqnp051v27fvs4yg7"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("libxslt" ,libxslt) ;; xslt for generating interface descriptions
       ("shared-mime-info" ,shared-mime-info)))
    (inputs
     `(("akonadi" ,akonadi)
       ("boost", boost)
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
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/akonadi/html/")
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-notes-" version ".tar.xz"))
       (sha256
        (base32 "0r8vh11bfjzhspb5kp2d0kcgwqd2m5qpxpamiajzjq910f51sw3w"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi" ,akonadi)
       ("kcodecs" ,kcodecs)
       ("ki18n" ,ki18n)
       ("kmime" ,kmime)
       ("qtbase" ,qtbase)))
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/akonadi/html/")
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/akonadi-search-" version ".tar.xz"))
       (sha256
        (base32 "16qzs2cs4nxwrpwcdgwry95qn6wmg8s1p4w3qajx1ahkgwmsh11s"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
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
     `(#:tests? #f)) ;; TODO: needs dbus
    (home-page "https://api.kde.org/stable/kdepimlibs-apidocs/akonadi/html/")
    (synopsis "Akonadi search library")
    (description "This package provides a library used to search in the
Akonadi PIM data server.  It uses Xapian for indexing and querying.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kaddressbook
  (package
    (name "kaddressbook")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kaddressbook-" version ".tar.xz"))
       (sha256
        (base32 "1bpl9cqjv7s6pnsaa266jqmny2s6ldkylxviri162jxg51v1hhz3"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kalarmcal-" version ".tar.xz"))
       (sha256
        (base32 "0w9qsx2gqwny2v4fsj4awn814s9b7yrxvqrawlick3r2kp4x1sgn"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("akonadi", akonadi)
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kblog-" version ".tar.xz"))
       (sha256
        (base32 "0r3ik3df444kzg2mnzckkh4kk6v08zil1f26dwmxsslsqw9hl0vv"))))
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
    (home-page "https://cgit.kde.org/kblog.git")
    (synopsis "Client-side support library for web application remote blogging
APIs")
    (description "KBlog is a library for calling functions on Blogger 1.0,
MetaWeblog, MovableType and GData compatible blogs.  It calls the APIs using
KXmlRpcClient and Syndication.  It supports asynchronous sending and fetching
of posts and, if supported on the server, multimedia files.  Almost every
modern blogging web application that provides an XML data interface supports
one of the APIs mentioned above.")
    (license license:lgpl2.0+)))

(define-public kcalutils
  (package
    (name "kcalutils")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kcalutils-" version ".tar.xz"))
       (sha256
        (base32 "1nlkik4qiciyh1slgpis3n5h9pks2ygdba9yq4s16nnmip4l45w2"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kdav-" version ".tar.xz"))
       (sha256
        (base32 "1w59n17lridglphnm4mnmmzq1ijpbp269qxfmz01vk6wry6hlnp8"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("qtbase" ,qtbase)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://cgit.kde.org/kdav.git")
    (synopsis "DAV protocol implementation with KJobs")
    (description "This is a DAV protocol implemention with KJobs.  Calendars
and todos are supported, using either GroupDAV or CalDAV, and contacts are
supported using GroupDAV or CardDAV.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kdepim-apps-libs
  (package
    (name "kdepim-apps-libs")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kdepim-apps-libs-" version ".tar.xz"))
       (sha256
        (base32 "10xbzvp9cm5fpy4nxp38qm4vf0bycpq94bm4z2j4lw7ll1aq8irw"))))
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
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
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
    (home-page "https://cgit.kde.org/kdepim-apps-libs.git")
    (synopsis "KDE PIM mail related libraries and data files")
    (description "This packages provides mail related libraries and data files
for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kdepim-runtime
  (package
    (name "kdepim-runtime")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kdepim-runtime-" version ".tar.xz"))
       (sha256
        (base32 "1skid9v6viw379mwhmb4xjh6bylv8wg7cy56kkbcpsmpars9cwr6"))
       (patches (search-patches
                 "kdepim-runtime-Fix-missing-link-libraries.patch"))))
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
       ("kdbusaddons" ,kdbusaddons)
       ("kholidays" ,kholidays)
       ("kiconthemes" ,kiconthemes)
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
     `(#:tests? #f))
    (home-page "https://cgit.kde.org/kdepim-runtime.git")
    (synopsis "Runtime components for Akonadi KDE")
    (description "This package contains Akonadi agents written using KDE
Development Platform libraries.  Any package that uses Akonadi should probably
pull this in as a dependency.  The kres-bridges is also parts of this
package.")
    (license ;; Files vary a lot regarding the license. GPL2+ and LGPL2.1+
     ;; have been used in those I checked. But the archive also includes
     ;; license texts for GPL3 and AGPL3.
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kgpg
  (package
    (name "kgpg")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kgpg-" version ".tar.xz"))
       (sha256
        (base32 "1dis7zv51a4lhx5l3wlwnhym8f79h8sibhhk97fkn8d7szdrmfw5"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kidentitymanagement-" version ".tar.xz"))
       (sha256
        (base32 "0dqz49sp5hq44590rrxav8688aqlzsww4q4n55ksfy13nk9i5mbf"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kimap-" version ".tar.xz"))
       (sha256
        (base32 "0l8hb2z82jzbwr12lw5fismwk1a3ca4dk966p1fxg4bibck8vjj6"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kldap-" version ".tar.xz"))
       (sha256
        (base32 "1blbnj8av6h168g14gyphyd9sz87af773b1qglmbkv5pzbzaanxn"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kleopatra-" version ".tar.xz"))
       (sha256
        (base32 "1bqwxdl91s2nai871vvhkmcc3simbnvr2i5m6dnl327bplzqgfa4"))))
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

(define-public kmailcommon
  (package
    (name "kmailcommon")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/mailcommon-" version ".tar.xz"))
       (sha256
        (base32 "1gsj89kgq4457mnfjlys4wiixpzwlbwhj4zpd7r4fdhbyihz3k2m"))))
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
    (home-page "https://cgit.kde.org/mailcommon.git")
    (synopsis "KDE email utility library")
    (description "The mail common library provides utility functions for
dealing with email.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmailimporter
  (package
    (name "kmailimporter")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/mailimporter-" version ".tar.xz"))
       (sha256
        (base32 "0vmrgjz47f96crrbv0bhaz0abh2am4whhb294rfz02mvjghbzpzv"))))
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
    (home-page "https://cgit.kde.org/mailimporter.git")
    (synopsis "KDE mail importer library")
    (description "This package provides libraries for importing mails other
e-mail client programs into KMail and KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmailtransport
  (package
    (name "kmailtransport")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kmailtransport-" version ".tar.xz"))
       (sha256
        (base32 "04jdnqxbp4382vjxh06rrvsigbrygqfkw0fvbbjnjymp585mgkr4"))))
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
       ("kitemmodels", kitemmodels)
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kmbox-" version ".tar.xz"))
       (sha256
        (base32 "13b5v1nx46k5ais3cms7yxrfi8p6xbljpkpg3f7v1asb6kshv7g2"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/messagelib-" version ".tar.xz"))
       (sha256
        (base32 "0a378aqkdjzyzlxxha2qxa6vzrj92l1fplzb6fajz4l7ipj4hbnv"))))
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
       ("qgpgme" ,qgpgme)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)
       ("qtwebkit" ,qtwebkit)
       ("sonnet" ,sonnet)))
    (arguments
     `(#:tests? #f)) ;; TODO many test fail for quite different reasons
    (home-page "https://cgit.kde.org/messagelib.git")
    (synopsis "KDE PIM messaging libraries")
    (description "This packages provides several libraries for messages,
e.g. a message list, a mime tree parse, a template parser and the
kwebengineviewer.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kmime
  (package
    (name "kmime")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kmime-" version ".tar.xz"))
       (sha256
        (base32 "1pc00pwwrngsyr7ppvqwfgvcgy2wiqdbqxhv9xidn4dw9way2ng6"))))
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

(define-public kontactinterface
  (package
    (name "kontactinterface")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kontactinterface-" version ".tar.xz"))
       (sha256
        (base32 "1p0iw9i8cxh3jn7094wvxhlpc2sw52q8csfdgch1lf3dwhkpp0k7"))))
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

(define-public kpimcommon
  (package
    (name "kpimcommon")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/pimcommon-" version ".tar.xz"))
       (sha256
        (base32 "1jl40ymq46yjn9va78hklgg91ikrfahf3w4jl5ziiqbivcl7r9kn"))))
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
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcontacts" ,kcontacts)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
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
    (home-page "https://cgit.kde.org/pimcommon.git")
    (synopsis "Common libraries for KDE PIM")
    (description "This package provides common libraries for KDE PIM.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public kpimtextedit
  (package
    (name "kpimtextedit")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/kpimtextedit-" version ".tar.xz"))
       (sha256
        (base32 "1as48j5qfpj9pqjck1615nlpk4a850m7xxcyl41gx8biww027zvm"))))
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
       ("qtspeech", qtspeech)
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/ksmtp-" version ".tar.xz"))
       (sha256
        (base32 "1pd8mma3xbq83jkn76gqinn6xh9imaji0jrg3qzysf5rvjl8kcqn"))))
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
    (home-page "https://cgit.kde.org/ksmtp.git")
    (synopsis "Library for sending email through an SMTP server")
    (description "This library provides an API for handling SMTP
services.  SMTP (Simple Mail Transfer Protocol) is the most prevalent Internet
standard protocols for e-mail transmission.")
    (license license:lgpl2.0+)))

(define-public ktnef
  (package
    (name "ktnef")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/ktnef-" version ".tar.xz"))
       (sha256
        (base32 "0kgfhh46130hg1xq8km5gjzxa3b620j1zdrg54qivxa782smgbl6"))))
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
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/libkdepim-" version ".tar.xz"))
       (sha256
        (base32 "0ndh97w1bfii4snx9yc0qazqk5jhx22s810kj656967xd1w4bj9n"))))
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
    (home-page "https://cgit.kde.org/libkdepim.git")
    (synopsis "Libraries for common KDE PIM apps")
    (description "This package provided libraries for common KDE PIM apps.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libkgapi
  (package
    (name "libkgapi")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/libkgapi-" version ".tar.xz"))
       (sha256
        (base32 "0z76b745n4hhjndrhv1w5acibia8x1frh78jx7bvxa72d8wphn08"))))
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
    (home-page "https://cgit.kde.org/libkgapi.git")
    (synopsis "Library for accessing various Google services via their public
API")
    (description "@code{LibKGAPI} is a C++ library that implements APIs for
various Google services.")
    (license license:lgpl2.0+)))

(define-public libkleo
  (package
    (name "libkleo")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/libkleo-" version ".tar.xz"))
       (sha256
        (base32 "0vjp07j102mi20c4q2fdvkjc0skb9q7msxp64n76wy3cciv346jz"))))
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
    (home-page "https://cgit.kde.org/libkleo.git/")
    (synopsis "KDE PIM cryptographic library")
    (description "@code{libkleo} is a library for Kleopatra and other parts of
KDE using certificate-based crypto.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))

(define-public libksieve
  (package
    (name "libksieve")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/libksieve-" version ".tar.xz"))
       (sha256
        (base32 "0q6f6lc4yvlq0vsfml10lz844z6zxxf7yivk7l3vglap58ci20x1"))
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
             #t)))))
    (home-page "https://cgit.kde.org/libksieve.git")
    (synopsis "KDE Sieve library")
    (description "Sieve is a language that can be used filter emails.  KSieve
is a Sieve parser and interpreter library for KDE.")
    (license ;; GPL for programs, LGPL for libraries
     (list license:gpl2+ license:lgpl2.0+))))
