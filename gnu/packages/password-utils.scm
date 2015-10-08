;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2015 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages password-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public pwgen
  (package
    (name "pwgen")
    (version "2.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pwgen/pwgen-"
                           version ".tar.gz"))
       (sha256
        (base32 "0mhmw700kkh238fzivcwnwi94bj9f3h36yfh3k3j2v19b0zmjx7b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (home-page "http://pwgen.sourceforge.net/")
    (synopsis "Password generator")
    (description "Pwgen generates passwords which can be easily memorized by a
human.")
    (license license:gpl2)))

(define-public keepassx
  (package
    (name "keepassx")
    (version "2.0-beta2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/keepassx/keepassx/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ljf9ws3wh62zd0gyb0vk2qw6pqsmxrlybrfs5mqahf44q92ca2q"))))
    (build-system cmake-build-system)
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libxtst" ,libxtst)
       ("qt" ,qt-4)))
    (native-inputs
     `(("zlib" ,zlib)))
    (home-page "https://www.keepassx.org")
    (synopsis "Password manager")
    (description "KeePassX is a password manager or safe which helps you to
manage your passwords in a secure way.  You can put all your passwords in one
database, which is locked with one master key or a key-file which can be stored
on an external storage device.  The databases are encrypted using the
algorithms AES or Twofish.")
    ;; Non functional parts use various licences.
    (license license:gpl3)))

(define-public shroud
  (package
    (name "shroud")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://files.dthompson.us/shroud/shroud-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1y43yhgy2zbrk5bqj3qyx9rkcz2bma9sinlrg7dip3jqms9gq4lr"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.0)
       ("gnupg" ,gnupg)
       ("xclip" ,xclip)))
    (synopsis "GnuPG-based secret manager")
    (description "Shroud is a simple secret manager with a command line
interface.  The password database is stored as a Scheme s-expression and
encrypted with a GnuPG key.  Secrets consist of an arbitrary number of
key/value pairs, making Shroud suitable for more than just password storage.
For copying and pasting secrets into web browsers and other graphical
applications, there is xclip integration." )
    (home-page "http://dthompson.us/pages/software/shroud.html")
    (license license:gpl3+)))
