;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages busybox)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl))

(define-public busybox
  (package
    (name "busybox")
    (version "1.29.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.busybox.net/downloads/" name "-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1dzg45vgy2w1xcd3p6h8d76ykhabbvk1h0lf8yb24ikrwlv8cr4p"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-taskset
           ;; This feature fails its tests in the build environment,
           ;; was default 'n' until after 1.26.2.
           (lambda _
             (substitute* "util-linux/taskset.c"
               (("default y") "default n"))
             #t))
         (replace 'configure
           (lambda _ (invoke "make" "defconfig")))
         (replace 'check
           (lambda _
             (substitute* '("testsuite/du/du-s-works"
                            "testsuite/du/du-works")
               (("/bin") "/etc"))  ; there is no /bin but there is a /etc

             ;; There is no /usr/bin or /bin - replace it with /gnu/store
             (substitute* "testsuite/cpio.tests"
               (("/usr/bin") (%store-directory))
               (("usr") (car (filter (negate string-null?)
                                     (string-split (%store-directory) #\/)))))

             (substitute* "testsuite/date/date-works-1"
               (("/bin/date") (which "date")))

             ;; The pidof tests assume that pid 1 is called "init" but that is not
             ;; true in guix build environment
             (substitute* "testsuite/pidof.tests"
               (("-s init") "-s $(cat /proc/1/comm)"))
  
             ;; This test cannot possibly pass.
             ;; It is trying to test that "which ls" returns "/bin/ls" when PATH is not set.
             ;; However, this relies on /bin/ls existing.  Which it does not in guix.
             (delete-file "testsuite/which/which-uses-default-path")
             (rmdir "testsuite/which")

             (invoke "make"
                     ;; "V=1"
                     "SKIP_KNOWN_BUGS=1"
                     "SKIP_INTERNET_TESTS=1"
                     "check")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       (string-append "CONFIG_PREFIX=" out)
                       "install")))))))
    (native-inputs `(("perl" ,perl) ; needed to generate the man pages (pod2man)
                     ;; The following are needed by the tests.
                     ("inetutils" ,inetutils)
                     ("which" ,(@ (gnu packages base) which))
                     ("zip" ,zip)))
    (synopsis "Many common UNIX utilities in a single executable")
    (description "BusyBox combines tiny versions of many common UNIX utilities
into a single small executable.  It provides a fairly complete environment for
any small or embedded system.")
    (home-page "https://www.busybox.net")
    ;; Some files are gplv2+
    (license gpl2)))

(define-public toybox
  (package
    (name "toybox")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://landley.net/toybox/downloads/toybox-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0mirj977zxsxnfaiqndwgsn9calgg312d817fi1hkfbd8kcyrk73"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-environment-variables
           (lambda _
             (setenv "CC" (which "gcc"))
             (setenv "HOSTCC" (which "gcc"))
             #t))
         (replace 'configure
           (lambda _ (invoke "make" "defconfig")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       (string-append "PREFIX=" out)
                       "install")))))
       #:test-target "tests"))
    (native-inputs `(("bc" ,bc)))
    (synopsis "Many common UNIX utilities in a single executable")
    (description "ToyBox combines tiny versions of many common UNIX utilities
into a single small executable.  It provides a fairly complete environment for
any small or embedded system.")
    (home-page "https://landley.net/toybox/")
    (license bsd-2)))
