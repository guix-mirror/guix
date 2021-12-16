;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages php)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public php
  (package
    (name "php")
    (version "7.4.26")
    (home-page "https://secure.php.net/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "distributions/"
                                  "php-" version ".tar.xz"))
              (sha256
               (base32
                "1y0f1xgfi8cks6npdhrycg8r9g3q0pikqgf5h4xafpy8znmb61g3"))
              (patches
               (search-patches "php-bug-74093-test.patch"
                               "php-openssl_x509_checkpurpose_basic.patch"))
              (modules '((guix build utils)))
              (snippet
               '(with-directory-excursion "ext"
                  (for-each delete-file-recursively
                            ;; Some of the bundled libraries have no proper upstream.
                            ;; Ideally we'd extract these out as separate packages:
                            ;;"mbstring/libmbfl"
                            ;;"date/lib"
                            ;;"bcmath/libbcmath"
                            ;;"fileinfo/libmagic" ; a patched version of libmagic
                            '("gd/libgd"
                              "pcre/pcre2lib"
                              "xmlrpc/libxmlrpc"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let-syntax ((with (syntax-rules ()
                            ((_ option input)
                             (string-append option "="
                                            (assoc-ref %build-inputs input))))))
         (list (with "--with-bz2" "bzip2")
               (with "--with-curl" "curl")
               (with "--with-gdbm" "gdbm")
               (with "--with-gettext" "libc")  ; libintl.h
               (with "--with-gmp" "gmp")
               (with "--with-ldap" "openldap")
               (with "--with-ldap-sasl" "cyrus-sasl")
               (with "--with-pdo-pgsql" "postgresql")
               (with "--with-pdo-sqlite" "sqlite")
               (with "--with-pgsql" "postgresql")
               ;; PHP’s Pspell extension, while retaining its current name,
               ;; now uses the Aspell library.
               (with "--with-pspell" "aspell")
               (with "--with-readline" "readline")
               (with "--with-sodium" "libsodium")
               (with "--with-sqlite3" "sqlite")
               (with "--with-tidy" "tidy")
               (with "--with-xsl" "libxslt")
               (with "--with-zlib-dir" "zlib")
               ;; We could add "--with-snmp", but it requires netsnmp that
               ;; we don't have a package for. It is used to build the snmp
               ;; extension of php.
               "--with-external-pcre"
               "--with-external-gd"
               "--with-iconv"
               "--with-openssl"
               "--with-mysqli"          ; Required for, e.g. wordpress
               "--with-pdo-mysql"
               "--with-zip"
               "--with-zlib"
               "--enable-bcmath"        ; Required for, e.g. Zabbix frontend
               "--enable-calendar"
               "--enable-dba=shared"
               "--enable-exif"
               "--enable-flatfile"
               "--enable-fpm"
               "--enable-ftp"
               "--enable-gd"
               "--enable-inifile"
               "--enable-intl"
               "--enable-mbstring"
               "--enable-pcntl"
               "--enable-sockets"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-record-build-flags
           (lambda _
             ;; Prevent configure flags from being stored and causing
             ;; unnecessary runtime dependencies.
             (substitute* "scripts/php-config.in"
               (("@CONFIGURE_OPTIONS@") "")
               (("@PHP_LDFLAGS@") ""))
             ;; This file has ISO-8859-1 encoding.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "main/build-defs.h.in"
                 (("@CONFIGURE_COMMAND@") "(omitted)")))))
         (add-before 'build 'patch-/bin/sh
           (lambda _
             (substitute* '("run-tests.php" "ext/standard/proc_open.c")
               (("/bin/sh") (which "sh")))))
         (add-before 'check 'prepare-tests
           (lambda _
             ;; Some of these files have ISO-8859-1 encoding, whereas others
             ;; use ASCII, so we can't use a "catch-all" find-files here.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* '("ext/mbstring/tests/mb_send_mail02.phpt"
                              "ext/mbstring/tests/mb_send_mail04.phpt"
                              "ext/mbstring/tests/mb_send_mail05.phpt"
                              "ext/mbstring/tests/mb_send_mail06.phpt")
                 (("/bin/cat") (which "cat"))))
             (substitute* '("ext/mbstring/tests/mb_send_mail01.phpt"
                            "ext/mbstring/tests/mb_send_mail03.phpt"
                            "ext/mbstring/tests/bug52681.phpt"
                            "ext/standard/tests/general_functions/bug34794.phpt"
                            "ext/standard/tests/general_functions/bug44667.phpt"
                            "ext/standard/tests/general_functions/proc_open.phpt")
               (("/bin/cat") (which "cat")))

             ;; The encoding of this file is not recognized, so we simply drop it.
             (delete-file "ext/mbstring/tests/mb_send_mail07.phpt")

             (substitute* "ext/standard/tests/streams/bug60602.phpt"
               (("'ls'") (string-append "'" (which "ls") "'")))

             ,@(if (string-prefix? "arm" (or (%current-system)
                                             (%current-target-system)))
                   ;; Drop tests known to fail on armhf.
                   '((for-each delete-file
                              (list
                                "ext/calendar/tests/unixtojd_error1.phpt"
                                ;; arm can be a lot slower, so a time-related test fails
                                "ext/fileinfo/tests/cve-2014-3538-nojit.phpt"
                                "ext/pcntl/tests/pcntl_unshare_01.phpt"
                                "ext/pcre/tests/bug76514.phpt"
                                "ext/pcre/tests/preg_match_error3.phpt"
                                "ext/pcre/tests/cache_limit.phpt"
                                "ext/sockets/tests/socket_getopt.phpt"
                                "ext/sockets/tests/socket_sendrecvmsg_error.phpt"
                                "ext/standard/tests/general_functions/var_export-locale.phpt"
                                "ext/standard/tests/general_functions/var_export_basic1.phpt"
                                "ext/intl/tests/timezone_getErrorCodeMessage_basic.phpt"
                                "ext/intl/tests/timezone_getOffset_error.phpt"
                                "sapi/cli/tests/cli_process_title_unix.phpt"
                                "sapi/cli/tests/upload_2G.phpt"
                                "Zend/tests/concat_003.phpt")))
                   '())

             ,@(if (target-ppc64le?)
                   ;; Drop tests known to fail on powerpc64le.
                   '((for-each delete-file
                               (list
                                ;; phpdbg watchpoints don't work.
                                ;; Bug tracked upstream at:
                                ;; https://bugs.php.net/bug.php?id=81408
                                "sapi/phpdbg/tests/watch_001.phpt"
                                "sapi/phpdbg/tests/watch_003.phpt"
                                "sapi/phpdbg/tests/watch_004.phpt"
                                "sapi/phpdbg/tests/watch_005.phpt"
                                "sapi/phpdbg/tests/watch_006.phpt")))
                   '())

             ;; Drop tests that are known to fail.
             (for-each delete-file
                       '("ext/posix/tests/posix_getgrgid.phpt"    ; Requires /etc/group.
                         "ext/posix/tests/posix_getgrnam_basic.phpt" ; Requires /etc/group.
                         "ext/sockets/tests/bug63000.phpt"        ; Fails to detect OS.
                         "ext/sockets/tests/socket_shutdown.phpt" ; Requires DNS.
                         "ext/sockets/tests/socket_send.phpt"     ; Likewise.
                         "ext/sockets/tests/mcast_ipv4_recv.phpt" ; Requires multicast.
                         ;; These needs /etc/services.
                         "ext/standard/tests/general_functions/getservbyname_basic.phpt"
                         "ext/standard/tests/general_functions/getservbyport_basic.phpt"
                         "ext/standard/tests/general_functions/getservbyport_variation1.phpt"
                         ;; And /etc/protocols.
                         "ext/standard/tests/network/getprotobyname_basic.phpt"
                         "ext/standard/tests/network/getprotobynumber_basic.phpt"
                         ;; And exotic locales.
                         "ext/standard/tests/strings/setlocale_basic1.phpt"
                         "ext/standard/tests/strings/setlocale_basic2.phpt"
                         "ext/standard/tests/strings/setlocale_basic3.phpt"
                         "ext/standard/tests/strings/setlocale_variation1.phpt"
                         ;; This failing test is skipped on PHP's Travis CI as it is
                         ;; supposedly inaccurate.
                         "ext/standard/tests/file/disk_free_space_basic.phpt"
                         ;; The following test erroneously expect the link
                         ;; count of a sub-directory to increase compared to
                         ;; its parent.
                         "ext/standard/tests/file/lstat_stat_variation8.phpt"
                         ;; This tests whether microseconds ‘differ enough’ and
                         ;; fails inconsistently on ‘fast’ machines.
                         "ext/date/tests/bug73837.phpt"

                         ;; XXX: These gd tests fails.  Likely because our version
                         ;; is different from the (patched) bundled one.
                         ;; Here, gd quits immediately after "fatal libpng error"; while the
                         ;; test expects it to additionally return a "setjmp" error and warning.
                         "ext/gd/tests/bug39780_extern.phpt"
                         "ext/gd/tests/libgd00086_extern.phpt"
                         ;; Extra newline in gd-png output.
                         "ext/gd/tests/bug45799.phpt"
                         ;; Test expects generic "gd warning" but gets the actual function name.
                         "ext/gd/tests/createfromwbmp2_extern.phpt"
                         ;; This bug should have been fixed in gd 2.2.2.
                         ;; Is it a regression?
                         "ext/gd/tests/bug65148.phpt"
                         ;; This bug should have been fixed in the gd 2.2
                         ;; series.  Perhaps a regression introduced by gd
                         ;; 2.3.0?
                         "ext/gd/tests/bug66590.phpt"
                         ;; This bug should have been fixed in the php-5.5
                         ;; series.  Perhaps a regression introduced by gd
                         ;; 2.3.0?
                         "ext/gd/tests/bug70102.phpt"
                         ;; This bug should have been fixed in the php-5.6
                         ;; series.  Perhaps a regression introduced by gd
                         ;; 2.3.0?
                         "ext/gd/tests/bug73869.phpt"
                         ;; Some WebP related tests fail.
                         "ext/gd/tests/webp_basic.phpt"
                         "ext/gd/tests/imagecreatefromstring_webp.phpt"
                         ;; Expected error message, but from the wrong function
                         "ext/gd/tests/bug77269.phpt"
                         ;; TODO: Enable these when libgd is built with xpm support.
                         "ext/gd/tests/xpm2gd.phpt"
                         "ext/gd/tests/xpm2jpg.phpt"
                         "ext/gd/tests/xpm2png.phpt"
                         ;; Whitespace difference, probably caused by a very
                         ;; long store path
                         "ext/gd/tests/bug77479.phpt"
                         ;; Expected invalid XBM but got EOF before image was
                         ;; complete.  It's a warning in both cases and test
                         ;; result is the same.
                         "ext/gd/tests/bug77973.phpt"
                         ;; Test expects uninitialized value to be false, but
                         ;; instead gets "resource(5) of type (gd)".
                         "ext/gd/tests/bug79067.phpt"
                         ;; The following test fails with "The image size
                         ;; differs: expected 114x115, got 117x117".
                         "ext/gd/tests/bug79068.phpt"

                         ;; XXX: These iconv tests have the expected outcome,
                         ;; but with different error messages.
                         ;; Expects "illegal character", instead gets "unknown error (84)".
                         "ext/iconv/tests/bug52211.phpt"
                         "ext/iconv/tests/bug60494.phpt"
                         ;; Expects "wrong charset", gets unknown error (22).
                         "ext/iconv/tests/iconv_strlen_error2.phpt"
                         "ext/iconv/tests/iconv_substr_error2.phpt"
                         ;; Expects conversion error, gets "error condition Termsig=11".
                         "ext/iconv/tests/iconv_strpos_error2.phpt"
                         "ext/iconv/tests/iconv_strrpos_error2.phpt"
                         ;; Expects "invalid multibyte sequence" but got
                         ;; "unknown error".
                         "ext/iconv/tests/bug76249.phpt"

                         ;; XXX: These test failures appear legitimate, needs investigation.
                         ;; open_basedir() restriction failure.
                         "ext/curl/tests/bug61948-unix.phpt"
                         ;; Expects a false boolean, gets empty array from glob().
                         "ext/standard/tests/file/bug41655_1.phpt"
                         "ext/standard/tests/file/glob_variation5.phpt"
                         ;; The test expects an Array, but instead get the contents(?).
                         "ext/gd/tests/bug43073.phpt"
                         ;; imagettftext() returns wrong coordinates.
                         "ext/gd/tests/bug48732-mb.phpt"
                         "ext/gd/tests/bug48732.phpt"
                         ;; Similarly for imageftbbox().
                         "ext/gd/tests/bug48801-mb.phpt"
                         "ext/gd/tests/bug48801.phpt"
                         ;; Different expected output from imagecolorallocate().
                         "ext/gd/tests/bug53504.phpt"
                         ;; Wrong image size after scaling an image.
                         "ext/gd/tests/bug73272.phpt"
                         ;; Expects iconv to detect illegal characters, instead gets
                         ;; "unknown error (84)" and heap corruption(!).
                         "ext/iconv/tests/bug48147.phpt"
                         ;; Expects illegal character ".", gets "=?utf-8?Q?."
                         "ext/iconv/tests/bug51250.phpt"
                         ;; iconv throws "buffer length exceeded" on some string checks.
                         "ext/iconv/tests/iconv_mime_encode.phpt"
                         ;; file_get_contents(): iconv stream filter
                         ;; ("ISO-8859-1"=>"UTF-8") unknown error.
                         "ext/standard/tests/file/bug43008.phpt"
                         ;; Table data not created in sqlite(?).
                         "ext/pdo_sqlite/tests/bug_42589.phpt"
                         ;; Expects an Array with 3 preg_matches; gets 0.
                         "ext/pcre/tests/bug79846.phpt"
                         ;; Expects an empty Array; gets one with " " in it.
                         "ext/pcre/tests/bug80118.phpt"
                         ;; Renicing a process fails in the build environment.
                         "ext/standard/tests/general_functions/proc_nice_basic.phpt"
                         ;; Can fail on fast machines?
                         "Zend/tests/bug74093.phpt"))

             ;; Accomodate two extra openssl errors flanking the expected one:
             ;; random number generator:RAND_{load,write}_file:Cannot open file
             ;; This is due to an invalid $HOME, but changing it in the test
             ;; still prints the first one & changing it globally is overkill.
             (substitute* "ext/openssl/tests/bug80747.phpt"
               ((".*error:%s:key size too small.*" match)
                (string-append "%s\n" match "%s\n")))

             ;; Skip tests requiring network access.
             (setenv "SKIP_ONLINE_TESTS" "1")
             ;; Without this variable, 'make test' passes regardless of failures.
             (setenv "REPORT_EXIT_STATUS" "1")
             ;; Skip tests requiring I/O facilities that are unavailable in the
             ;; build environment
             (setenv "SKIP_IO_CAPTURE_TESTS" "1"))))
       #:test-target "test"))
    (inputs
     `(("aspell" ,aspell)
       ("bzip2" ,bzip2)
       ("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("gd" ,gd)
       ("gdbm" ,gdbm)
       ("gmp" ,gmp)
       ("gnutls" ,gnutls)
       ("icu4c" ,icu4c)
       ("libgcrypt" ,libgcrypt)
       ("libpng" ,libpng)
       ("libsodium" ,libsodium)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libx11" ,libx11)
       ("libzip" ,libzip)
       ("oniguruma" ,oniguruma)
       ("openldap" ,openldap)
       ("openssl" ,openssl)
       ("pcre" ,pcre2)
       ("postgresql" ,postgresql)
       ("readline" ,readline)
       ("sqlite" ,sqlite)
       ("tidy" ,tidy)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("gettext" ,gettext-minimal)
       ("procps" ,procps)))             ; for tests
    (synopsis "PHP programming language")
    (description
      "PHP (PHP Hypertext Processor) is a server-side (CGI) scripting
language designed primarily for web development but is also used as
a general-purpose programming language.  PHP code may be embedded into
HTML code, or it can be used in combination with various web template
systems, web content management systems and web frameworks." )
    (license (list
              (license:non-copyleft "file://LICENSE")       ; The PHP license.
              (license:non-copyleft "file://Zend/LICENSE")  ; The Zend license.
              license:lgpl2.1                               ; ext/mbstring/libmbfl
              license:lgpl2.1+                              ; ext/bcmath/libbcmath
              license:bsd-2                                 ; ext/fileinfo/libmagic
              license:expat))))                             ; ext/date/lib
