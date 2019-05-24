dnl GNU Guix --- Functional package management for GNU
dnl Copyright © 2012, 2013, 2014, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
dnl Copyright © 2014 Mark H Weaver <mhw@netris.org>
dnl Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
dnl
dnl This file is part of GNU Guix.
dnl
dnl GNU Guix is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl GNU Guix is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

dnl GUIX_SYSTEM_TYPE
dnl
dnl Determine the Guix host system type, and store it in the
dnl `guix_system' variable.
AC_DEFUN([GUIX_SYSTEM_TYPE], [
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_PATH_PROG([SED], [sed])

  AC_ARG_WITH(system, AC_HELP_STRING([--with-system=SYSTEM],
    [Platform identifier (e.g., `i686-linux').]),
    [guix_system="$withval"],
    [case "$host_cpu" in
       i*86)
	  machine_name="i686";;
       amd64)
          machine_name="x86_64";;
       arm|armv[[7-9]]*)
          # Here we want to exclude CPUs such as "armv6l".  On ARMv7
          # machines, we normally get "armv7l".  However, in Guix, we
          # configure with --build=arm-unknown-linux-gnueabihf, leading
          # to just "arm", so we also have to allow it.
          #
          # TODO: If not cross-compiling, add a sanity check to make
          #       sure this build machine has the needed features to
          #       support executables compiled using our armhf gcc,
          #       configured with:
          #         --with-arch=armv7-a
          #         --with-float=hard
          #         --with-mode=thumb
          #         --with-fpu=vfpv3-d16
	  machine_name="armhf";;
       *)
	  machine_name="$host_cpu";;
     esac

     case "$host_os" in
       linux-gnu*)
	  # For backward compatibility, strip the `-gnu' part.
	  guix_system="$machine_name-linux";;
       gnu*)
          # Always use i586 for GNU/Hurd.
          guix_system="i586-gnu";;
       *)
	  # Strip the version number from names such as `gnu0.3',
	  # `darwin10.2.0', etc.
	  guix_system="$machine_name-`echo $host_os | "$SED" -e's/[0-9.]*$//g'`";;
     esac])

  AC_MSG_CHECKING([for the Guix system type])
  AC_MSG_RESULT([$guix_system])

  AC_SUBST([guix_system])
])

dnl GUIX_ASSERT_SUPPORTED_SYSTEM
dnl
dnl Assert that this is a system to which the distro is ported.
AC_DEFUN([GUIX_ASSERT_SUPPORTED_SYSTEM], [
  AC_REQUIRE([GUIX_SYSTEM_TYPE])

  AC_ARG_WITH([courage], [AC_HELP_STRING([--with-courage],
    [Assert that even if this platform is unsupported, you will be
courageous and port the GNU System distribution to it (see
"GNU Distribution" in the manual.)])],
    [guix_courageous="$withval"],
    [guix_courageous="no"])

  # Currently only Linux-based systems are supported, and only on some
  # platforms.
  case "$guix_system" in
    x86_64-linux|i686-linux|armhf-linux|aarch64-linux|mips64el-linux)
      ;;
    *)
      if test "x$guix_courageous" = "xyes"; then
        AC_MSG_WARN([building Guix on `$guix_system', which is not supported])
      else
        AC_MSG_ERROR([`$guix_system' is not a supported platform.
See "GNU Distribution" in the manual, or try `--with-courage'.])
      fi
      ;;
  esac
])

dnl GUIX_ASSERT_GUILE_FEATURES FEATURES
dnl
dnl Assert that FEATURES are provided by $GUILE.
AC_DEFUN([GUIX_ASSERT_GUILE_FEATURES], [
  for guix_guile_feature in $1
  do
    AC_MSG_CHECKING([whether $GUILE provides feature '$guix_guile_feature'])
    if "$GUILE" -c "(exit (provided? '$guix_guile_feature))"
    then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([$GUILE does not support feature '$guix_guile_feature', which is required.])
    fi
  done
])

dnl GUIX_ASSERT_SYNTAX_OBJECT_EQUAL
dnl
dnl Guile 2.2.1 was a brown-paper-bag release where 'equal?' wouldn't work
dnl for syntax objects, which broke gexps.  Unfortunately Fedora 25 provides it.
dnl Reject it.
AC_DEFUN([GUIX_ASSERT_SYNTAX_OBJECT_EQUAL], [
  AC_CACHE_CHECK([whether 'equal?' works for syntax objects],
    [ac_cv_guix_syntax_object_equal],
    [if "$GUILE" -c '(exit (equal? (syntax x) (syntax x)))'
     then
       ac_cv_guix_syntax_object_equal=yes
     else
       ac_cv_guix_syntax_object_equal=no
     fi])
  if test "x$ac_cv_guix_syntax_object_equal" != xyes; then
    # This bug was present in Guile 2.2.1 only.
    AC_MSG_ERROR(['equal?' does not work for syntax object; upgrade to Guile 2.2.2 or later.])
  fi
])

dnl GUIX_CHECK_GUILE_SSH
dnl
dnl Check whether a recent-enough Guile-SSH is available.
AC_DEFUN([GUIX_CHECK_GUILE_SSH], [
  dnl Check whether 'channel-send-eof' (introduced in 0.10.2) is present.
  AC_CACHE_CHECK([whether Guile-SSH is available and recent enough],
    [guix_cv_have_recent_guile_ssh],
    [GUILE_CHECK([retval],
      [(and (@ (ssh channel) channel-send-eof)
            (@ (ssh popen) open-remote-pipe)
	    (@ (ssh dist node) node-eval))])
     if test "$retval" = 0; then
       guix_cv_have_recent_guile_ssh="yes"
     else
       guix_cv_have_recent_guile_ssh="no"
     fi])
])

dnl GUIX_CHECK_GUILE_SQLITE3
dnl
dnl Check whether a recent-enough Guile-Sqlite3 is available.
AC_DEFUN([GUIX_CHECK_GUILE_SQLITE3], [
  dnl Check whether 'sqlite-bind-arguments' is available.  It was introduced
  dnl in February 2018:
  dnl <https://notabug.org/guile-sqlite3/guile-sqlite3/commit/1cd1dec96a9999db48c0ff45bab907efc637247f>.
  AC_CACHE_CHECK([whether Guile-Sqlite3 is available and recent enough],
    [guix_cv_have_recent_guile_sqlite3],
    [GUILE_CHECK([retval],
      [(@ (sqlite3) sqlite-bind-arguments)])
     if test "$retval" = 0; then
       guix_cv_have_recent_guile_sqlite3="yes"
     else
       guix_cv_have_recent_guile_sqlite3="no"
     fi])
])

dnl GUIX_TEST_ROOT_DIRECTORY
AC_DEFUN([GUIX_TEST_ROOT_DIRECTORY], [
  AC_CACHE_CHECK([for unit test root directory],
    [ac_cv_guix_test_root],
    [ac_cv_guix_test_root="`pwd`/test-tmp"])
])

dnl 'BINPRM_BUF_SIZE' constant in Linux (we leave room for the trailing zero.)
dnl The Hurd has a limit of about a page (see exec/hashexec.c.)
m4_define([LINUX_HASH_BANG_LIMIT], 127)

dnl Hardcoded 'sun_path' length in <sys/un.h>.
m4_define([SOCKET_FILE_NAME_LIMIT], 108)

dnl GUIX_SOCKET_FILE_NAME_LENGTH
AC_DEFUN([GUIX_SOCKET_FILE_NAME_LENGTH], [
  AC_CACHE_CHECK([the length of the installed socket file name],
    [ac_cv_guix_socket_file_name_length],
    [ac_cv_guix_socket_file_name_length="`echo -n "$guix_localstatedir/guix/daemon-socket/socket" | wc -c`"])
])

dnl GUIX_TEST_SOCKET_FILE_NAME_LENGTH
AC_DEFUN([GUIX_TEST_SOCKET_FILE_NAME_LENGTH], [
  AC_REQUIRE([GUIX_TEST_ROOT_DIRECTORY])
  AC_CACHE_CHECK([the length of the socket file name used in tests],
    [ac_cv_guix_test_socket_file_name_length],
    [ac_cv_guix_test_socket_file_name_length="`echo -n "$ac_cv_guix_test_root/var/123456/daemon-socket/socket" | wc -c`"])
])

dnl GUIX_HASH_BANG_LENGTH
AC_DEFUN([GUIX_HASH_BANG_LENGTH], [
  AC_CACHE_CHECK([the length of a typical hash bang line],
    [ac_cv_guix_hash_bang_length],
    [ac_cv_guix_hash_bang_length=`echo -n "$storedir/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-bootstrap-binaries-0/bin/bash" | wc -c`])
])

dnl GUIX_TEST_HASH_BANG_LENGTH
AC_DEFUN([GUIX_TEST_HASH_BANG_LENGTH], [
  AC_REQUIRE([GUIX_TEST_ROOT_DIRECTORY])
  AC_CACHE_CHECK([the length of a hash bang line used in tests],
    [ac_cv_guix_test_hash_bang_length],
    [ac_cv_guix_test_hash_bang_length=`echo -n "$ac_cv_guix_test_root/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-bootstrap-binaries-0/bin/bash" | wc -c`])
])

dnl GUIX_CHECK_FILE_NAME_LIMITS
dnl
dnl GNU/Linux has a couple of silly limits that we can easily run into.
dnl Make sure everything is fine with the current settings.  Set $1 to
dnl 'yes' if tests can run, 'no' otherwise.
AC_DEFUN([GUIX_CHECK_FILE_NAME_LIMITS], [
  AC_REQUIRE([GUIX_SOCKET_FILE_NAME_LENGTH])
  AC_REQUIRE([GUIX_TEST_SOCKET_FILE_NAME_LENGTH])
  AC_REQUIRE([GUIX_HASH_BANG_LENGTH])
  AC_REQUIRE([GUIX_TEST_HASH_BANG_LENGTH])

  if test "$ac_cv_guix_socket_file_name_length" -ge ]SOCKET_FILE_NAME_LIMIT[; then
    AC_MSG_ERROR([socket file name would exceed the maxium allowed length])
  fi
  if test "$ac_cv_guix_test_socket_file_name_length" -ge ]SOCKET_FILE_NAME_LIMIT[; then
    AC_MSG_WARN([socket file name limit may be exceeded when running tests])
  fi

  $1=yes
  if test "$ac_cv_guix_hash_bang_length" -ge ]LINUX_HASH_BANG_LIMIT[; then
    $1=no
    AC_MSG_ERROR([store directory '$storedir' would lead to overly long hash-bang lines])
  fi
  if test "$ac_cv_guix_test_hash_bang_length" -ge ]LINUX_HASH_BANG_LIMIT[; then
    $1=no
    AC_MSG_WARN([test directory '$ac_cv_guix_test_root' may lead to overly long hash-bang lines])
  fi
])

dnl GUIX_CHECK_CXX11
dnl
dnl Check whether the C++ compiler can compile a typical C++11 program.
AC_DEFUN([GUIX_CHECK_CXX11], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_CACHE_CHECK([whether $CXX supports C++11],
    [ac_cv_guix_cxx11_support],
    [save_CXXFLAGS="$CXXFLAGS"
     CXXFLAGS="-std=c++11 $CXXFLAGS"
     AC_COMPILE_IFELSE([
      AC_LANG_SOURCE([
	#include <functional>

	std::function<int(int)>
	return_plus_lambda (int x)
	{
	  auto result = [[&]](int y) {
	    return x + y;
	  };

	  return result;
	}
      ])],
      [ac_cv_guix_cxx11_support=yes],
      [ac_cv_guix_cxx11_support=no])
    CXXFLAGS="$save_CXXFLAGS"
  ])
])

dnl GUIX_ASSERT_CXX11
dnl
dnl Error out if the C++ compiler cannot compile C++11 code.
AC_DEFUN([GUIX_ASSERT_CXX11], [
  GUIX_CHECK_CXX11
  if test "x$ac_cv_guix_cxx11_support" != "xyes"; then
    AC_MSG_ERROR([C++ compiler '$CXX' does not support the C++11 standard])
  fi
])

dnl GUIX_LIBGCRYPT_LIBDIR VAR
dnl
dnl Attempt to determine libgcrypt's LIBDIR; store the result in VAR.
AC_DEFUN([GUIX_LIBGCRYPT_LIBDIR], [
  AC_PATH_PROG([LIBGCRYPT_CONFIG], [libgcrypt-config])
  AC_CACHE_CHECK([libgcrypt's library directory],
    [guix_cv_libgcrypt_libdir],
    [if test "x$LIBGCRYPT_CONFIG" != "x"; then
       guix_cv_libgcrypt_libdir=`$LIBGCRYPT_CONFIG --libs | grep -e -L | sed -e "s/.*-L\([[^ ]]\+\)[[[:blank:]]]\+-lgcrypt.*/\1/g"`
     else
       guix_cv_libgcrypt_libdir=""
     fi])
  $1="$guix_cv_libgcrypt_libdir"
])

dnl GUIX_LIBZ_LIBDIR VAR
dnl
dnl Attempt to determine libz's LIBDIR; store the result in VAR.
AC_DEFUN([GUIX_LIBZ_LIBDIR], [
  AC_REQUIRE([PKG_PROG_PKG_CONFIG])
  AC_CACHE_CHECK([zlib's library directory],
    [guix_cv_libz_libdir],
    [guix_cv_libz_libdir="`$PKG_CONFIG zlib --variable=libdir 2> /dev/null`"])
  $1="$guix_cv_libz_libdir"
])

dnl GUIX_LIBLZ_FILE_NAME VAR
dnl
dnl Attempt to determine liblz's absolute file name; store the result in VAR.
AC_DEFUN([GUIX_LIBLZ_FILE_NAME], [
  AC_REQUIRE([PKG_PROG_PKG_CONFIG])
  AC_CACHE_CHECK([lzlib's file name],
    [guix_cv_liblz_libdir],
    [old_LIBS="$LIBS"
     LIBS="-llz"
     AC_LINK_IFELSE([AC_LANG_SOURCE([int main () { return LZ_decompress_open(); }])],
       [guix_cv_liblz_libdir="`ldd conftest$EXEEXT | grep liblz | sed '-es/.*=> \(.*\) .*$/\1/g'`"])
     LIBS="$old_LIBS"])
  $1="$guix_cv_liblz_libdir"
])

dnl GUIX_CURRENT_LOCALSTATEDIR
dnl
dnl Determine the localstatedir of an existing Guix installation and set
dnl 'guix_cv_current_localstatedir' accordingly.  Set it to "none" if no
dnl existing installation was found.
AC_DEFUN([GUIX_CURRENT_LOCALSTATEDIR], [
  AC_PATH_PROG([GUILE], [guile])
  AC_CACHE_CHECK([the current installation's localstatedir],
    [guix_cv_current_localstatedir],
    [dnl Call 'dirname' because (guix config) appends "/guix" to LOCALSTATEDIR.
     guix_cv_current_localstatedir="`"$GUILE" \
       -c '(use-modules (guix config))
           (when (string=? %store-directory "'$storedir'")
             (display (dirname %state-directory)))' \
       2>/dev/null`"
     if test "x$guix_cv_current_localstatedir" = "x"; then
       guix_cv_current_localstatedir=none
     fi])])

dnl GUIX_CHECK_LOCALSTATEDIR
dnl
dnl Check that the LOCALSTATEDIR value is consistent with that of the existing
dnl Guix installation, if any.  Error out or warn if they do not match.
AC_DEFUN([GUIX_CHECK_LOCALSTATEDIR], [
  AC_REQUIRE([GUIX_CURRENT_LOCALSTATEDIR])
  if test "x$guix_cv_current_localstatedir" != "xnone"; then
    if test "$guix_cv_current_localstatedir" != "$guix_localstatedir"; then
      case "$localstatedir" in
        NONE|\${prefix}*)
          # User kept the default value---i.e., did not pass '--localstatedir'.
          AC_MSG_ERROR([chosen localstatedir '$guix_localstatedir' does not match \
that of the existing installation '$guix_cv_current_localstatedir'
Installing may corrupt $storedir!
Use './configure --localstatedir=$guix_cv_current_localstatedir'.])
          ;;
        *)
          # User passed an explicit '--localstatedir'.  Assume they know what
          # they're doing.
          AC_MSG_WARN([chosen localstatedir '$guix_localstatedir' does not match \
that of the existing installation '$guix_cv_current_localstatedir'])
          AC_MSG_WARN([installing may corrupt $storedir!])
         ;;
      esac
    fi
  fi])
