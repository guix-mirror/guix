dnl GNU Guix --- Functional package management for GNU
dnl Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

dnl GUIX_ASSERT_LIBGCRYPT_USABLE
dnl
dnl Assert that GNU libgcrypt is usable from Guile.
AC_DEFUN([GUIX_ASSERT_LIBGCRYPT_USABLE],
  [AC_CACHE_CHECK([whether $LIBGCRYPT can be dynamically loaded],
    [guix_cv_libgcrypt_usable_p],
    [GUILE_CHECK([retval],
      [(dynamic-func \"gcry_md_hash_buffer\" (dynamic-link \"$LIBGCRYPT\"))])
     if test "$retval" = 0; then
       guix_cv_libgcrypt_usable_p="yes"
     else
       guix_cv_libgcrypt_usable_p="no"
     fi])

   if test "x$guix_cv_libgcrypt_usable_p" != "xyes"; then
     AC_MSG_ERROR([GNU libgcrypt does not appear to be usable; see `--with-libgcrypt-prefix' and `README'.])
   fi])

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
       *)
	  machine_name="$host_cpu";;
     esac

     case "$host_os" in
       linux-gnu*)
	  # For backward compatibility, strip the `-gnu' part.
	  guix_system="$machine_name-linux";;
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
    x86_64-linux|i686-linux|mips64el-linux)
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

dnl GUIX_CHECK_SRFI_37
dnl
dnl Check whether SRFI-37 suffers from <http://bugs.gnu.org/13176>.
dnl This bug was fixed in Guile 2.0.9.
AC_DEFUN([GUIX_CHECK_SRFI_37], [
  AC_CACHE_CHECK([whether (srfi srfi-37) is affected by http://bugs.gnu.org/13176],
    [ac_cv_guix_srfi_37_broken],
    [if "$GUILE" -c "(use-modules (srfi srfi-37))			\
       (sigaction SIGALRM (lambda _ (primitive-exit 1)))		\
       (alarm 1)							\
       (define opts (list (option '(#\I) #f #t (lambda _ #t))))		\
       (args-fold '(\"-I\") opts (lambda _ (error)) (lambda _ #f) '())"
     then
       ac_cv_guix_srfi_37_broken=no
     else
       ac_cv_guix_srfi_37_broken=yes
     fi])
])
