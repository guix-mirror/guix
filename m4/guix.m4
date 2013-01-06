dnl GNU Guix --- Functional package management for GNU
dnl Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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
	  guix_system="$machine_name-`echo $host_os | "$SED" -e's/@<:@0-9.@:>@*$//g'`";;
     esac])
  AC_SUBST([guix_system])
])
