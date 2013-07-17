#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.68)
AC_INIT([GNU Guix], [0.3], [bug-guix@gnu.org], [guix],
  [http://www.gnu.org/software/guix/])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects \
 color-tests parallel-tests])

AC_CONFIG_SRCDIR([guix.scm])
AC_CONFIG_MACRO_DIR([m4])

dnl For the C++ code.  This must be used early.
AC_USE_SYSTEM_EXTENSIONS

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.18.1])

guilemoduledir="${datarootdir}/guile/site/2.0"
AC_SUBST([guilemoduledir])

GUIX_SYSTEM_TYPE
GUIX_ASSERT_SUPPORTED_SYSTEM

AC_ARG_WITH(store-dir,
  AC_HELP_STRING([--with-store-dir=PATH],
    [path of the store (defaults to /nix/store)]),
  [storedir="$withval"],
  [storedir="/nix/store"])
AC_SUBST(storedir)

AC_ARG_ENABLE([daemon],
  [AS_HELP_STRING([--disable-daemon], [build the Nix daemon (C++)])],
  [guix_build_daemon="$enableval"],
  [guix_build_daemon="yes"])

# Prepare a version of $localstatedir that does not contain references
# to shell variables.
guix_localstatedir="`eval echo $localstatedir | sed -e "s|NONE|/usr/local|g"`"
AC_SUBST([guix_localstatedir])

PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.5])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl Make sure we have a full-fledged Guile.
GUIX_ASSERT_GUILE_FEATURES([regex posix socket net-db threads])

dnl Check whether (srfi srfi-37) works, and provide our own if it doesn't.
GUIX_CHECK_SRFI_37
AM_CONDITIONAL([INSTALL_SRFI_37], [test "x$ac_cv_guix_srfi_37_broken" = xyes])

AC_ARG_WITH([nix-prefix],
  [AS_HELP_STRING([--with-nix-prefix=DIR], [search for Nix in DIR])],
  [case "$withval" in
    yes|no) ;;
    *)
      NIX_PREFIX="$withval"
      PATH="$NIX_PREFIX/bin:$PATH"; export PATH
      AC_SUBST([NIX_PREFIX])
      ;;
   esac],
  [])

AC_PATH_PROG([NIX_INSTANTIATE], [nix-instantiate])
AC_PATH_PROG([NIX_HASH], [nix-hash])
if test "x$guix_build_daemon$NIX_INSTANTIATE$NIX_HASH" = "xno"; then
   AC_MSG_ERROR([Nix programs not found; please install Nix or use `--with-nix-prefix'.])
fi

if test "x$NIX_INSTANTIATE" = "x"; then
   # This program is an optional dependency, so we just want it to be
   # taken from $PATH if it's not available right now.
   NIX_INSTANTIATE="nix-instantiate"
fi

AC_ARG_WITH([nixpkgs],
  [AS_HELP_STRING([--with-nixpkgs=DIR], [search for Nixpkgs in DIR])],
  [case "$withval" in
    yes|no) AC_MSG_ERROR([Please use `--with-nixpkgs=DIR'.]);;
    *)      NIXPKGS="$withval";;
   esac],
  [])

AC_MSG_CHECKING([for Nixpkgs source tree])
if test -f "$NIXPKGS/default.nix"; then
   AC_MSG_RESULT([$NIXPKGS])
   AC_SUBST([NIXPKGS])
else
   AC_MSG_RESULT([not found])
fi

AC_ARG_WITH([libgcrypt-prefix],
  [AS_HELP_STRING([--with-libgcrypt-prefix=DIR], [search for GNU libgcrypt in DIR])],
  [case "$withval" in
    yes|no)
      LIBGCRYPT="libgcrypt"
      LIBGCRYPT_PREFIX="no"
      ;;
    *)
      LIBGCRYPT="$withval/lib/libgcrypt"
      LIBGCRYPT_PREFIX="$withval"
      ;;
   esac],
  [LIBGCRYPT="libgcrypt"])

dnl Library name suitable for `dynamic-link'.
AC_MSG_CHECKING([for libgcrypt shared library name])
AC_MSG_RESULT([$LIBGCRYPT])
AC_SUBST([LIBGCRYPT])
AC_SUBST([LIBGCRYPT_PREFIX])

GUIX_ASSERT_LIBGCRYPT_USABLE

AC_CACHE_SAVE

m4_include([config-daemon.ac])

AC_CONFIG_FILES([Makefile
                 po/Makefile.in
		 guix/config.scm])

AC_CONFIG_FILES([scripts/guix], [chmod +x scripts/guix])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env],     [chmod +x test-env])

AC_OUTPUT
