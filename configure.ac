#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.68)
AC_INIT([guix], [0.0], [guile-user@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.11 foreign silent-rules subdir-objects \
 color-tests parallel-tests])

AC_CONFIG_SRCDIR([guix.scm])
AC_CONFIG_MACRO_DIR([m4])

guilemoduledir="${datarootdir}/guile/site/2.0"
AC_SUBST([guilemoduledir])

AC_CANONICAL_HOST

PKG_CHECK_MODULES([GUILE], [guile-2.0])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])

AC_ARG_WITH([nix-prefix],
  [AS_HELP_STRING([--with-nix-prefix=DIR], [search for Nix in DIR])],
  [case "$withval" in
    yes|no) ;;
    *)      PATH="$withval/bin:$PATH"; export PATH;;
   esac],
  [])

AC_PATH_PROG([NIX_INSTANTIATE], [nix-instantiate])
AC_PATH_PROG([NIX_HASH], [nix-hash])
if test "x$NIX_INSTANTIATE$NIX_HASH" = "x"; then
   AC_MSG_ERROR([Nix programs not found; please install Nix or use `--with-nix-prefix'.])
fi

AC_ARG_WITH([nixpkgs],
  [AS_HELP_STRING([--with-nixpkgs=DIR], [search for Nixpkgs in DIR])],
  [case "$withval" in
    yes|no) AC_MSG_ERROR([Please use `--with-nixpkgs=DIR'.]);;
    *)      NIXPKGS="$withval";;
   esac],
  [])

if test -f "$NIXPKGS/default.nix"; then
   AC_MSG_CHECKING([for Nixpkgs source tree])
   AC_MSG_RESULT([$NIXPKGS])
   AC_SUBST([NIXPKGS])
else
   AC_MSG_WARN([Nixpkgs not found; this will prevent most tests from running.])
   AC_MSG_WARN([Please use `--with-nixpkgs'.])
fi

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
