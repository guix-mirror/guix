#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.68)
AC_INIT([Guix], [0.1], [guile-user@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.11 gnu silent-rules subdir-objects \
 color-tests parallel-tests])

AC_CONFIG_SRCDIR([guix.scm])
AC_CONFIG_MACRO_DIR([m4])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.18.1])

guilemoduledir="${datarootdir}/guile/site/2.0"
AC_SUBST([guilemoduledir])

AC_CANONICAL_HOST

PKG_CHECK_MODULES([GUILE], [guile-2.0])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

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
      ;;
    *)
      LIBGCRYPT="$withval/lib/libgcrypt"
      ;;
   esac],
  [LIBGCRYPT="libgcrypt"])

dnl Library name suitable for `dynamic-link'.
AC_MSG_CHECKING([for libgcrypt shared library name])
AC_MSG_RESULT([$LIBGCRYPT])
AC_SUBST([LIBGCRYPT])

AC_CONFIG_FILES([Makefile
                 po/Makefile.in
		 guix-build
		 guix-download
		 guix-package
		 pre-inst-env])

AC_CONFIG_COMMANDS([commands-exec],
  [chmod +x guix-build guix-download guix-package pre-inst-env])

AC_OUTPUT
