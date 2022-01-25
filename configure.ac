#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([GNU Guix],
  [m4_esyscmd([build-aux/git-version-gen .tarball-version])],
  [bug-guix@gnu.org], [guix],
  [https://www.gnu.org/software/guix/])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

# Enable silent rules by default.
AM_SILENT_RULES([yes])

AC_CONFIG_SRCDIR([guix.scm])
AC_CONFIG_MACRO_DIR([m4])

dnl For the C++ code.  This must be used early.
AC_USE_SYSTEM_EXTENSIONS

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.19.1])

GUIX_SYSTEM_TYPE
GUIX_ASSERT_SUPPORTED_SYSTEM
GUIX_CHANNEL_METADATA

AM_CONDITIONAL([CROSS_COMPILING], [test "x$cross_compiling" = "xyes"])

AC_ARG_WITH(store-dir,
  AS_HELP_STRING([--with-store-dir=PATH],
    [file name of the store (defaults to /gnu/store)]),
  [storedir="$withval"],
  [storedir="/gnu/store"])
AC_SUBST(storedir)

AC_ARG_WITH([bash-completion-dir],
  AS_HELP_STRING([--with-bash-completion-dir=DIR],
    [name of the Bash completion directory]),
  [bashcompletiondir="$withval"],
  [bashcompletiondir='${sysconfdir}/bash_completion.d'])
AC_SUBST([bashcompletiondir])

AC_ARG_WITH([zsh-completion-dir],
  AS_HELP_STRING([--with-zsh-completion-dir=DIR],
    [name of the Zsh completion directory]),
  [zshcompletiondir="$withval"],
  [zshcompletiondir='${datadir}/zsh/site-functions'])
AC_SUBST([zshcompletiondir])

AC_ARG_WITH([fish-completion-dir],
  AS_HELP_STRING([--with-fish-completion-dir=DIR],
    [name of the Fish completion directory]),
  [fishcompletiondir="$withval"],
  [fishcompletiondir='${datadir}/fish/vendor_completions.d'])
AC_SUBST([fishcompletiondir])

AC_ARG_WITH([selinux-policy-dir],
  AS_HELP_STRING([--with-selinux-policy-dir=DIR],
    [name of the SELinux policy directory]),
  [selinux_policydir="$withval"],
  [selinux_policydir='${datadir}/selinux/'])
AC_SUBST([selinux_policydir])

dnl Better be verbose.
AC_MSG_CHECKING([for the store directory])
AC_MSG_RESULT([$storedir])

AC_ARG_ENABLE([daemon],
  [AS_HELP_STRING([--disable-daemon], [do not build the Nix daemon (C++)])],
  [guix_build_daemon="$enableval"],
  [guix_build_daemon="yes"])

# Prepare a version of $localstatedir & co. that does not contain references
# to shell variables.
guix_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
guix_localstatedir="`eval echo $localstatedir | sed -e "s|NONE|$guix_prefix|g"`"
guix_sysconfdir="`eval echo $sysconfdir | sed -e "s|NONE|$guix_prefix|g"`"
guix_sbindir="`eval echo $sbindir | sed -e "s|NONE|$guix_prefix|g"`"
AC_SUBST([guix_localstatedir])
AC_SUBST([guix_sysconfdir])
AC_SUBST([guix_sbindir])

GUIX_CHECK_FILE_NAME_LIMITS([can_run_tests])
AM_CONDITIONAL([CAN_RUN_TESTS], [test "x$can_run_tests" = "xyes"])

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([GUILE_MODULE_AVAILABLE])
m4_pattern_forbid([^GUILE_P])
m4_pattern_allow([^GUILE_PKG_ERRORS])
m4_pattern_forbid([^GUIX_])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
fi

dnl (guix ui), notably, requires 'default-optimization-level' added in 3.0.3.
PKG_CHECK_MODULES([GUILE], [guile-3.0 >= 3.0.3])

dnl Get CFLAGS and LDFLAGS for libguile.
GUILE_FLAGS

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

dnl The GnuTLS bindings are necessary for substitutes over HTTPS and for 'guix
dnl pull', among other things.
GUILE_MODULE_AVAILABLE([have_gnutls], [(gnutls)])
if test "x$have_gnutls" != "xyes"; then
  AC_MSG_ERROR([The Guile bindings of GnuTLS are missing; please install them.])
fi

dnl Check for Guile-Git.
GUILE_MODULE_AVAILABLE([have_guile_git], [(git)])
if test "x$have_guile_git" != "xyes"; then
  AC_MSG_ERROR([Guile-Git is missing; please install it.])
fi

dnl Check for Guile-JSON.
GUIX_CHECK_GUILE_JSON
if test "x$guix_cv_have_recent_guile_json" != "xyes"; then
  AC_MSG_ERROR([Guile-JSON is missing; please install it.])
fi

dnl Guile-Sqlite3 is used by the (guix store ...) modules.
GUIX_CHECK_GUILE_SQLITE3
if test "x$guix_cv_have_recent_guile_sqlite3" != "xyes"; then
  AC_MSG_ERROR([A recent Guile-SQLite3 could not be found; please install it.])
fi

GUIX_CHECK_GUILE_GCRYPT
if test "x$guix_cv_have_recent_guile_gcrypt" != "xyes"; then
  AC_MSG_ERROR([A recent Guile-Gcrypt could not be found; please install it.])
fi

GUIX_CHECK_GUILE_GIT
if test "x$guix_cv_have_recent_guile_git" != "xyes"; then
  AC_MSG_ERROR([A recent Guile-Git could not be found; please install it.])
fi

dnl Check for the optional Guile-Lib.
GUILE_MODULE_EXPORTS([have_guile_lib], [(htmlprag)], [%strict-tokenizer?])
AM_CONDITIONAL([HAVE_GUILE_LIB], [test "x$have_guile_lib" = "xyes"])
AM_COND_IF(HAVE_GUILE_LIB,,
  [AC_MSG_WARN([The Guile-Lib requirement was not satisfied (>= 0.2.7);
Some features such as the Go importer will not be usable.])])

dnl Check for Guile-zlib.
GUIX_CHECK_GUILE_ZLIB
if test "x$guix_cv_have_recent_guile_zlib" != "xyes"; then
  AC_MSG_ERROR([A recent Guile-zlib could not be found; please install it.])
fi

dnl Check for Guile-lzlib.
GUILE_MODULE_AVAILABLE([have_guile_lzlib], [(lzlib)])
if test "x$have_guile_lzlib" != "xyes"; then
  AC_MSG_ERROR([Guile-lzlib is missing; please install it.])
fi

dnl Check for Guile-Avahi.
GUILE_MODULE_AVAILABLE([have_guile_avahi], [(avahi)])
AM_CONDITIONAL([HAVE_GUILE_AVAHI],
  [test "x$have_guile_avahi" = "xyes"])

dnl Guile-newt is used by the graphical installer.
GUILE_MODULE_AVAILABLE([have_guile_newt], [(newt)])

AC_ARG_ENABLE([installer],
    AS_HELP_STRING([--enable-installer], [Build the graphical installer sources.]))

AS_IF([test "x$enable_installer" = "xyes"], [
if test "x$have_guile_newt" != "xyes"; then
  AC_MSG_ERROR([Guile-newt could not be found; please install it.])
fi
])

AM_CONDITIONAL([ENABLE_INSTALLER],
  [test "x$enable_installer" = "xyes"])

dnl Make sure we have a full-fledged Guile.
GUIX_ASSERT_GUILE_FEATURES([regex posix socket net-db threads])

AC_PROG_SED

dnl Decompressors, for use by the substituter and other modules.
AC_PATH_PROG([GZIP], [gzip])
AC_PATH_PROG([BZIP2], [bzip2])
AC_PATH_PROG([XZ], [xz])
AC_SUBST([GZIP])
AC_SUBST([BZIP2])
AC_SUBST([XZ])

LIBGCRYPT_LIBDIR="no"
LIBGCRYPT_PREFIX="no"

AC_ARG_WITH([libgcrypt-prefix],
  [AS_HELP_STRING([--with-libgcrypt-prefix=DIR], [search for GNU libgcrypt in DIR])],
  [case "$withval" in
    yes|no)
      ;;
    *)
      LIBGCRYPT_PREFIX="$withval"
      LIBGCRYPT_LIBDIR="$withval/lib"
      ;;
   esac])

AC_ARG_WITH([libgcrypt-libdir],
  [AS_HELP_STRING([--with-libgcrypt-libdir=DIR],
     [search for GNU libgcrypt's shared library in DIR])],
  [case "$withval" in
    yes|no)
      LIBGCRYPT_LIBDIR="no"
      ;;
    *)
      LIBGCRYPT_LIBDIR="$withval"
      ;;
   esac])

dnl If none of the --with-libgcrypt-* options was used, try to determine the
dnl the library directory.
case "x$LIBGCRYPT_PREFIX$LIBGCRYPT_LIBDIR" in
  xnono)
    GUIX_LIBGCRYPT_LIBDIR([LIBGCRYPT_LIBDIR])
    ;;
esac

AC_SUBST([LIBGCRYPT_PREFIX])
AC_SUBST([LIBGCRYPT_LIBDIR])

dnl Check for Guile-SSH, for the (guix ssh) module.
GUIX_CHECK_GUILE_SSH
AM_CONDITIONAL([HAVE_GUILE_SSH],
  [test "x$guix_cv_have_recent_guile_ssh" = "xyes"])

AC_CACHE_SAVE

m4_include([config-daemon.ac])

dnl `dot' (from the Graphviz package) is only needed for maintainers.
dnl See `Building from Git' in the manual for more info.
AM_MISSING_PROG([DOT], [dot])

dnl Manual pages.
AM_MISSING_PROG([HELP2MAN], [help2man])

dnl Documentation translation.
AM_MISSING_PROG([PO4A_TRANSLATE], [po4a-translate])
AM_MISSING_PROG([PO4A_UPDATEPO], [po4a-updatepo])

case "$storedir" in
  /gnu/store)
    ;;
  *)
    AC_MSG_WARN([Using a store directory other than '/gnu/store' will prevent you])
    AC_MSG_WARN([from downloading substitutes from gnu.org.])
    ;;
esac

AC_CONFIG_FILES([Makefile
                 po/guix/Makefile.in
                 po/packages/Makefile.in
                 etc/guix-daemon.cil
                 guix/config.scm])

AC_CONFIG_FILES([etc/committer.scm], [chmod +x etc/committer.scm])
AC_CONFIG_FILES([test-env:build-aux/test-env.in], [chmod +x test-env])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])

AC_OUTPUT
