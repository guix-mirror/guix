# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2013 Andreas Enge <andreas@enge.fr>
#
# This file is part of GNU Guix.
#
# GNU Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

bin_SCRIPTS =					\
  guix-build					\
  guix-download					\
  guix-import					\
  guix-package					\
  guix-gc

MODULES =					\
  guix/base32.scm				\
  guix/utils.scm				\
  guix/derivations.scm				\
  guix/download.scm				\
  guix/gnu-maintenance.scm			\
  guix/licenses.scm				\
  guix/build-system.scm				\
  guix/build-system/gnu.scm			\
  guix/build-system/trivial.scm			\
  guix/ftp-client.scm				\
  guix/store.scm				\
  guix/ui.scm					\
  guix/build/download.scm			\
  guix/build/gnu-build-system.scm		\
  guix/build/utils.scm				\
  guix/build/union.scm				\
  guix/packages.scm				\
  guix/snix.scm					\
  guix.scm					\
  gnu/packages.scm				\
  gnu/packages/acl.scm				\
  gnu/packages/algebra.scm			\
  gnu/packages/aspell.scm			\
  gnu/packages/attr.scm				\
  gnu/packages/autotools.scm			\
  gnu/packages/base.scm				\
  gnu/packages/bash.scm				\
  gnu/packages/bdb.scm				\
  gnu/packages/bdw-gc.scm			\
  gnu/packages/bison.scm			\
  gnu/packages/bootstrap.scm			\
  gnu/packages/check.scm			\
  gnu/packages/compression.scm			\
  gnu/packages/cpio.scm				\
  gnu/packages/cyrus-sasl.scm			\
  gnu/packages/dejagnu.scm			\
  gnu/packages/ddrescue.scm			\
  gnu/packages/ed.scm				\
  gnu/packages/emacs.scm			\
  gnu/packages/file.scm				\
  gnu/packages/flex.scm				\
  gnu/packages/freetype.scm			\
  gnu/packages/gawk.scm				\
  gnu/packages/gdb.scm				\
  gnu/packages/gdbm.scm				\
  gnu/packages/gettext.scm			\
  gnu/packages/ghostscript.scm			\
  gnu/packages/glib.scm				\
  gnu/packages/global.scm			\
  gnu/packages/gnupg.scm			\
  gnu/packages/gnutls.scm			\
  gnu/packages/gperf.scm			\
  gnu/packages/gsasl.scm			\
  gnu/packages/guile.scm			\
  gnu/packages/help2man.scm			\
  gnu/packages/icu4c.scm			\
  gnu/packages/idutils.scm			\
  gnu/packages/indent.scm			\
  gnu/packages/ld-wrapper.scm			\
  gnu/packages/less.scm				\
  gnu/packages/libevent.scm			\
  gnu/packages/libffi.scm			\
  gnu/packages/libidn.scm			\
  gnu/packages/libjpeg.scm			\
  gnu/packages/libpng.scm			\
  gnu/packages/libsigsegv.scm			\
  gnu/packages/libtiff.scm			\
  gnu/packages/libunistring.scm			\
  gnu/packages/libusb.scm			\
  gnu/packages/linux.scm			\
  gnu/packages/lout.scm				\
  gnu/packages/lsh.scm				\
  gnu/packages/m4.scm				\
  gnu/packages/mailutils.scm			\
  gnu/packages/make-bootstrap.scm		\
  gnu/packages/mit-krb5.scm			\
  gnu/packages/multiprecision.scm		\
  gnu/packages/mysql.scm			\
  gnu/packages/nano.scm				\
  gnu/packages/ncurses.scm			\
  gnu/packages/nettle.scm			\
  gnu/packages/openssl.scm			\
  gnu/packages/oggvorbis.scm			\
  gnu/packages/perl.scm				\
  gnu/packages/pkg-config.scm			\
  gnu/packages/pth.scm				\
  gnu/packages/python.scm			\
  gnu/packages/readline.scm			\
  gnu/packages/recutils.scm			\
  gnu/packages/rsync.scm			\
  gnu/packages/scheme.scm			\
  gnu/packages/screen.scm 			\
  gnu/packages/shishi.scm			\
  gnu/packages/system.scm			\
  gnu/packages/tcl.scm				\
  gnu/packages/texinfo.scm			\
  gnu/packages/time.scm				\
  gnu/packages/tor.scm				\
  gnu/packages/wget.scm				\
  gnu/packages/which.scm			\
  gnu/packages/xml.scm				\
  gnu/packages/zile.scm


GOBJECTS = $(MODULES:%.scm=%.go) guix/config.go

nobase_dist_guilemodule_DATA = $(MODULES)

patchdir = $(guilemoduledir)/gnu/packages/patches
dist_patch_DATA =					\
  gnu/packages/patches/bigloo-gc-shebangs.patch		\
  gnu/packages/patches/binutils-ld-new-dtags.patch	\
  gnu/packages/patches/cpio-gets-undeclared.patch	\
  gnu/packages/patches/diffutils-gets-undeclared.patch	\
  gnu/packages/patches/emacs-configure-sh.patch		\
  gnu/packages/patches/flex-bison-tests.patch		\
  gnu/packages/patches/gawk-shell.patch			\
  gnu/packages/patches/gettext-gets-undeclared.patch	\
  gnu/packages/patches/glib-tests-desktop.patch		\
  gnu/packages/patches/glib-tests-homedir.patch		\
  gnu/packages/patches/glib-tests-timezone.patch	\
  gnu/packages/patches/glibc-bootstrap-system.patch	\
  gnu/packages/patches/glibc-no-ld-so-cache.patch	\
  gnu/packages/patches/guile-1.8-cpp-4.5.patch		\
  gnu/packages/patches/guile-default-utf8.patch		\
  gnu/packages/patches/guile-relocatable.patch		\
  gnu/packages/patches/libtool-skip-tests.patch		\
  gnu/packages/patches/lsh-guile-compat.patch		\
  gnu/packages/patches/lsh-no-root-login.patch		\
  gnu/packages/patches/lsh-pam-service-name.patch	\
  gnu/packages/patches/m4-gets-undeclared.patch		\
  gnu/packages/patches/m4-readlink-EINVAL.patch		\
  gnu/packages/patches/m4-s_isdir.patch			\
  gnu/packages/patches/make-impure-dirs.patch		\
  gnu/packages/patches/findutils-absolute-paths.patch	\
  gnu/packages/patches/perl-no-sys-dirs.patch		\
  gnu/packages/patches/procps-make-3.82.patch		\
  gnu/packages/patches/readline-link-ncurses.patch	\
  gnu/packages/patches/shishi-gets-undeclared.patch	\
  gnu/packages/patches/tar-gets-undeclared.patch

bootstrapdir = $(guilemoduledir)/gnu/packages/bootstrap
bootstrap_x86_64_linuxdir = $(bootstrapdir)/x86_64-linux
bootstrap_i686_linuxdir = $(bootstrapdir)/i686-linux

dist_bootstrap_x86_64_linux_DATA =		\
  gnu/packages/bootstrap/x86_64-linux/bash	\
  gnu/packages/bootstrap/x86_64-linux/mkdir	\
  gnu/packages/bootstrap/x86_64-linux/tar	\
  gnu/packages/bootstrap/x86_64-linux/xz

dist_bootstrap_i686_linux_DATA =		\
  gnu/packages/bootstrap/i686-linux/bash	\
  gnu/packages/bootstrap/i686-linux/mkdir	\
  gnu/packages/bootstrap/i686-linux/tar	\
  gnu/packages/bootstrap/i686-linux/xz

# Big bootstrap binaries are not included in the tarball.  Instead, they
# are downloaded.
nodist_bootstrap_x86_64_linux_DATA =					\
  gnu/packages/bootstrap/x86_64-linux/guile-2.0.7.tar.xz
nodist_bootstrap_i686_linux_DATA =					\
  gnu/packages/bootstrap/i686-linux/guile-2.0.7.tar.xz

# Those files must remain executable, so they remain executable once
# imported into the store.
install-data-hook:
	chmod +x $(DESTDIR)$(bootstrapdir)/*/{bash,mkdir,tar,xz}

DISTCLEANFILES =				\
  $(nodist_bootstrap_x86_64_linux_DATA)		\
  $(nodist_bootstrap_i686_linux_DATA)

# Method to download a file from an external source.
DOWNLOAD_FILE =								\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir):$$GUILE_LOAD_COMPILED_PATH"	\
  $(GUILE) --no-auto-compile -L "$(top_builddir)" -L "$(top_srcdir)"	\
           "$(top_srcdir)/build-aux/download.scm"

gnu/packages/bootstrap/x86_64-linux/guile-2.0.7.tar.xz:
	$(MKDIR_P) `dirname "$@"`
	$(DOWNLOAD_FILE) "$@" "bc43210dcd146d242bef4d354b0aeac12c4ef3118c07502d17ffa8d49e15aa2c"
gnu/packages/bootstrap/i686-linux/guile-2.0.7.tar.xz:
	$(MKDIR_P) `dirname "$@"`
	$(DOWNLOAD_FILE) "$@" "f9a7c6f4c556eaafa2a69bcf07d4ffbb6682ea831d4c9da9ba095aca3ccd217c"

nobase_nodist_guilemodule_DATA = $(GOBJECTS) guix/config.scm

TESTS =						\
  tests/base32.scm				\
  tests/builders.scm				\
  tests/derivations.scm				\
  tests/utils.scm				\
  tests/build-utils.scm				\
  tests/packages.scm				\
  tests/snix.scm				\
  tests/store.scm				\
  tests/union.scm				\
  tests/guix-build.sh				\
  tests/guix-download.sh			\
  tests/guix-gc.sh				\
  tests/guix-package.sh

TEST_EXTENSIONS = .scm .sh

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

EXTRA_DIST =					\
  HACKING					\
  ROADMAP					\
  TODO						\
  .dir-locals.el				\
  hydra.scm					\
  build-aux/download.scm			\
  srfi/srfi-64.scm				\
  srfi/srfi-64.upstream.scm			\
  tests/test.drv				\
  build-aux/config.rpath			\
  bootstrap					\
  release.nix					\
  $(TESTS)

CLEANFILES = $(GOBJECTS) *.log

.scm.go:
	$(MKDIR_P) `dirname "$@"`
	$(top_builddir)/pre-inst-env					\
	$(GUILD) compile -L "$(top_builddir)" -L "$(top_srcdir)"	\
	  -Wformat -Wunbound-variable -Warity-mismatch			\
	  --target="$(host)"						\
	  -o "$@" "$<"

SUFFIXES = .go

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guix_install_go_files = install-nobase_nodist_guilemoduleDATA
$(guix_install_go_files): install-nobase_dist_guilemoduleDATA

SUBDIRS = po

info_TEXINFOS = doc/guix.texi
EXTRA_DIST += doc/fdl-1.3.texi

if BUILD_DAEMON

include daemon.am

endif BUILD_DAEMON

ACLOCAL_AMFLAGS = -I m4
AM_DISTCHECK_CONFIGURE_FLAGS =			\
  --with-libgcrypt-prefix="$(LIBGCRYPT_PREFIX)"	\
  --with-nix-prefix="$(NIX_PREFIX)"		\
  --enable-daemon
