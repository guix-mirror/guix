# Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
# Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of Guix.
#
# Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guix.  If not, see <http://www.gnu.org/licenses/>.

bin_SCRIPTS =					\
  guix-build					\
  guix-download					\
  guix-import					\
  guix-package

MODULES =					\
  guix/base32.scm				\
  guix/utils.scm				\
  guix/derivations.scm				\
  guix/download.scm				\
  guix/gnu-maintenance.scm			\
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
  distro.scm					\
  distro/packages/autotools.scm			\
  distro/packages/base.scm			\
  distro/packages/bash.scm			\
  distro/packages/bdw-gc.scm			\
  distro/packages/bison.scm			\
  distro/packages/bootstrap.scm			\
  distro/packages/compression.scm		\
  distro/packages/cpio.scm			\
  distro/packages/ddrescue.scm			\
  distro/packages/ed.scm			\
  distro/packages/flex.scm			\
  distro/packages/gawk.scm			\
  distro/packages/gdbm.scm			\
  distro/packages/gettext.scm			\
  distro/packages/gnupg.scm			\
  distro/packages/gnutls.scm			\
  distro/packages/gperf.scm			\
  distro/packages/guile.scm			\
  distro/packages/help2man.scm			\
  distro/packages/ld-wrapper.scm		\
  distro/packages/less.scm			\
  distro/packages/libffi.scm			\
  distro/packages/libsigsegv.scm		\
  distro/packages/libunistring.scm		\
  distro/packages/linux.scm			\
  distro/packages/lout.scm			\
  distro/packages/lsh.scm			\
  distro/packages/m4.scm			\
  distro/packages/make-bootstrap.scm		\
  distro/packages/multiprecision.scm		\
  distro/packages/nano.scm			\
  distro/packages/ncurses.scm			\
  distro/packages/nettle.scm			\
  distro/packages/perl.scm			\
  distro/packages/pkg-config.scm		\
  distro/packages/pth.scm			\
  distro/packages/readline.scm			\
  distro/packages/recutils.scm			\
  distro/packages/shishi.scm			\
  distro/packages/system.scm			\
  distro/packages/texinfo.scm			\
  distro/packages/time.scm			\
  distro/packages/wget.scm			\
  distro/packages/which.scm			\
  distro/packages/zile.scm


GOBJECTS = $(MODULES:%.scm=%.go) guix/config.go

nobase_dist_guilemodule_DATA = $(MODULES)

patchdir = $(guilemoduledir)/distro/packages/patches
dist_patch_DATA =						\
  distro/packages/patches/binutils-ld-new-dtags.patch		\
  distro/packages/patches/cpio-gets-undeclared.patch		\
  distro/packages/patches/diffutils-gets-undeclared.patch	\
  distro/packages/patches/flex-bison-tests.patch		\
  distro/packages/patches/gettext-gets-undeclared.patch		\
  distro/packages/patches/glibc-no-ld-so-cache.patch		\
  distro/packages/patches/guile-1.8-cpp-4.5.patch		\
  distro/packages/patches/guile-default-utf8.patch		\
  distro/packages/patches/guile-relocatable.patch		\
  distro/packages/patches/libtool-skip-tests.patch		\
  distro/packages/patches/lsh-guile-compat.patch		\
  distro/packages/patches/lsh-no-root-login.patch		\
  distro/packages/patches/lsh-pam-service-name.patch		\
  distro/packages/patches/m4-gets-undeclared.patch		\
  distro/packages/patches/m4-readlink-EINVAL.patch		\
  distro/packages/patches/m4-s_isdir.patch			\
  distro/packages/patches/make-impure-dirs.patch		\
  distro/packages/patches/findutils-absolute-paths.patch	\
  distro/packages/patches/perl-no-sys-dirs.patch		\
  distro/packages/patches/readline-link-ncurses.patch		\
  distro/packages/patches/shishi-gets-undeclared.patch		\
  distro/packages/patches/tar-gets-undeclared.patch

bootstrapdir = $(guilemoduledir)/distro/packages/bootstrap
bootstrap_x86_64_linuxdir = $(bootstrapdir)/x86_64-linux
bootstrap_i686_linuxdir = $(bootstrapdir)/i686-linux

dist_bootstrap_x86_64_linux_DATA =		\
  distro/packages/bootstrap/x86_64-linux/bash	\
  distro/packages/bootstrap/x86_64-linux/mkdir	\
  distro/packages/bootstrap/x86_64-linux/tar	\
  distro/packages/bootstrap/x86_64-linux/xz

dist_bootstrap_i686_linux_DATA =		\
  distro/packages/bootstrap/i686-linux/bash	\
  distro/packages/bootstrap/i686-linux/mkdir	\
  distro/packages/bootstrap/i686-linux/tar	\
  distro/packages/bootstrap/i686-linux/xz

# Big bootstrap binaries are not included in the tarball.  Instead, they
# are downloaded.
nodist_bootstrap_x86_64_linux_DATA =					\
  distro/packages/bootstrap/x86_64-linux/guile-bootstrap-2.0.6.tar.xz
nodist_bootstrap_i686_linux_DATA =					\
  distro/packages/bootstrap/i686-linux/guile-bootstrap-2.0.6.tar.xz

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

distro/packages/bootstrap/x86_64-linux/guile-bootstrap-2.0.6.tar.xz:
	$(MKDIR_P) `dirname "$@"`
	$(DOWNLOAD_FILE) "$@" "0467a82cbe4136f60a79eb4176011bf88cf28ea19c9ad9defa365811ff8e11cf"
distro/packages/bootstrap/i686-linux/guile-bootstrap-2.0.6.tar.xz:
	$(MKDIR_P) `dirname "$@"`
	$(DOWNLOAD_FILE) "$@" "93b537766dfab3ad287143523751e3ec02dd32d3ccaf88ad2d31c63158f342ee"

nobase_nodist_guilemodule_DATA = $(GOBJECTS) guix/config.scm

TESTS =						\
  tests/base32.scm				\
  tests/builders.scm				\
  tests/derivations.scm				\
  tests/utils.scm				\
  tests/build-utils.scm				\
  tests/packages.scm				\
  tests/snix.scm				\
  tests/union.scm				\
  tests/guix-build.sh				\
  tests/guix-download.sh			\
  tests/guix-package.sh

TEST_EXTENSIONS = .scm .sh

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/pre-inst-env $(SHELL)
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

ACLOCAL_AMFLAGS = -I m4
AM_DISTCHECK_CONFIGURE_FLAGS =			\
  --with-libgcrypt-prefix="$(LIBGCRYPT_PREFIX)"	\
  --with-nix-prefix="$(NIX_PREFIX)"
