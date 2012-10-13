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

bin_SCRIPTS = guix-build

MODULES =					\
  guix/utils.scm				\
  guix/derivations.scm				\
  guix/build-system.scm				\
  guix/build-system/gnu.scm			\
  guix/build-system/trivial.scm			\
  guix/ftp.scm					\
  guix/ftp-client.scm				\
  guix/http.scm					\
  guix/store.scm				\
  guix/build/gnu-build-system.scm		\
  guix/build/ftp.scm				\
  guix/build/http.scm				\
  guix/build/utils.scm				\
  guix/packages.scm				\
  guix.scm					\
  distro.scm					\
  distro/packages/base.scm			\
  distro/packages/databases.scm			\
  distro/packages/guile.scm			\
  distro/packages/ld-wrapper.scm		\
  distro/packages/typesetting.scm

GOBJECTS = $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA = $(MODULES)

patchdir = $(pkgdatadir)/patches
dist_patch_DATA =					\
  distro/patches/binutils-ld-new-dtags.patch		\
  distro/patches/diffutils-gets-undeclared.patch	\
  distro/patches/guile-1.8-cpp-4.5.patch		\
  distro/patches/guile-relocatable.patch		\
  distro/patches/libtool-skip-tests.patch		\
  distro/patches/m4-gets-undeclared.patch		\
  distro/patches/m4-readlink-EINVAL.patch		\
  distro/patches/m4-s_isdir.patch			\
  distro/patches/make-impure-dirs.patch			\
  distro/patches/findutils-absolute-paths.patch		\
  distro/patches/perl-no-sys-dirs.patch			\
  distro/patches/readline-link-ncurses.patch		\
  distro/patches/tar-gets-undeclared.patch

nobase_nodist_guilemodule_DATA = $(GOBJECTS)

TESTS =						\
  tests/builders.scm				\
  tests/derivations.scm				\
  tests/utils.scm				\
  tests/build-utils.scm				\
  tests/packages.scm

TESTS_ENVIRONMENT =								\
  NIX_HASH="$(NIX_HASH)";							\
  NIX_INSTANTIATE="$(NIX_INSTANTIATE)";						\
  NIXPKGS="$(NIXPKGS)";								\
  DISTRO_PATCH_DIRECTORY="$(top_srcdir)/distro/patches"				\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir):$$GUILE_LOAD_COMPILED_PATH";	\
  export NIX_HASH NIX_INSTANTIATE NIXPKGS GUILE_LOAD_COMPILED_PATH;

LOG_COMPILER =					\
  $(GUILE) --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST =					\
  .dir-locals.el				\
  srfi/srfi-64.scm				\
  srfi/srfi-64.upstream.scm			\
  tests/test.drv				\
  build-aux/config.rpath			\
  release.nix					\
  $(TESTS)

CLEANFILES = $(GOBJECTS) *.log

.scm.go:
	$(MKDIR_P) `dirname "$@"`
	NIX_INSTANTIATE="$(NIX_INSTANTIATE)"					\
	NIXPKGS="$(NIXPKGS)"							\
	LIBGCRYPT="$(LIBGCRYPT)"						\
	DISTRO_PATCH_DIRECTORY="$(top_srcdir)/distro/patches"			\
	DISTRO_INSTALLED_PATCH_DIRECTORY="$(patchdir)"				\
	GUILE_AUTO_COMPILE=0							\
	GUILE_LOAD_COMPILED_PATH="$(top_builddir):$$GUILE_LOAD_COMPILED_PATH"	\
	$(GUILD) compile -L "$(top_srcdir)"					\
	  -Wformat -Wunbound-variable -Warity-mismatch				\
	  --target="$(host)"							\
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
AM_DISTCHECK_CONFIGURE_FLAGS =					\
  --with-nixpkgs=$(NIXPKGS) --with-nix-prefix="$(NIX_PREFIX)"
