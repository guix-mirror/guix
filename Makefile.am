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
  guix/http.scm					\
  guix/store.scm				\
  guix/build/gnu-build-system.scm		\
  guix/build/http.scm				\
  guix/build/utils.scm				\
  guix/packages.scm				\
  guix.scm					\
  distro.scm					\
  distro/base.scm

GOBJECTS = $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA =			\
  $(MODULES)					\
  distro/guile-1.8-cpp-4.5.patch		\
  distro/m4-readlink-EINVAL.patch		\
  distro/m4-s_isdir.patch			\
  distro/make-impure-dirs.patch			\
  distro/findutils-absolute-paths.patch		\
  distro/readline-link-ncurses.patch

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
  GUILE_LOAD_COMPILED_PATH="$(top_builddir):$$GUILE_LOAD_COMPILED_PATH";	\
  export NIX_HASH NIX_INSTANTIATE NIXPKGS GUILE_LOAD_COMPILED_PATH;

LOG_COMPILER =					\
  $(GUILE) --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST =					\
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

ACLOCAL_AMFLAGS = -I m4
AM_DISTCHECK_CONFIGURE_FLAGS =					\
  --with-nixpkgs=$(NIXPKGS) --with-nix-prefix="$(NIX_PREFIX)"
