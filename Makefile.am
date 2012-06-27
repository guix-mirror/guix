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

MODULES =					\
  guix/derivations.scm				\
  guix/build-system.scm				\
  guix/build-system/gnu.scm			\
  guix/http.scm					\
  guix/store.scm				\
  guix/utils.scm				\
  guix/build/gnu-build-system.scm		\
  guix/build/http.scm				\
  guix/build/utils.scm				\
  guix.scm

GOBJECTS = $(MODULES:%.scm=%.go)

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guilemodule_DATA = $(GOBJECTS)

TESTS =						\
  tests/builders.scm				\
  tests/derivations.scm				\
  tests/utils.scm

TESTS_ENVIRONMENT =							\
  NIXPKGS="$(NIXPKGS)"							\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir):$$GUILE_LOAD_COMPILED_PATH"	\
  $(GUILE) -L "$(top_srcdir)"

CLEANFILES = $(GOBJECTS) *.log

.scm.go:
	$(MKDIR_P) `dirname "$@"`
	NIXPKGS="$(NIXPKGS)"							\
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
