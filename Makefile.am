# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  scripts/guix

nodist_noinst_SCRIPTS =				\
  pre-inst-env					\
  test-env

include gnu-system.am

MODULES =					\
  guix/base32.scm				\
  guix/base64.scm				\
  guix/records.scm				\
  guix/hash.scm					\
  guix/pk-crypto.scm				\
  guix/pki.scm					\
  guix/utils.scm				\
  guix/download.scm				\
  guix/git-download.scm				\
  guix/monads.scm				\
  guix/profiles.scm				\
  guix/serialization.scm			\
  guix/nar.scm					\
  guix/derivations.scm				\
  guix/gnu-maintenance.scm			\
  guix/licenses.scm				\
  guix/build-system.scm				\
  guix/build-system/cmake.scm			\
  guix/build-system/gnu.scm			\
  guix/build-system/perl.scm			\
  guix/build-system/python.scm			\
  guix/build-system/trivial.scm			\
  guix/ftp-client.scm				\
  guix/http-client.scm				\
  guix/gnupg.scm				\
  guix/store.scm				\
  guix/svn-download.scm				\
  guix/ui.scm					\
  guix/build/download.scm			\
  guix/build/cmake-build-system.scm		\
  guix/build/git.scm				\
  guix/build/gnome.scm				\
  guix/build/gnu-build-system.scm		\
  guix/build/gnu-dist.scm			\
  guix/build/linux-initrd.scm			\
  guix/build/perl-build-system.scm		\
  guix/build/python-build-system.scm		\
  guix/build/utils.scm				\
  guix/build/union.scm				\
  guix/build/pull.scm				\
  guix/build/rpath.scm				\
  guix/build/svn.scm				\
  guix/packages.scm				\
  guix/snix.scm					\
  guix/scripts/download.scm			\
  guix/scripts/build.scm			\
  guix/scripts/archive.scm			\
  guix/scripts/import.scm			\
  guix/scripts/package.scm			\
  guix/scripts/gc.scm				\
  guix/scripts/hash.scm				\
  guix/scripts/pull.scm				\
  guix/scripts/substitute-binary.scm		\
  guix/scripts/authenticate.scm			\
  guix/scripts/refresh.scm			\
  guix/scripts/system.scm			\
  guix.scm					\
  $(GNU_SYSTEM_MODULES)

if BUILD_DAEMON_OFFLOAD

MODULES +=					\
  guix/scripts/offload.scm

endif BUILD_DAEMON_OFFLOAD

# Because of the autoload hack in (guix build download), we must build it
# first to avoid errors on systems where (gnutls) is unavailable.
guix/scripts/download.go: guix/build/download.go
guix/download.go: guix/build/download.go


GOBJECTS = $(MODULES:%.scm=%.go) guix/config.go

nobase_dist_guilemodule_DATA = $(MODULES)
nobase_nodist_guilemodule_DATA = $(GOBJECTS) guix/config.scm

# Do we need to provide our own non-broken (srfi srfi-37) module?
if INSTALL_SRFI_37

nobase_nodist_guilemodule_DATA += srfi/srfi-37.scm
GOBJECTS += srfi/srfi-37.go

srfi/srfi-37.scm: srfi/srfi-37.scm.in
	$(MKDIR_P) srfi
	cp "$<" "$@"

endif INSTALL_SRFI_37

# Handy way to remove the .go files without removing all the rest.
clean-go:
	-$(RM) -f $(GOBJECTS)


SCM_TESTS =					\
  tests/base32.scm				\
  tests/base64.scm				\
  tests/hash.scm				\
  tests/pk-crypto.scm				\
  tests/pki.scm					\
  tests/substitute-binary.scm			\
  tests/builders.scm				\
  tests/derivations.scm				\
  tests/ui.scm					\
  tests/records.scm				\
  tests/utils.scm				\
  tests/build-utils.scm				\
  tests/packages.scm				\
  tests/snix.scm				\
  tests/store.scm				\
  tests/monads.scm				\
  tests/nar.scm					\
  tests/union.scm				\
  tests/profiles.scm

SH_TESTS =					\
  tests/guix-build.sh				\
  tests/guix-download.sh			\
  tests/guix-gc.sh				\
  tests/guix-hash.sh				\
  tests/guix-package.sh				\
  tests/guix-archive.sh				\
  tests/guix-authenticate.sh

if BUILD_DAEMON

SH_TESTS += tests/guix-register.sh

endif BUILD_DAEMON


TESTS = $(SCM_TESTS) $(SH_TESTS)

TEST_EXTENSIONS = .scm .sh

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

# Make sure `tests/guix-gc.sh' runs last, after all the others.  Otherwise it
# could end up removing files from the store while they are being used by
# other instances of the daemon.
tests/guix-gc.log:							\
  $(patsubst %.sh,%.log,$(filter-out tests/guix-gc.sh,$(SH_TESTS)))	\
  $(SCM_TESTS:%.scm=%.log)

# Public key used to sign substitutes from hydra.gnu.org.
pkgdata_DATA = hydra.gnu.org.pub

EXTRA_DIST =					\
  HACKING					\
  ROADMAP					\
  TODO						\
  .dir-locals.el				\
  build-aux/hydra/gnu-system.scm		\
  build-aux/hydra/guix.scm			\
  build-aux/check-available-binaries.scm	\
  build-aux/download.scm			\
  build-aux/list-packages.scm			\
  build-aux/sync-descriptions.scm		\
  srfi/srfi-37.scm.in				\
  srfi/srfi-64.scm				\
  srfi/srfi-64.upstream.scm			\
  tests/test.drv				\
  tests/signing-key.pub				\
  tests/signing-key.sec				\
  build-aux/config.rpath			\
  bootstrap					\
  release.nix					\
  $(TESTS)

if !BUILD_DAEMON_OFFLOAD

EXTRA_DIST +=					\
  guix/scripts/offload.scm

endif !BUILD_DAEMON_OFFLOAD


CLEANFILES =					\
  $(GOBJECTS)					\
  $(SCM_TESTS:tests/%.scm=%.log)

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC" $@;

# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(AM_V_GUILEC)$(MKDIR_P) `dirname "$@"` ;			\
	LC_ALL=C							\
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

include doc.am

if BUILD_DAEMON

include daemon.am

endif BUILD_DAEMON

ACLOCAL_AMFLAGS = -I m4
AM_DISTCHECK_CONFIGURE_FLAGS =			\
  --with-libgcrypt-prefix="$(LIBGCRYPT_PREFIX)"	\
  --with-nix-prefix="$(NIX_PREFIX)"		\
  --enable-daemon

dist-hook: sync-descriptions gen-ChangeLog assert-no-store-file-names
distcheck-hook: assert-binaries-available

sync-descriptions:
	-$(top_builddir)/pre-inst-env $(GUILE)		\
	   $(top_srcdir)/build-aux/sync-descriptions.scm

gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;				\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

# Make sure we're not shipping a file that embeds a local /gnu/store file name.
assert-no-store-file-names:
	if grep -r --exclude=*.texi --exclude=*.info				\
	     "$(storedir)/[a-z0-9]{32}-" $(distdir) ;				\
	then									\
	  echo "error: store file names embedded in the distribution" >&2 ;	\
	  exit 1 ;								\
	fi

# Make sure hydra.gnu.org has the important binaries.
assert-binaries-available:
	$(top_builddir)/pre-inst-env "$(GUILE)"				\
	  "$(top_srcdir)/build-aux/check-available-binaries.scm"

.PHONY: sync-descriptions gen-ChangeLog clean-go
.PHONY: assert-no-store-file-names assert-binaries-available
