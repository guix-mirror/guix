# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2013 Andreas Enge <andreas@enge.fr>
# Copyright © 2015, 2017 Alex Kost <alezost@gmail.com>
# Copyright © 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
# Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
# Copyright © 2017 Leo Famulari <leo@famulari.name>
# Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
# Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
# Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
# Copyright © 2018 Nils Gillmann <ng0@n0.is>
# Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
# Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
# Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

MSGMERGE_UPDATE = @MSGMERGE@ --update

bin_SCRIPTS = scripts/guix

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

scripts/guix: scripts/guix.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	$(AM_V_at)chmod a+x,a-w "$@-t" && mv -f "$@-t" "$@"

nodist_noinst_SCRIPTS =				\
  pre-inst-env					\
  test-env

# Modules that are not compiled but are installed nonetheless, such as
# build-side modules with unusual dependencies.
MODULES_NOT_COMPILED =				\
  guix/man-db.scm

include gnu/local.mk
include po/doc/local.mk

MODULES =					\
  guix/base16.scm				\
  guix/base32.scm				\
  guix/base64.scm				\
  guix/ci.scm					\
  guix/cpio.scm					\
  guix/deprecation.scm				\
  guix/docker.scm	   			\
  guix/records.scm				\
  guix/pki.scm					\
  guix/progress.scm				\
  guix/combinators.scm				\
  guix/memoization.scm				\
  guix/utils.scm				\
  guix/sets.scm					\
  guix/modules.scm				\
  guix/download.scm				\
  guix/discovery.scm				\
  guix/git-download.scm				\
  guix/hg-download.scm				\
  guix/swh.scm					\
  guix/monads.scm				\
  guix/monad-repl.scm				\
  guix/gexp.scm					\
  guix/profiles.scm				\
  guix/serialization.scm			\
  guix/nar.scm					\
  guix/derivations.scm				\
  guix/grafts.scm				\
  guix/inferior.scm				\
  guix/describe.scm				\
  guix/channels.scm				\
  guix/gnu-maintenance.scm			\
  guix/self.scm					\
  guix/upstream.scm				\
  guix/licenses.scm				\
  guix/glob.scm					\
  guix/git.scm					\
  guix/graph.scm				\
  guix/cache.scm				\
  guix/cve.scm					\
  guix/workers.scm				\
  guix/zlib.scm					\
  guix/build-system.scm				\
  guix/build-system/android-ndk.scm		\
  guix/build-system/ant.scm			\
  guix/build-system/cargo.scm			\
  guix/build-system/clojure.scm			\
  guix/build-system/cmake.scm			\
  guix/build-system/dub.scm			\
  guix/build-system/dune.scm			\
  guix/build-system/emacs.scm			\
  guix/build-system/font.scm			\
  guix/build-system/go.scm			\
  guix/build-system/meson.scm			\
  guix/build-system/minify.scm			\
  guix/build-system/asdf.scm			\
  guix/build-system/glib-or-gtk.scm		\
  guix/build-system/gnu.scm			\
  guix/build-system/guile.scm			\
  guix/build-system/haskell.scm			\
  guix/build-system/perl.scm			\
  guix/build-system/python.scm			\
  guix/build-system/ocaml.scm			\
  guix/build-system/waf.scm			\
  guix/build-system/r.scm			\
  guix/build-system/ruby.scm			\
  guix/build-system/scons.scm			\
  guix/build-system/texlive.scm			\
  guix/build-system/trivial.scm			\
  guix/ftp-client.scm				\
  guix/http-client.scm				\
  guix/gnupg.scm				\
  guix/elf.scm					\
  guix/profiling.scm				\
  guix/store.scm				\
  guix/cvs-download.scm				\
  guix/svn-download.scm				\
  guix/i18n.scm					\
  guix/ui.scm					\
  guix/status.scm				\
  guix/build/android-ndk-build-system.scm	\
  guix/build/ant-build-system.scm		\
  guix/build/download.scm			\
  guix/build/download-nar.scm			\
  guix/build/cargo-build-system.scm		\
  guix/build/cargo-utils.scm			\
  guix/build/cmake-build-system.scm		\
  guix/build/dub-build-system.scm		\
  guix/build/dune-build-system.scm		\
  guix/build/emacs-build-system.scm		\
  guix/build/meson-build-system.scm		\
  guix/build/minify-build-system.scm		\
  guix/build/font-build-system.scm		\
  guix/build/go-build-system.scm		\
  guix/build/asdf-build-system.scm		\
  guix/build/git.scm				\
  guix/build/hg.scm				\
  guix/build/glib-or-gtk-build-system.scm	\
  guix/build/gnu-build-system.scm		\
  guix/build/gnu-dist.scm			\
  guix/build/guile-build-system.scm		\
  guix/build/perl-build-system.scm		\
  guix/build/python-build-system.scm		\
  guix/build/ocaml-build-system.scm		\
  guix/build/r-build-system.scm			\
  guix/build/ruby-build-system.scm		\
  guix/build/scons-build-system.scm		\
  guix/build/texlive-build-system.scm		\
  guix/build/waf-build-system.scm		\
  guix/build/haskell-build-system.scm		\
  guix/build/store-copy.scm			\
  guix/build/utils.scm				\
  guix/build/union.scm				\
  guix/build/profiles.scm			\
  guix/build/compile.scm			\
  guix/build/rpath.scm				\
  guix/build/cvs.scm				\
  guix/build/svn.scm				\
  guix/build/syscalls.scm                       \
  guix/build/gremlin.scm			\
  guix/build/debug-link.scm			\
  guix/build/clojure-build-system.scm		\
  guix/build/clojure-utils.scm			\
  guix/build/emacs-utils.scm			\
  guix/build/java-utils.scm			\
  guix/build/lisp-utils.scm			\
  guix/build/graft.scm				\
  guix/build/bournish.scm			\
  guix/build/qt-utils.scm			\
  guix/build/make-bootstrap.scm			\
  guix/search-paths.scm				\
  guix/packages.scm				\
  guix/import/cabal.scm				\
  guix/import/cpan.scm				\
  guix/import/cran.scm				\
  guix/import/crate.scm				\
  guix/import/elpa.scm   			\
  guix/import/gem.scm				\
  guix/import/github.scm   			\
  guix/import/gnome.scm				\
  guix/import/gnu.scm				\
  guix/import/hackage.scm			\
  guix/import/json.scm				\
  guix/import/opam.scm				\
  guix/import/print.scm				\
  guix/import/pypi.scm				\
  guix/import/snix.scm				\
  guix/import/stackage.scm			\
  guix/import/texlive.scm   			\
  guix/import/utils.scm				\
  guix/scripts.scm				\
  guix/scripts/download.scm			\
  guix/scripts/perform-download.scm		\
  guix/scripts/build.scm			\
  guix/scripts/archive.scm			\
  guix/scripts/import.scm			\
  guix/scripts/package.scm			\
  guix/scripts/gc.scm				\
  guix/scripts/hash.scm				\
  guix/scripts/pack.scm				\
  guix/scripts/pull.scm				\
  guix/scripts/processes.scm			\
  guix/scripts/substitute.scm			\
  guix/scripts/authenticate.scm			\
  guix/scripts/refresh.scm			\
  guix/scripts/repl.scm				\
  guix/scripts/describe.scm			\
  guix/scripts/system.scm			\
  guix/scripts/system/search.scm		\
  guix/scripts/lint.scm				\
  guix/scripts/challenge.scm			\
  guix/scripts/import/crate.scm			\
  guix/scripts/import/cran.scm			\
  guix/scripts/import/elpa.scm  		\
  guix/scripts/import/gem.scm			\
  guix/scripts/import/gnu.scm			\
  guix/scripts/import/hackage.scm		\
  guix/scripts/import/json.scm  		\
  guix/scripts/import/nix.scm			\
  guix/scripts/import/opam.scm			\
  guix/scripts/import/pypi.scm			\
  guix/scripts/import/stackage.scm		\
  guix/scripts/import/texlive.scm  		\
  guix/scripts/environment.scm			\
  guix/scripts/publish.scm			\
  guix/scripts/edit.scm				\
  guix/scripts/size.scm				\
  guix/scripts/graph.scm			\
  guix/scripts/weather.scm			\
  guix/scripts/container.scm			\
  guix/scripts/container/exec.scm		\
  guix.scm					\
  $(GNU_SYSTEM_MODULES)

if HAVE_GUILE_SSH

MODULES +=					\
  guix/ssh.scm					\
  guix/scripts/copy.scm				\
  guix/store/ssh.scm

endif HAVE_GUILE_SSH

if BUILD_DAEMON_OFFLOAD

MODULES +=					\
  guix/scripts/offload.scm

endif BUILD_DAEMON_OFFLOAD

# Scheme implementation of the build daemon and related functionality.
STORE_MODULES =					\
  guix/store/database.scm			\
  guix/store/deduplication.scm

MODULES += $(STORE_MODULES)

# Internal modules with test suite support.
dist_noinst_DATA = guix/tests.scm guix/tests/http.scm

# Auxiliary files for packages.
AUX_FILES =						\
  gnu/packages/aux-files/chromium/master-preferences.json		\
  gnu/packages/aux-files/emacs/guix-emacs.el		\
  gnu/packages/aux-files/linux-libre/4.20-arm.conf	\
  gnu/packages/aux-files/linux-libre/4.20-arm64.conf	\
  gnu/packages/aux-files/linux-libre/4.20-i686.conf	\
  gnu/packages/aux-files/linux-libre/4.20-x86_64.conf	\
  gnu/packages/aux-files/linux-libre/4.19-arm.conf	\
  gnu/packages/aux-files/linux-libre/4.19-arm64.conf	\
  gnu/packages/aux-files/linux-libre/4.19-i686.conf	\
  gnu/packages/aux-files/linux-libre/4.19-x86_64.conf	\
  gnu/packages/aux-files/linux-libre/4.14-arm.conf	\
  gnu/packages/aux-files/linux-libre/4.14-i686.conf	\
  gnu/packages/aux-files/linux-libre/4.14-x86_64.conf	\
  gnu/packages/aux-files/linux-libre/4.9-i686.conf	\
  gnu/packages/aux-files/linux-libre/4.9-x86_64.conf	\
  gnu/packages/aux-files/linux-libre/4.4-i686.conf	\
  gnu/packages/aux-files/linux-libre/4.4-x86_64.conf	\
  gnu/packages/aux-files/run-in-namespace.c

# Templates, examples.
EXAMPLES =					\
  gnu/system/examples/bare-bones.tmpl		\
  gnu/system/examples/beaglebone-black.tmpl	\
  gnu/system/examples/desktop.tmpl		\
  gnu/system/examples/lightweight-desktop.tmpl	\
  gnu/system/examples/vm-image.tmpl

GOBJECTS = $(MODULES:%.scm=%.go) guix/config.go $(dist_noinst_DATA:%.scm=%.go)

nobase_dist_guilemodule_DATA =					\
  guix/store/schema.sql						\
  $(MODULES) $(MODULES_NOT_COMPILED) $(AUX_FILES) $(EXAMPLES)	\
  $(MISC_DISTRO_FILES)
nobase_nodist_guilemodule_DATA = guix/config.scm
nobase_nodist_guileobject_DATA = $(GOBJECTS)

# Handy way to remove the .go files without removing all the rest.
clean-go:
	-$(RM) -f $(GOBJECTS)
	@find . -path ./test-tmp -prune -o -name '*.go' -print | \
	  if test -t 1; then \
	    xargs -r echo -e "\033[31mwarning:\033[0m stray .go files:"; \
	  else \
	    xargs -r echo "warning: stray .go files:"; \
	  fi


# Test extensions; has to be unconditional.
TEST_EXTENSIONS = .scm .sh

if CAN_RUN_TESTS

SCM_TESTS =					\
  tests/base16.scm				\
  tests/base32.scm				\
  tests/base64.scm				\
  tests/channels.scm				\
  tests/cpan.scm				\
  tests/cpio.scm				\
  tests/crate.scm				\
  tests/gem.scm					\
  tests/pki.scm					\
  tests/print.scm				\
  tests/sets.scm				\
  tests/modules.scm				\
  tests/gnu-maintenance.scm			\
  tests/substitute.scm				\
  tests/builders.scm				\
  tests/derivations.scm				\
  tests/glob.scm				\
  tests/grafts.scm				\
  tests/ui.scm					\
  tests/status.scm				\
  tests/records.scm				\
  tests/processes.scm				\
  tests/upstream.scm				\
  tests/combinators.scm				\
  tests/discovery.scm				\
  tests/utils.scm				\
  tests/build-utils.scm				\
  tests/packages.scm				\
  tests/snix.scm				\
  tests/hackage.scm				\
  tests/cran.scm				\
  tests/elpa.scm				\
  tests/texlive.scm				\
  tests/store.scm				\
  tests/monads.scm				\
  tests/gexp.scm				\
  tests/nar.scm					\
  tests/union.scm				\
  tests/profiles.scm				\
  tests/search-paths.scm			\
  tests/syscalls.scm				\
  tests/inferior.scm				\
  tests/gremlin.scm				\
  tests/debug-link.scm				\
  tests/bournish.scm				\
  tests/lint.scm				\
  tests/publish.scm				\
  tests/scripts.scm				\
  tests/size.scm				\
  tests/graph.scm				\
  tests/challenge.scm				\
  tests/cache.scm				\
  tests/cve.scm					\
  tests/workers.scm				\
  tests/zlib.scm				\
  tests/file-systems.scm			\
  tests/uuid.scm				\
  tests/system.scm				\
  tests/services.scm				\
  tests/scripts-build.scm			\
  tests/containers.scm				\
  tests/opam.scm				\
  tests/pack.scm				\
  tests/pypi.scm				\
  tests/import-utils.scm			\
  tests/store-database.scm			\
  tests/store-deduplication.scm

SH_TESTS =					\
  tests/guix-build.sh				\
  tests/guix-build-branch.sh			\
  tests/guix-download.sh			\
  tests/guix-gc.sh				\
  tests/guix-hash.sh				\
  tests/guix-pack.sh				\
  tests/guix-pack-localstatedir.sh		\
  tests/guix-pack-relocatable.sh		\
  tests/guix-package.sh				\
  tests/guix-package-net.sh			\
  tests/guix-system.sh				\
  tests/guix-archive.sh				\
  tests/guix-authenticate.sh			\
  tests/guix-environment.sh			\
  tests/guix-environment-container.sh		\
  tests/guix-graph.sh				\
  tests/guix-describe.sh			\
  tests/guix-lint.sh

TESTS = $(SCM_TESTS) $(SH_TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =				\
  $(top_builddir)/test-env --quiet-stderr	\
  $(GUILE) --no-auto-compile -e main		\
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

# Make sure `tests/guix-gc.sh' runs last, after all the others.  Otherwise it
# could end up removing files from the store while they are being used by
# other instances of the daemon.
tests/guix-gc.log:							\
  $(patsubst %.sh,%.log,$(filter-out tests/guix-gc.sh,$(SH_TESTS)))	\
  $(SCM_TESTS:%.scm=%.log)

else !CAN_RUN_TESTS

TESTS =
SH_TESTS =
SCM_TESTS =

# Automake always generates a 'check' target, so better not override it.
check-local:
	@echo
	@echo "Cannot run tests because file name limits would be exceeded." >&2
	@echo "Look for 'length' in the 'config.log' file for details." >&2
	@echo
	@exit 1

endif !CAN_RUN_TESTS

check-system: $(GOBJECTS)
	$(AM_V_at)$(top_builddir)/pre-inst-env			\
	   $(GUILE) --no-auto-compile				\
	   -e '(@@ (run-system-tests) run-system-tests)'	\
	   $(top_srcdir)/build-aux/run-system-tests.scm

# Public key used to sign substitutes from hydra.gnu.org & co.
dist_pkgdata_DATA =				\
  etc/substitutes/hydra.gnu.org.pub		\
  etc/substitutes/berlin.guixsd.org.pub		\
  etc/substitutes/ci.guix.info.pub

# Bash completion file.
dist_bashcompletion_DATA = etc/completion/bash/guix	\
  etc/completion/bash/guix-daemon

# Zsh completion file.
dist_zshcompletion_DATA = etc/completion/zsh/_guix

# Fish completion file.
dist_fishcompletion_DATA = etc/completion/fish/guix.fish

# SELinux policy
dist_selinux_policy_DATA = etc/guix-daemon.cil

EXTRA_DIST +=						\
  HACKING						\
  ROADMAP						\
  TODO							\
  CODE-OF-CONDUCT					\
  .dir-locals.el					\
  scripts/guix.in					\
  etc/guix-install.sh					\
  build-aux/build-self.scm				\
  build-aux/compile-all.scm				\
  build-aux/hydra/evaluate.scm				\
  build-aux/hydra/gnu-system.scm			\
  build-aux/hydra/guix.scm				\
  build-aux/hydra/guix-modular.scm			\
  build-aux/cuirass/gnu-system.scm			\
  build-aux/cuirass/guix-modular.scm			\
  build-aux/cuirass/hydra-to-cuirass.scm		\
  build-aux/check-available-binaries.scm		\
  build-aux/check-final-inputs-self-contained.scm	\
  build-aux/compile-as-derivation.scm			\
  build-aux/generate-authors.scm			\
  build-aux/test-driver.scm				\
  build-aux/update-guix-package.scm			\
  build-aux/update-NEWS.scm				\
  build-aux/run-system-tests.scm			\
  d3.v3.js						\
  graph.js						\
  tests/test.drv					\
  tests/signing-key.pub					\
  tests/signing-key.sec					\
  tests/cve-sample.xml					\
  build-aux/config.rpath				\
  bootstrap						\
  release.nix						\
  $(TESTS)

if !BUILD_DAEMON_OFFLOAD

EXTRA_DIST +=					\
  guix/scripts/offload.scm

endif !BUILD_DAEMON_OFFLOAD


CLEANFILES =					\
  $(bin_SCRIPTS)				\
  $(GOBJECTS)					\
  $(SCM_TESTS:tests/%.scm=%.log)

# Unset 'GUILE_LOAD_COMPILED_PATH' altogether while compiling.  Otherwise, if
# $GUILE_LOAD_COMPILED_PATH contains $(moduledir), we may find .go files in
# there that are newer than the local .scm files (for instance because the
# user ran 'make install' recently).  When that happens, we end up loading
# those previously-installed .go files, which may be stale, thereby breaking
# the whole thing.  Likewise, set 'XDG_CACHE_HOME' to avoid loading possibly
# stale files from ~/.cache/guile/ccache.
%.go: make-go ; @:
make-go: $(MODULES) guix/config.scm $(dist_noinst_DATA)
	$(AM_V_at)echo "Compiling Scheme modules..." ;			\
	unset GUILE_LOAD_COMPILED_PATH ;				\
	XDG_CACHE_HOME=/nowhere						\
	host=$(host) srcdir="$(top_srcdir)"				\
	$(top_builddir)/pre-inst-env					\
	$(GUILE) -L "$(top_builddir)" -L "$(top_srcdir)"		\
	  --no-auto-compile 						\
	  -s "$(top_srcdir)"/build-aux/compile-all.scm $^

SUFFIXES = .go

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guix_install_go_files = install-nobase_nodist_guileobjectDATA
$(guix_install_go_files): install-nobase_dist_guilemoduleDATA

# The above trick doesn't work for 'config.go' because both 'config.scm' and
# 'config.go' are listed in $(nobase_nodist_guileobject_DATA).  Thus, give it
# special treatment.
install-data-hook: set-bootstrap-executable-permissions
	touch "$(DESTDIR)$(guileobjectdir)/guix/config.go"

# Assuming Guix is already installed and the daemon is up and running, this
# rule builds from $(srcdir), creating and building derivations.
as-derivation:
	$(AM_V_at)echo "Building Guix in Guix..." ;		\
	$(GUILE) --no-auto-compile				\
	  "$(top_srcdir)/build-aux/compile-as-derivation.scm"	\
	  "$(abs_top_srcdir)"

SUBDIRS = po/guix po/packages
BUILT_SOURCES =

include doc/local.mk

if BUILD_DAEMON

include nix/local.mk

endif BUILD_DAEMON

ACLOCAL_AMFLAGS = -I m4

# Pass an explicit '--localstatedir' so that configure does not error out if
# it finds an existing installation with a different localstatedir.  Inherit
# 'ac_cv_guix_test_root' so that "make check" in $(distdir) does not have to
# repopulate the whole store, and to make sure $(GUIX_TEST_ROOT) is short
# enough for shebangs.
AM_DISTCHECK_CONFIGURE_FLAGS =			\
  --localstatedir="$$dc_install_base/var"	\
  --with-libgcrypt-prefix="$(LIBGCRYPT_PREFIX)"	\
  --with-libgcrypt-libdir="$(LIBGCRYPT_LIBDIR)"	\
  --with-nix-prefix="$(NIX_PREFIX)"		\
  --enable-daemon				\
  ac_cv_guix_test_root="$(GUIX_TEST_ROOT)"

# The self-contained tarball.
guix-binary.%.tar.xz:
	$(AM_V_GEN)GUIX_PACKAGE_PATH=					\
	tarball=`$(top_builddir)/pre-inst-env guix pack -K -C xz	\
	  -s "$*" --localstatedir --profile-name=current-guix guix` ;	\
	cp "$$tarball" "$@.tmp" ; mv "$@.tmp" "$@"


dist-hook: $(distdir)/ChangeLog gen-AUTHORS gen-tarball-version
dist-hook: assert-no-store-file-names
dist-hook: doc-po-update

distcheck-hook: assert-binaries-available assert-final-inputs-self-contained

EXTRA_DIST += $(top_srcdir)/.version
BUILT_SOURCES += $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo $(VERSION) > "$@-t" && mv "$@-t" "$@"

gen-tarball-version:
	echo $(VERSION) > "$(distdir)/.tarball-version"

gen-ChangeLog $(distdir)/ChangeLog:
	$(AM_V_GEN)if test -d .git; then		\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t;				\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

gen-AUTHORS:
	$(AM_V_GEN)if test -d .git; then			\
	  rm -f "$(distdir)/AUTHORS";				\
	  $(top_builddir)/pre-inst-env "$(GUILE)"		\
	    "$(top_srcdir)/build-aux/generate-authors.scm"	\
	    "$(top_srcdir)" "$(distdir)/AUTHORS";		\
	fi


#
# Release management.
#

releasedir = release-$(PACKAGE_VERSION)

PACKAGE_FULL_TARNAME = $(PACKAGE_TARNAME)-$(PACKAGE_VERSION)

# List of source tarballs produced.  This must be kept in sync with the
# 'dist-' options of 'AM_INIT_AUTOMAKE' in 'configure.ac'.
SOURCE_TARBALLS =					\
  $(foreach ext,tar.gz,$(PACKAGE_FULL_TARNAME).$(ext))

# Systems supported by Guix.
SUPPORTED_SYSTEMS ?= x86_64-linux i686-linux armhf-linux aarch64-linux

# Guix binary tarballs.
BINARY_TARBALLS =							\
  $(foreach system,$(SUPPORTED_SYSTEMS),guix-binary.$(system).tar.xz)

# Systems supported by GuixSD.
GUIXSD_SUPPORTED_SYSTEMS ?= x86_64-linux i686-linux

# Systems for which we build GuixSD VMs.
GUIXSD_VM_SYSTEMS ?= x86_64-linux

# Prefix of the GuixSD installation image file name.
GUIXSD_IMAGE_BASE = guixsd-install-$(PACKAGE_VERSION)

# Prefix of the GuixSD VM image file name.
GUIXSD_VM_IMAGE_BASE = guixsd-vm-image-$(PACKAGE_VERSION)

# The release process works in several phases:
#
#   0. We assume the developer created a 'vX.Y' tag.
#   1. Build the source tarball.
#   2. Update the 'guix' package so that it corresponds to the 'vX.Y' tag.
#   3. Build the binary tarballs for that 'guix' package.
#   4. Update the 'guix' package again.
#   5. Build the GuixSD installation images.  The images will run 'guix'
#      corresponding to 'vX.Y' + 1 commit, and they will install 'vX.Y'.
#
# This 'release' target takes care of everything and copies the resulting
# files to $(releasedir).
#
# XXX: Depend on 'dist' rather than 'distcheck' to work around the Gettext
# issue described at <https://savannah.gnu.org/bugs/index.php?51027>.
release: dist
	cd po; git checkout .
	@if ! git diff-index --quiet HEAD; then			\
	  echo "There are uncommitted changes; stopping." >&2 ;	\
	  exit 1 ;						\
	fi
	$(MKDIR_P) "$(releasedir)"
	rm -f "$(releasedir)"/*
	mv $(SOURCE_TARBALLS) "$(releasedir)"
	$(top_builddir)/pre-inst-env "$(GUILE)"			\
	   $(top_srcdir)/build-aux/update-guix-package.scm	\
	   "`git rev-parse HEAD`" "$(PACKAGE_VERSION)"
	git add $(top_srcdir)/gnu/packages/package-management.scm
	git commit -m "gnu: guix: Update to $(PACKAGE_VERSION)."
	rm -f $(BINARY_TARBALLS)
	$(MAKE) $(BINARY_TARBALLS)
	for system in $(SUPPORTED_SYSTEMS) ; do					\
	  mv "guix-binary.$$system.tar.xz"					\
	      "$(releasedir)/guix-binary-$(PACKAGE_VERSION).$$system.tar.xz" ;	\
	done
	$(top_builddir)/pre-inst-env "$(GUILE)"			\
	   $(top_srcdir)/build-aux/update-guix-package.scm	\
	   "`git rev-parse HEAD`"
	git add $(top_srcdir)/gnu/packages/package-management.scm
	git commit -m "gnu: guix: Update to `git rev-parse HEAD | cut -c1-7`."
	for system in $(GUIXSD_SUPPORTED_SYSTEMS) ; do					\
	  image=`$(top_builddir)/pre-inst-env						\
	    guix system disk-image							\
	    --file-system-type=iso9660							\
            --system=$$system								\
	    gnu/system/install.scm` ;							\
	  if [ ! -f "$$image" ] ; then							\
	    echo "failed to produced GuixSD installation image for $$system" >&2 ;	\
	    exit 1 ;									\
	  fi ;										\
	  xz < "$$image" > "$(releasedir)/$(GUIXSD_IMAGE_BASE).$$system.iso.xz.tmp" ;	\
	  mv "$(releasedir)/$(GUIXSD_IMAGE_BASE).$$system.iso.xz.tmp"			\
	     "$(releasedir)/$(GUIXSD_IMAGE_BASE).$$system.iso.xz" ;			\
	done
	for system in $(GUIXSD_VM_SYSTEMS) ; do						\
	  image=`$(top_builddir)/pre-inst-env						\
	    guix system vm-image							\
	    --system=$$system								\
	    gnu/system/examples/vm-image.tmpl` ;					\
	  if [ ! -f "$$image" ] ; then							\
	    echo "failed to produced GuixSD VM image for $$system" >&2 ;		\
	    exit 1 ;									\
	  fi ;										\
	  xz < "$$image" > "$(releasedir)/$(GUIXSD_VM_IMAGE_BASE).$$system.xz.tmp" ;	\
	  mv "$(releasedir)/$(GUIXSD_VM_IMAGE_BASE).$$system.xz.tmp"			\
	     "$(releasedir)/$(GUIXSD_VM_IMAGE_BASE).$$system.xz" ;			\
	done
	@echo
	@echo "Congratulations!  All the release files are now in $(releasedir)."
	@echo

update-guix-package:
	git rev-parse HEAD
	$(top_builddir)/pre-inst-env "$(GUILE)"			\
	   $(top_srcdir)/build-aux/update-guix-package.scm	\
	   "`git rev-parse HEAD`"

# Location of a checkout of <git://git.savannah.gnu.org/guix/maintenance.git>.
# Package data from this checkout is used by 'update-NEWS.scm'.
GUIX_MAINTENANCE_DIRECTORY ?= $(top_srcdir)/../guix-maintenance

update-NEWS: $(GOBJECTS)
	$(top_builddir)/pre-inst-env "$(GUILE)"				\
	  $(top_srcdir)/build-aux/update-NEWS.scm			\
	  $(top_srcdir)/NEWS "$(GUIX_MAINTENANCE_DIRECTORY)/data"

# Make sure we're not shipping a file that embeds a local /gnu/store file name.
assert-no-store-file-names: $(distdir)/ChangeLog
	$(AM_V_at)if grep -r --exclude=*.texi --exclude=*.info			\
	     --exclude=*.info-[0-9] --exclude=*.dot				\
	     --exclude=*.eps --exclude-dir=bootstrap				\
		 --exclude=guix-manual.pot --exclude=guix-manual.*.po		\
	     --exclude=guix-prettify.el						\
	     --exclude=ChangeLog						\
	     -E "$(storedir)/[a-z0-9]{32}-" $(distdir) ;			\
	then									\
	  echo "error: store file names embedded in the distribution" >&2 ;	\
	  exit 1 ;								\
	fi

# Make sure important substitutes are available.
assert-binaries-available: $(GOBJECTS)
	$(AM_V_at)$(top_builddir)/pre-inst-env "$(GUILE)"			\
	  "$(top_srcdir)/build-aux/check-available-binaries.scm"

# Make sure the final inputs don't refer to bootstrap tools.
assert-final-inputs-self-contained: $(GOBJECTS)
	$(AM_V_at)$(top_builddir)/pre-inst-env "$(GUILE)"			\
	  "$(top_srcdir)/build-aux/check-final-inputs-self-contained.scm"

# Compute the Hydra jobs and write them in the target file.
hydra-jobs.scm: $(GOBJECTS)
	$(AM_V_at)$(MKDIR_P) "`dirname "$@"`"
	$(AM_V_GEN)$(top_builddir)/pre-inst-env "$(GUILE)"		\
	  "$(top_srcdir)/build-aux/hydra/evaluate.scm"			\
	  "$(top_srcdir)/build-aux/hydra/gnu-system.scm" > "$@.tmp"
	$(AM_V_at)mv "$@.tmp" "$@"

# Compute the Cuirass jobs and write them in the target file.
cuirass-jobs.scm: $(GOBJECTS)
	$(AM_V_at)$(MKDIR_P) "`dirname "$@"`"
	$(AM_V_GEN)$(top_builddir)/pre-inst-env "$(GUILE)"		\
	  "$(top_srcdir)/build-aux/hydra/evaluate.scm"			\
	  "$(top_srcdir)/build-aux/cuirass/gnu-system.scm" 		\
	  cuirass > "$@.tmp"
	$(AM_V_at)mv "$@.tmp" "$@"

.PHONY: gen-ChangeLog gen-AUTHORS gen-tarball-version
.PHONY: assert-no-store-file-names assert-binaries-available
.PHONY: assert-final-inputs-self-contained
.PHONY: clean-go make-go as-derivation
.PHONY: update-guix-package update-NEWS release

## -------------- ##
## Silent rules.  ##
## -------------- ##

AM_V_DL = $(AM_V_DL_$(V))
AM_V_DL_ = $(AM_V_DL_$(AM_DEFAULT_VERBOSITY))
AM_V_DL_0 = @echo "  DL      " $@;

AM_V_DOT = $(AM_V_DOT_$(V))
AM_V_DOT_ = $(AM_V_DOT_$(AM_DEFAULT_VERBOSITY))
AM_V_DOT_0 = @echo "  DOT     " $@;

AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;

AM_V_PO4A = $(AM_V_PO4A_$(V))
AM_V_PO4A_ = $(AM_V_PO4A_$(AM_DEFAULT_VERBOSITY))
AM_V_PO4A_0 = @echo "  PO4A" $@;

AM_V_POXREF = $(AM_V_POXREF_$(V))
AM_V_POXREF_ = $(AM_V_POXREF_$(AM_DEFAULT_VERBOSITY))
AM_V_POXREF_0 = @echo "  POXREF" $@;
