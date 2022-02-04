# GNU Guix --- Functional package management for GNU
# Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
# Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2013 Andreas Enge <andreas@enge.fr>
# Copyright © 2016 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
# Copyright © 2016, 2018 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2018, 2021 Julien Lepiller <julien@lepiller.eu>
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

# If adding a language, update the following variables, and info_TEXINFOS.
MANUAL_LANGUAGES = de es fa fi fr it ko pt_BR ru sk zh_CN
COOKBOOK_LANGUAGES = de es fa fi fr ko pt_BR ru sk zh_Hans

# Arg1: A list of languages codes.
# Arg2: The file name stem.
lang_to_texinfo = $(foreach lang,$(1),%D%/$(2).$(lang).texi)

# Automake does not understand GNU Make non-standard extensions,
# unfortunately, so we cannot use the above patsubst-based function here.
info_TEXINFOS = %D%/guix.texi			\
  %D%/guix.de.texi				\
  %D%/guix.es.texi				\
  %D%/guix.fa.texi				\
  %D%/guix.fi.texi				\
  %D%/guix.fr.texi				\
  %D%/guix.it.texi				\
  %D%/guix.ko.texi				\
  %D%/guix.pt_BR.texi				\
  %D%/guix.ru.texi				\
  %D%/guix.sk.texi				\
  %D%/guix.zh_CN.texi				\
  %D%/guix-cookbook.texi			\
  %D%/guix-cookbook.de.texi			\
  %D%/guix-cookbook.es.texi			\
  %D%/guix-cookbook.fa.texi			\
  %D%/guix-cookbook.fi.texi			\
  %D%/guix-cookbook.fr.texi			\
  %D%/guix-cookbook.ko.texi			\
  %D%/guix-cookbook.pt_BR.texi			\
  %D%/guix-cookbook.ru.texi			\
  %D%/guix-cookbook.sk.texi			\
  %D%/guix-cookbook.zh_Hans.texi

%C%_guix_TEXINFOS = \
  %D%/contributing.texi \
  %D%/fdl-1.3.texi

DOT_FILES =					\
  %D%/images/bootstrap-graph.dot		\
  %D%/images/bootstrap-packages.dot		\
  %D%/images/coreutils-graph.dot		\
  %D%/images/coreutils-bag-graph.dot		\
  %D%/images/gcc-core-mesboot0-graph.dot	\
  %D%/images/service-graph.dot			\
  %D%/images/shepherd-graph.dot

DOT_VECTOR_GRAPHICS =				\
  $(DOT_FILES:%.dot=%.eps)			\
  $(DOT_FILES:%.dot=%.pdf)

EXTRA_DIST +=					\
  %D%/htmlxref.cnf				\
  $(DOT_FILES)					\
  $(DOT_VECTOR_GRAPHICS)			\
  %D%/images/coreutils-size-map.eps		\
  %D%/environment-gdb.scm			\
  %D%/package-hello.scm				\
  %D%/package-hello.json

OS_CONFIG_EXAMPLES_TEXI =			\
  %D%/os-config-bare-bones.texi			\
  %D%/os-config-desktop.texi			\
  %D%/os-config-lightweight-desktop.texi	\
  %D%/he-config-bare-bones.scm

TRANSLATED_INFO = 						\
  $(call lang_to_texinfo,$(MANUAL_LANGUAGES),guix)		\
  $(call lang_to_texinfo,$(MANUAL_LANGUAGES),contributing)	\
  $(call lang_to_texinfo,$(COOKBOOK_LANGUAGES),guix-cookbook)

# Bundle this file so that makeinfo finds it in out-of-source-tree builds.
BUILT_SOURCES        += $(OS_CONFIG_EXAMPLES_TEXI) $(TRANSLATED_INFO)
EXTRA_DIST           += $(OS_CONFIG_EXAMPLES_TEXI) $(TRANSLATED_INFO)
MAINTAINERCLEANFILES  = $(OS_CONFIG_EXAMPLES_TEXI) $(TRANSLATED_INFO)

PO4A_PARAMS := -M UTF-8 -L UTF-8 #master and localized encoding
PO4A_PARAMS += -k 0 # produce an output even if the translation is not complete
PO4A_PARAMS += -f texinfo # texinfo format

# When a change to guix.texi occurs, it is not translated immediately.
# Because @pxref and @xref commands are references to sections by name, they
# should be translated. If a modification adds a reference to a section, this
# reference is not translated, which means it references a section that does not
# exist.
define xref_command
$(top_srcdir)/pre-inst-env $(GUILE) --no-auto-compile	\
  "$(top_srcdir)/build-aux/convert-xref.scm"		\
  $@.tmp $<
endef

$(srcdir)/%D%/guix.%.texi: po/doc/guix-manual.%.po $(srcdir)/%D%/contributing.%.texi guix/build/po.go
	-$(AM_V_PO4A)$(PO4A_TRANSLATE) $(PO4A_PARAMS) -m "%D%/guix.texi" -p "$<" -l "$@.tmp"
	-sed -i "s|guix\.info|$$(basename "$@" | sed 's|texi$$|info|')|" "$@.tmp"
	-$(AM_V_POXREF)LC_ALL=en_US.UTF-8 $(xref_command)
	-mv "$@.tmp" "$@"

$(srcdir)/%D%/guix-cookbook.%.texi: po/doc/guix-cookbook.%.po guix/build/po.go
	-$(AM_V_PO4A)$(PO4A_TRANSLATE) $(PO4A_PARAMS) -m "%D%/guix-cookbook.texi" -p "$<" -l "$@.tmp"
	-sed -i "s|guix-cookbook\.info|$$(basename "$@" | sed 's|texi$$|info|')|" "$@.tmp"
	-$(AM_V_POXREF)LC_ALL=en_US.UTF-8 $(xref_command)
	-mv "$@.tmp" "$@"

$(srcdir)/%D%/contributing.%.texi: po/doc/guix-manual.%.po guix/build/po.go
	-$(AM_V_PO4A)$(PO4A_TRANSLATE) $(PO4A_PARAMS) -m "%D%/contributing.texi" -p "$<" -l "$@.tmp"
	-$(AM_V_POXREF)LC_ALL=en_US.UTF-8 $(xref_command)
	-mv "$@.tmp" "$@"

%D%/os-config-%.texi: gnu/system/examples/%.tmpl
	$(AM_V_GEN)$(MKDIR_P) "`dirname $@`";	\
	cp "$<" "$@"

infoimagedir = $(infodir)/images
dist_infoimage_DATA =				\
  $(DOT_FILES:%.dot=%.png)			\
  %D%/images/coreutils-size-map.png		\
  %D%/images/installer-network.png		\
  %D%/images/installer-partitions.png		\
  %D%/images/installer-resume.png

# Ask for warnings about cross-referenced manuals that are not listed in
# htmlxref.cnf.
AM_MAKEINFOHTMLFLAGS = --set-customization-variable CHECK_HTMLXREF=true

# Try hard to obtain an image size and aspect that's reasonable for inclusion
# in an Info or PDF document.
DOT_OPTIONS =					\
  -Gratio=.9 -Gnodesep=.005 -Granksep=.00005	\
  -Nfontsize=9 -Nheight=.1 -Nwidth=.1

.dot.png:
	$(AM_V_DOT)$(DOT) -Tpng $(DOT_OPTIONS) < "$<" > "$(srcdir)/$@.tmp"; \
	mv "$(srcdir)/$@.tmp" "$(srcdir)/$@"

.dot.pdf:
	$(AM_V_DOT)$(DOT) -Tpdf $(DOT_OPTIONS) < "$<" > "$(srcdir)/$@.tmp"; \
	mv "$(srcdir)/$@.tmp" "$(srcdir)/$@"

.dot.eps:
	$(AM_V_DOT)$(DOT) -Teps $(DOT_OPTIONS) < "$<" > "$(srcdir)/$@.tmp"; \
	mv "$(srcdir)/$@.tmp" "$(srcdir)/$@"

.png.eps:
	$(AM_V_GEN)convert "$<" "$@-tmp.eps"; \
	mv "$@-tmp.eps" "$@"

# We cannot add new dependencies to `%D%/guix.pdf' & co. (info "(automake)
# Extending").  Using the `-local' rules is imperfect, because they may be
# triggered after the main rule.  Oh, well.
pdf-local: $(DOT_FILES=%.dot=$(top_srcdir)/%.pdf)
info-local: $(DOT_FILES=%.dot=$(top_srcdir)/%.png)
ps-local: $(DOT_FILES=%.dot=$(top_srcdir)/%.eps)		\
	  $(top_srcdir)/%D%/images/coreutils-size-map.eps
dvi-local: ps-local

## ----------- ##
##  Man pages. ##
## ----------- ##

# The man pages are generated using GNU Help2man.  In makefiles rules they
# depend not on the binary, but on the source files.  This usage allows a
# manual page to be generated by the maintainer and included in the
# distribution without requiring the end-user to have 'help2man' installed.
# They are built in $(srcdir) like info manuals.

sub_commands_mans =				\
  $(srcdir)/%D%/guix-archive.1			\
  $(srcdir)/%D%/guix-build.1			\
  $(srcdir)/%D%/guix-challenge.1		\
  $(srcdir)/%D%/guix-deploy.1			\
  $(srcdir)/%D%/guix-download.1			\
  $(srcdir)/%D%/guix-edit.1			\
  $(srcdir)/%D%/guix-environment.1		\
  $(srcdir)/%D%/guix-gc.1			\
  $(srcdir)/%D%/guix-hash.1			\
  $(srcdir)/%D%/guix-import.1			\
  $(srcdir)/%D%/guix-lint.1			\
  $(srcdir)/%D%/guix-package.1			\
  $(srcdir)/%D%/guix-publish.1			\
  $(srcdir)/%D%/guix-pull.1			\
  $(srcdir)/%D%/guix-refresh.1			\
  $(srcdir)/%D%/guix-size.1			\
  $(srcdir)/%D%/guix-system.1			\
  $(srcdir)/%D%/guix-time-machine.1		\
  $(srcdir)/%D%/guix-weather.1

# Assume that cross-compiled commands cannot be executed.
if !CROSS_COMPILING

dist_man1_MANS =				\
  $(srcdir)/%D%/guix.1				\
  $(sub_commands_mans)

endif

gen_man =						\
  LANGUAGE= $(top_builddir)/pre-inst-env $(HELP2MAN)	\
  $(HELP2MANFLAGS)

HELP2MANFLAGS = --source=GNU --info-page=$(PACKAGE_TARNAME)

$(srcdir)/%D%/guix.1: scripts/guix.in $(sub_commands_mans)
	-$(AM_V_HELP2MAN)$(gen_man) --output="$@" `basename "$@" .1`

# The 'case' ensures the man pages are only generated if the corresponding
# source script (the first prerequisite) has been changed.  The $(GOBJECTS)
# prerequisite is solely meant to force these docs to be made only after all
# Guile modules have been compiled.
$(srcdir)/%D%/guix-%.1: guix/scripts/%.scm $(GOBJECTS)
	-@case '$?' in \
	  *$<*) $(AM_V_P) && set -x || echo "  HELP2MAN $@"; \
	        $(gen_man) --output="$@" "guix $*";; \
	  *)    : ;; \
	esac

if BUILD_DAEMON
if !CROSS_COMPILING

dist_man1_MANS += $(srcdir)/%D%/guix-daemon.1

$(srcdir)/%D%/guix-daemon.1: guix-daemon$(EXEEXT)
	-$(AM_V_HELP2MAN)$(gen_man) --output="$@" `basename "$@" .1`

endif
endif
