# GNU Guix --- Functional package management for GNU
# Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
# Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

DOC_PO_FILES =					\
  %D%/guix-manual.es.po				\
  %D%/guix-manual.de.po				\
  %D%/guix-manual.fa.po				\
  %D%/guix-manual.fr.po				\
  %D%/guix-manual.it.po				\
  %D%/guix-manual.ko.po				\
  %D%/guix-manual.pt_BR.po			\
  %D%/guix-manual.ru.po				\
  %D%/guix-manual.sk.po				\
  %D%/guix-manual.zh_CN.po

DOC_COOKBOOK_PO_FILES =				\
  %D%/guix-cookbook.de.po			\
  %D%/guix-cookbook.fa.po			\
  %D%/guix-cookbook.fr.po			\
  %D%/guix-cookbook.ko.po			\
  %D%/guix-cookbook.zh_Hans.po

EXTRA_DIST = \
  %D%/guix-manual.pot \
  %D%/guix-cookbook.pot \
  $(DOC_PO_FILES) \
  $(DOC_COOKBOOK_PO_FILES)

POT_OPTIONS = \
	--package-name "guix manual" --package-version "$(VERSION)" 	\
	--copyright-holder "the authors of Guix (msgids)" 		\
	--msgid-bugs-address "bug-guix@gnu.org"

# make-update-po-files-rule PO_FILES POT_FILE_INPUT
define make-update-po-files-rule
$(1): $(2)
	@if ! [ -f "$$@" ]; then \
	    echo "File $$po_file does not exist.  If you are a translator, \
you can create it with 'msginit'." 1>&2; \
	    exit 1; \
	fi && \
	lang=$$$$(echo $$@ | $(SED) -E 's|.*\.([^.]*)\.po$$$$|\1|') && \
	echo $(MSGMERGE_UPDATE) $(MSGMERGE_OPTIONS) \
	  --lang=$$$${lang} "$$@" "$$<" && \
	$(MSGMERGE_UPDATE) $(MSGMERGE_OPTIONS) --lang=$$$${lang} "$$@" "$$<"
endef

%D%/%.pot: $(srcdir)/doc/%.texi
	$(AM_V_PO4A)$(PO4A_UPDATEPO) -M UTF-8 -f texinfo -m "$<" \
	   -p "$@" $(POT_OPTIONS) && \
	touch $@

%D%/guix-manual.pot: %D%/guix.pot %D%/contributing.pot
	msgcat $^ > $@

$(eval $(call make-update-po-files-rule,$(DOC_PO_FILES),%D%/guix-manual.pot))
$(eval $(call make-update-po-files-rule,\
  $(DOC_COOKBOOK_PO_FILES),%D%/guix-cookbook.pot))

doc-pot-update: %D%/guix-manual.pot %D%/guix-cookbook.pot
doc-po-update: $(DOC_PO_FILES) $(DOC_COOKBOOK_PO_FILES)
.PHONY: doc-pot-update doc-po-update
