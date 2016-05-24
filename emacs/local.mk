# GNU Guix --- Functional package management for GNU
# Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
# Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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

AUTOLOADS = %D%/guix-autoloads.el

ELFILES =					\
  %D%/guix-about.el				\
  %D%/guix-backend.el				\
  %D%/guix-base.el				\
  %D%/guix-build-log.el				\
  %D%/guix-buffer.el				\
  %D%/guix-command.el				\
  %D%/guix-devel.el				\
  %D%/guix-emacs.el				\
  %D%/guix-entry.el				\
  %D%/guix-external.el				\
  %D%/guix-geiser.el				\
  %D%/guix-guile.el				\
  %D%/guix-help-vars.el				\
  %D%/guix-history.el				\
  %D%/guix-hydra.el				\
  %D%/guix-hydra-build.el			\
  %D%/guix-hydra-jobset.el			\
  %D%/guix-info.el				\
  %D%/guix-init.el				\
  %D%/guix-license.el				\
  %D%/guix-list.el				\
  %D%/guix-location.el				\
  %D%/guix-messages.el				\
  %D%/guix-pcomplete.el				\
  %D%/guix-popup.el				\
  %D%/guix-prettify.el				\
  %D%/guix-profiles.el				\
  %D%/guix-read.el				\
  %D%/guix-ui.el				\
  %D%/guix-ui-license.el			\
  %D%/guix-ui-location.el			\
  %D%/guix-ui-package.el			\
  %D%/guix-ui-generation.el			\
  %D%/guix-ui-system-generation.el		\
  %D%/guix-utils.el

if HAVE_EMACS

dist_lisp_DATA = $(ELFILES)

nodist_lisp_DATA = 		\
  %D%/guix-config.el		\
  $(AUTOLOADS)

$(AUTOLOADS): $(ELFILES)
	$(AM_V_EMACS)$(EMACS) --batch --eval				\
	  "(let ((backup-inhibited t)					\
	         (generated-autoload-file				\
	          (expand-file-name \"$(AUTOLOADS)\" \"$(builddir)\")))	\
	     (update-directory-autoloads				\
	      (expand-file-name \"emacs\" \"$(srcdir)\")))"

CLEANFILES += $(AUTOLOADS)

endif HAVE_EMACS
