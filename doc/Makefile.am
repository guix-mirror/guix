# GNU Guix --- Functional package management for GNU
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

info_TEXINFOS = guix.texi
EXTRA_DIST =                                \
  fdl-1.3.texi                              \
  images/bootstrap-graph.dot                \
  images/bootstrap-graph.eps

infoimagedir = $(infodir)/images
dist_infoimage_DATA = images/bootstrap-graph.png

# Try hard to obtain an image size and aspect that's reasonable for inclusion
# in an Info or PDF document.
DOT_OPTIONS =                                           \
  -Tpng -Gratio=.9 -Gnodesep=.005 -Granksep=.00005      \
  -Nfontsize=9 -Nheight=.1 -Nwidth=.1

.dot.png:
	dot -Tpng $(DOT_OPTIONS) < "$<" > "$@.tmp"
	mv "$@.tmp" "$@"

.dot.eps:
	dot -Teps $(DOT_OPTIONS) < "$<" > "$@.tmp"
	mv "$@.tmp" "$@"

guix.pdf: images/bootstrap-graph.png
guix.info: images/bootstrap-graph.png
guix.ps: images/bootstrap-graph.eps

