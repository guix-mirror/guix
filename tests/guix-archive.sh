# GNU Guix --- Functional package management for GNU
# Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

#
# Test the 'guix archive' command-line utility.
#

guix archive --version

archive="t-archive-$$"
archive_alt="t-archive-alt-$$"
rm -f "$archive" "$archive_alt"

trap 'rm -f "$archive" "$archive_alt"' EXIT

guix archive --export guile-bootstrap > "$archive"
guix archive --export guile-bootstrap:out > "$archive_alt"
cmp "$archive" "$archive_alt"

guix archive --export							\
    -e '(@ (gnu packages bootstrap) %bootstrap-guile)' > "$archive_alt"
cmp "$archive" "$archive_alt"

guix archive --export `guix build guile-bootstrap` > "$archive_alt"
cmp "$archive" "$archive_alt"

guix archive --import < "$archive" 2>&1 | grep "import.*guile-bootstrap"

if guix archive something-that-does-not-exist
then false; else true; fi

if echo foo | guix archive --authorize
then false; else true; fi
