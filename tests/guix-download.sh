# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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
# Test the `guix download' command-line utility.
#

guix download --version

# Make sure it fails here.
! guix download http://does.not/exist

! guix download unknown://some/where;

! guix download /does-not-exist

# This one should succeed.
guix download "file://$abs_top_srcdir/README"

# And this one, without the URI scheme.
guix download "$abs_top_srcdir/README"

# This one too, even if it cannot talk to the daemon.
output="t-download-$$"
trap 'rm -f "$output"' EXIT
GUIX_DAEMON_SOCKET="/nowhere" guix download -o "$output" \
		  "file://$abs_top_srcdir/README"
cmp "$output" "$abs_top_srcdir/README"

# This one should fail.
! guix download "file:///does-not-exist" "file://$abs_top_srcdir/README"
