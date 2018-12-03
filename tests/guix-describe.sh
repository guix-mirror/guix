# GNU Guix --- Functional package management for GNU
# Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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
# Test 'guix describe'.
#

guix describe --version

tmpfile="t-guix-describe-$$"
trap "rm -f $tmpfile" EXIT
rm -f "$tmpfile"

if [ -d "$abs_top_srcdir/.git" ]
then
    # Since we're in a Git checkout, we can at least check that these things
    # work.
    guix describe | grep -i "checkout"
    if git --version > /dev/null 2>&1
    then
	result="`guix describe | grep commit: | cut -d : -f 2-`"
	commit="`git log | head -1 | cut -c 7-`"
	test "x$result" = "x$commit"
    fi
    guix describe -f channels
    case "`guix describe -f channels | grep url`" in
	*"(url \"$abs_top_srcdir\")") true;;
	*) false;;
    esac
else
    exit 77
fi
