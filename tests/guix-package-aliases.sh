# GNU Guix --- Functional package management for GNU
# Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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
# Test the `guix package' aliases.
#

guix install --version

readlink_base ()
{
    basename `readlink "$1"`
}

profile="t-profile-$$"
rm -f "$profile"

trap 'rm -f "$profile" "$profile-"[0-9]*' EXIT

guix install --bootstrap guile-bootstrap -p "$profile"
test -x "$profile/bin/guile"

# Make sure '-r' isn't passed as-is to 'guix package'.
if guix install -r guile-bootstrap -p "$profile" --bootstrap
then false; else true; fi
test -x "$profile/bin/guile"

guix upgrade --version
guix upgrade -n
guix upgrade gui.e -n
if guix upgrade foo bar -n;
then false; else true; fi

guix remove --version
guix remove --bootstrap guile-bootstrap -p "$profile"
! test -x "$profile/bin/guile"
test `guix package -p "$profile" -I | wc -l` -eq 0

if guix remove -p "$profile" this-is-not-installed --bootstrap
then false; else true; fi

if guix remove -i guile-bootstrap -p "$profile" --bootstrap
then false; else true; fi
