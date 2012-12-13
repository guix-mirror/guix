# Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
# Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of Guix.
#
# Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guix.  If not, see <http://www.gnu.org/licenses/>.

#
# Test the `guix-package' command-line utility.
#

guix-package --version

profile="t-profile-$$"
rm -f "$profile"

guix-package -b -p "$profile"						\
    -i `guix-build -e '(@@ (distro packages base) %bootstrap-guile)'`
test -L "$profile" && test -L "$profile-1-link"
test -f "$profile/bin/guile"


# Check whether we have network access.
if guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    guix-package -b -p "$profile"						\
	-i `guix-build -e '(@@ (distro packages base) gnu-make-boot0)'`
    test -L "$profile-2-link"
    test -f "$profile/bin/make" && test -f "$profile/bin/guile"


    # Check whether `--list-installed' works.
    # XXX: Change the tests when `--install' properly extracts the package
    # name and version string.
    installed="`guix-package -p "$profile" --list-installed | cut -f1 | xargs echo | sort`"
    case "x$installed" in
	"guile-bootstrap make-boot0")
	    true;;
	"make-boot0 guile-bootstrap")
	    true;;
	"*")
            false;;
    esac

    test "`guix-package -p "$profile" -I 'g.*e' | cut -f1`" = "guile-bootstrap"

    # Remove a package.
    guix-package -b -p "$profile" -r "guile-bootstrap-2.0"
    test -L "$profile-3-link"
    test -f "$profile/bin/make" && ! test -f "$profile/bin/guile"
fi

# Make sure the `:' syntax works.
guix-package -b -i "libsigsegv:lib" -n

# Check whether `--list-available' returns something sensible.
guix-package -A 'gui.*e' | grep guile

rm "$profile" "$profile-"[0-9]*
