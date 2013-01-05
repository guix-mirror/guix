# Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
# Copyright (C) 2013 Ludovic Courtès <ludo@gnu.org>
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
# Test the `guix-gc' command-line utility.
#

guix-gc --version

trap "rm -f guix-gc-root" EXIT
rm -f guix-gc-root

# Add then reclaim a .drv file.
drv="`guix-build idutils -d`"
test -f "$drv"

guix-gc --list-dead | grep "$drv"
guix-gc --delete "$drv"
! test -f "$drv"

# Add a .drv, register it as a root.
drv="`guix-build --root=guix-gc-root lsh -d`"
test -f "$drv" && test -L guix-gc-root

guix-gc --list-live | grep "$drv"
if guix-gc --delete "$drv";
then false; else true; fi

rm guix-gc-root
guix-gc --list-dead | grep "$drv"
guix-gc --delete "$drv"
! test -f "$drv"

# Try a random collection.
guix-gc -C 1KiB

# Check trivial error cases.
if guix-gc --delete /dev/null;
then false; else true; fi
