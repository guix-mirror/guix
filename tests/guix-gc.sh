# GNU Guix --- Functional package management for GNU
# Copyright © 2013, 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
# Test the `guix gc' command-line utility.
#

guix gc --version

trap "rm -f guix-gc-root" EXIT
rm -f guix-gc-root

# Below we are using 'drv' and 'out' to contain store file names.  If 'drv'
# and 'out' are environment variables, 'list-runtime-roots' will "see" them
# and thus prevent $drv and $out from being garbage-collected.  Using 'unset'
# allows us to make sure these are truly local shell variables and not
# environments variables.
unset drv
unset out

# For some operations, passing extra arguments is an error.
for option in "" "-C 500M" "--verify" "--optimize" "--list-roots"
do
    if guix gc $option whatever; then false; else true; fi
done

# This should fail.
if guix gc --verify=foo; then false; else true; fi

# Check the references of a .drv.
drv="`guix build guile-bootstrap -d`"
out="`guix build guile-bootstrap`"
test -f "$drv" && test -d "$out"

guix gc --references "$drv" | grep -e -bash
guix gc --references "$out"
guix gc --references "$out/bin/guile"

if guix gc --references /dev/null;
then false; else true; fi

# Check derivers.
guix gc --derivers "$out" | grep "$drv"

# Add then reclaim a .drv file.
drv="`guix build idutils -d`"
test -f "$drv"

guix gc --list-dead | grep "$drv"
guix gc --delete "$drv"
! test -f "$drv"

# Add a .drv, register it as a root.
drv="`guix build --root=guix-gc-root lsh -d`"
test -f "$drv" && test -L guix-gc-root

guix gc --list-roots | grep "$PWD/guix-gc-root"

guix gc --list-live | grep "$drv"
if guix gc --delete "$drv";
then false; else true; fi

rm guix-gc-root
guix gc --list-dead | grep "$drv"
guix gc --delete "$drv"
! test -f "$drv"

# Try a random collection.
guix gc -C 1KiB

# Check trivial error cases.
if guix gc --delete /dev/null;
then false; else true; fi

# Bug #19757
out="`guix build guile-bootstrap`"
test -d "$out"

guix gc --delete "$out"

! test -d "$out"

out="`guix build guile-bootstrap`"
test -d "$out"

guix gc --delete "$out/"

! test -d "$out"

out="`guix build guile-bootstrap`"
test -d "$out"

guix gc --delete "$out/bin/guile"
