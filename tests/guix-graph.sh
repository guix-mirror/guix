# GNU Guix --- Functional package management for GNU
# Copyright © 2015, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix graph' command-line utility.
#

tmpfile1="t-guix-graph1-$$"
tmpfile2="t-guix-graph2-$$"
trap 'rm -f "$tmpfile1" "$tmpfile2"' EXIT

guix graph --version

for package in guile-bootstrap coreutils python
do
    for graph in package bag-emerged bag bag-with-origins
    do
	guix graph -t "$graph" "$package" | grep "$package"
    done
done

guix build guile-bootstrap
guix graph -t references guile-bootstrap | grep guile-bootstrap

guix graph -e '(@ (gnu packages bootstrap) %bootstrap-guile)' \
    | grep guile-bootstrap

if guix graph -e +; then false; else true; fi

# Try passing store file names.

guix graph -t references guile-bootstrap > "$tmpfile1"
guix graph -t references `guix build guile-bootstrap` > "$tmpfile2"
cmp "$tmpfile1" "$tmpfile2"

# XXX: Filter the file names in the graph to work around the fact that we get
# a mixture of relative and absolute file names.
guix graph -t derivation coreutils > "$tmpfile1"
guix graph -t derivation `guix build -d coreutils` > "$tmpfile2"
cmp "$tmpfile1" "$tmpfile2"

# Try package transformation options.
guix graph git | grep 'label = "openssl'
guix graph git --with-input=openssl=libressl | grep 'label = "libressl'
if guix graph git --with-input=openssl=libressl | grep 'label = "openssl'
then false; else true; fi
