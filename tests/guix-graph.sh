# GNU Guix --- Functional package management for GNU
# Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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
