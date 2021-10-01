# GNU Guix --- Functional package management for GNU
# Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix shell' alias.
#

guix shell --version

tmpdir="t-guix-shell-$$"
trap 'rm -r "$tmpdir"' EXIT
mkdir "$tmpdir"

guix shell --bootstrap --pure guile-bootstrap -- guile --version

# '--ad-hoc' is a thing of the past.
! guix shell --ad-hoc guile-bootstrap

if guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    # Compute the build environment for the initial GNU Make.
    guix shell --bootstrap --no-substitutes --search-paths --pure \
         -D -e '(@ (guix tests) gnu-make-for-tests)' > "$tmpdir/a"

    # Make sure bootstrap binaries are in the profile.
    profile=`grep "^export PATH" "$tmpdir/a" | sed -r 's|^.*="(.*)/bin"|\1|'`

    # Make sure the bootstrap binaries are all listed where they belong.
    grep -E "^export PATH=\"$profile/bin\""         "$tmpdir/a"
    grep -E "^export CPATH=\"$profile/include\""    "$tmpdir/a"
    grep -E "^export LIBRARY_PATH=\"$profile/lib\"" "$tmpdir/a"
    for dep in bootstrap-binaries-0 gcc-bootstrap-0 glibc-bootstrap-0
    do
	guix gc --references "$profile" | grep "$dep"
    done

    # 'make-boot0' itself must not be listed.
    ! guix gc --references "$profile" | grep make-boot0
fi
