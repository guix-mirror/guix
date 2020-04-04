# GNU Guix --- Functional package management for GNU
# Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
# Test 'guix build --with-branch'.
#

guix build --version

# 'guix build --with-branch' requires access to the network to clone the
# Git repository below.

if ! guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    # Skipping.
    exit 77
fi

orig_drv="`guix build guile-gcrypt -d`"
latest_drv="`guix build guile-gcrypt --with-branch=guile-gcrypt=master -d`"
test -n "$latest_drv"
test "$orig_drv" != "$latest_drv"

# FIXME: '-S' currently doesn't work with non-derivation source.
# checkout="`guix build guile-gcrypt --with-branch=guile-gcrypt=master -S`"
checkout="`guix gc --references "$latest_drv" | grep guile-gcrypt | grep -v -E '(-builder|\.drv)'`"
test -d "$checkout"
test -f "$checkout/COPYING"

orig_drv="`guix build guix -d`"
latest_drv="`guix build guix --with-branch=guile-gcrypt=master -d`"
guix gc -R "$latest_drv" | grep guile-gcrypt-git.master
test "$orig_drv" != "$latest_drv"

v0_1_0_drv="`guix build guix --with-commit=guile-gcrypt=9e3eacdec1d -d`"
guix gc -R "$v0_1_0_drv" | grep guile-gcrypt-git.9e3eacd
test "$v0_1_0_drv" != "$latest_drv"
test "$v0_1_0_drv" != "$orig_drv"

v0_1_0_drv="`guix build guix --with-commit=guile-gcrypt=v0.1.0 -d`"
guix gc -R "$v0_1_0_drv" | grep guile-gcrypt-0.1.0
guix gc -R "$v0_1_0_drv" | grep guile-gcrypt-8fe64e8 # this is the tag ID
test "$v0_1_0_drv" != "$latest_drv"
test "$v0_1_0_drv" != "$orig_drv"

if guix build guix --with-commit=guile-gcrypt=000 -d
then false; else true; fi
