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
# Test the daemon.
#

set -e

NIX_SUBSTITUTERS=""		# don't resort to substituters
NIX_IGNORE_SYMLINK_STORE=1	# in case the store is a symlink
NIX_STORE_DIR="$TEST_ROOT/store"
NIX_LOCALSTATE_DIR="$TEST_ROOT/var"
NIX_LOG_DIR="$TEST_ROOT/var/log/nix"
NIX_STATE_DIR="$TEST_ROOT/var/nix"
NIX_DB_DIR="$TEST_ROOT/db"
export NIX_SUBSTITUTERS NIX_IGNORE_SYMLINK_STORE NIX_STORE_DIR	\
    NIX_LOCALSTATE_DIR NIX_LOG_DIR NIX_STATE_DIR NIX_DB_DIR

guix-daemon --version
guix-build --version

# Launch the daemon without chroot support because is may be
# unavailable, for instance if we're not running as root.
guix-daemon --disable-chroot &

daemon_pid=$!
trap "kill $daemon_pid" EXIT

guix-build -e '(@ (distro packages bootstrap) %bootstrap-guile)'
guix-build coreutils -n
