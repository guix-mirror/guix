# GNU Guix --- Functional package management for GNU
# Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix-register' command-line utility.
#

guix-register --version

new_store="t-register-$$"
closure="t-register-closure-$$"
rm -rf "$new_store"

exit_hook=":"
trap "chmod -R +w $new_store ; rm -rf $new_store $closure ; \$exit_hook" EXIT

mkdir -p "$new_store/$storedir"
new_store_dir="`cd "$new_store/$storedir" ; pwd`"
new_store="`cd "$new_store" ; pwd`"

to_copy="`guix build guile-bootstrap`"
cp -r "$to_copy" "$new_store_dir"
copied="$new_store_dir/`basename $to_copy`"

# Create a file representing a closure with zero references, and with an empty
# "deriver" field.
cat >> "$closure" <<EOF
$copied

0
EOF

# Register it.
guix-register -p "$new_store" < "$closure"

# Doing it a second time shouldn't hurt.
guix-register -p "$new_store" "$closure"

# Now make sure this is recognized as valid.

NIX_IGNORE_SYMLINK_STORE=1
NIX_STORE_DIR="$new_store_dir"
NIX_LOCALSTATE_DIR="$new_store$localstatedir"
NIX_LOG_DIR="$new_store$localstatedir/log/nix"
NIX_DB_DIR="$new_store$localstatedir/nix/db"

export NIX_IGNORE_SYMLINK_STORE NIX_STORE_DIR NIX_LOCALSTATE_DIR	\
  NIX_LOG_DIR NIX_DB_DIR

guix-daemon --disable-chroot &
subdaemon_pid=$!
exit_hook="kill $subdaemon_pid"

# At this point the copy in $new_store must be valid, and unreferenced.
guile -c "
   (use-modules (guix store))
   (define s (open-connection))
   (exit (and (valid-path? s \"$copied\")
              (equal? (list \"$copied\") (dead-paths s))))"
