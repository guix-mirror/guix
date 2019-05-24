# GNU Guix --- Functional package management for GNU
# Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix pack --localstatedir' command-line utility.
#

guix pack --version

# 'guix pack --localstatedir' produces derivations that depend on
# guile-sqlite3 and guile-gcrypt.  To make that relatively inexpensive, run
# the test in the user's global store if possible, on the grounds that
# binaries may already be there or can be built or downloaded inexpensively.

storedir="`guile -c '(use-modules (guix config))(display %storedir)'`"
localstatedir="`guile -c '(use-modules (guix config))(display %localstatedir)'`"
NIX_STORE_DIR="$storedir"
GUIX_DAEMON_SOCKET="$localstatedir/guix/daemon-socket/socket"
export NIX_STORE_DIR GUIX_DAEMON_SOCKET

if ! guile -c '(use-modules (guix)) (exit (false-if-exception (open-connection)))'
then
    exit 77
fi

# Build a tarball with '--localstatedir'
the_pack="`guix pack -C none --localstatedir --profile-name=current-guix \
            guile-bootstrap`"
test_directory="`mktemp -d`"
trap 'chmod -Rf +w "$test_directory"; rm -rf "$test_directory"' EXIT

cd "$test_directory"
tar -xf "$the_pack"

profile="`find -name current-guix`"
test "`readlink $profile`" = "current-guix-1-link"
test -s "`dirname $profile`/../../../db/db.sqlite"
test -x ".`guix build guile-bootstrap`/bin/guile"
cd -

# Make sure the store database is not completely bogus.
guile -c "(use-modules (sqlite3) (guix config) (ice-9 match))

  (define db
    (sqlite-open (string-append \"$test_directory\"
                                %localstatedir
                               \"/guix/db/db.sqlite\")
                 SQLITE_OPEN_READONLY))

  (define stmt
    (sqlite-prepare db \"SELECT * FROM ValidPaths;\"))

  (match (sqlite-fold cons '() stmt)
    ((#(ids paths hashes times derivers sizes) ...)
     (exit (member \"`guix build guile-bootstrap`\" paths))))"
