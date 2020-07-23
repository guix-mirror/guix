# GNU Guix --- Functional package management for GNU
# Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix git authenticate' command-line utility.
#

# Skip if we're not in a Git checkout.
[ -d "$abs_top_srcdir/.git" ] || exit 77

# Skip if there's no 'keyring' branch.
guile -c '(use-modules (git))
  (member "refs/heads/keyring" (branch-list (repository-open ".")))' || \
    exit 77

# Keep in sync with '%default-channels' in (guix channels)!
intro_commit="9edb3f66fd807b096b48283debdcddccfea34bad"
intro_signer="BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"

cache_key="test-$$"

guix git authenticate "$intro_commit" "$intro_signer"	\
     --cache-key="$cache_key" --stats			\
     --end=9549f0283a78fe36f2d4ff2a04ef8ad6b0c02604

rm "$XDG_CACHE_HOME/guix/authentication/$cache_key"

# Commit and signer of the 'v1.0.0' tag.
v1_0_0_commit="6298c3ffd9654d3231a6f25390b056483e8f407c"
v1_0_0_signer="3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5" # civodul
v1_0_1_commit="d68de958b60426798ed62797ff7c96c327a672ac"

# This should fail because these commits lack '.guix-authorizations'.
if guix git authenticate "$v1_0_0_commit" "$v1_0_0_signer" \
	--cache-key="$cache_key" --end="$v1_0_1_commit";
then false; else true; fi

# This should work thanks to '--historical-authorizations'.
guix git authenticate "$v1_0_0_commit" "$v1_0_0_signer" 	\
     --cache-key="$cache_key" --end="$v1_0_1_commit" --stats	\
     --historical-authorizations="$abs_top_srcdir/etc/historical-authorizations"
