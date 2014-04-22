# GNU Guix --- Functional package management for GNU
# Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix authenticate' command-line utility.
#

guix authenticate --version

sig="t-signature-$$"
hash="t-hash-$$"
rm -f "$sig" "$hash"

trap 'rm -f "$sig" "$hash"' EXIT

# A hexadecimal string as long as a sha256 hash.
echo "2749f0ea9f26c6c7be746a9cff8fa4c2f2a02b000070dba78429e9a11f87c6eb" \
    > "$hash"

guix authenticate rsautl -sign				\
    -inkey "$abs_top_srcdir/tests/signing-key.sec"	\
    -in "$hash" > "$sig"
test -f "$sig"

hash2="`guix authenticate rsautl -verify		\
          -inkey $abs_top_srcdir/tests/signing-key.pub	\
          -pubin -in $sig`"
test "$hash2" = `cat "$hash"`

# Same thing in a pipeline, using the command line syntax that Nix/Crypto.pm
# uses.
hash2="`						\
  cat "$hash"						\
  | guix authenticate rsautl -sign			\
    -inkey "$abs_top_srcdir/tests/signing-key.sec"	\
  | guix authenticate rsautl -verify			\
          -inkey $abs_top_srcdir/tests/signing-key.pub	\
          -pubin`"
test "$hash2" = `cat "$hash"`

# Detect corrupt signatures.
if guix authenticate rsautl -verify				\
          -inkey "$abs_top_srcdir/tests/signing-key.pub"	\
          -pubin -in /dev/null
then false
else true
fi

# Detect invalid signatures.
# The signature has (payload (data ... (hash sha256 #...#))).  We proceed by
# modifying this hash.
sed -i "$sig"											\
    -e's|#[A-Z0-9]\{64\}#|#0000000000000000000000000000000000000000000000000000000000000000#|g'
if guix authenticate rsautl -verify				\
          -inkey "$abs_top_srcdir/tests/signing-key.pub"	\
          -pubin -in "$sig"
then false
else true
fi


# Test for <http://bugs.gnu.org/17312>: make sure 'guix authenticate' produces
# valid signatures when run in the C locale.
echo "5eff0b55c9c5f5e87b4e34cd60a2d5654ca1eb78c7b3c67c3179fed1cff07b4c" \
    > "$hash"

LC_ALL=C
export LC_ALL

guix authenticate rsautl -sign				\
    -inkey "$abs_top_srcdir/tests/signing-key.sec"	\
    -in "$hash" > "$sig"

guix authenticate rsautl -verify			\
        -inkey "$abs_top_srcdir/tests/signing-key.pub"	\
        -pubin -in "$sig"
hash2="`guix authenticate rsautl -verify		\
          -inkey $abs_top_srcdir/tests/signing-key.pub	\
          -pubin -in $sig`"
test "$hash2" = `cat "$hash"`
