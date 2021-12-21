# GNU Guix --- Functional package management for GNU
# Copyright © 2013, 2014, 2020 Ludovic Courtès <ludo@gnu.org>
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

key="$abs_top_srcdir/tests/keys/signing-key.sec"
key_len="`echo -n $key | wc -c`"

# A hexadecimal string as long as a sha256 hash.
hash="2749f0ea9f26c6c7be746a9cff8fa4c2f2a02b000070dba78429e9a11f87c6eb"
hash_len="`echo -n $hash | wc -c`"

echo "sign $key_len:$key $hash_len:$hash" | guix authenticate > "$sig"
test -f "$sig"
case "$(cat $sig)" in
    "0 "*) ;;
    *)     echo "broken signature: $(cat $sig)"
	   exit 42;;
esac

# Remove the leading "0".
sed -i "$sig" -e's/^0 //g'

hash2="$(echo verify $(cat "$sig") | guix authenticate)"
test "$(echo $hash2 | cut -d : -f 2)" = "$hash"

# Detect corrupt signatures.
code="$(echo "verify 5:wrong" | guix authenticate | cut -f1 -d ' ')"
test "$code" -ne 0

# Detect invalid signatures.
# The signature has (payload (data ... (hash sha256 #...#))).  We proceed by
# modifying this hash.
sed -i "$sig"											\
    -e's|#[A-Z0-9]\{64\}#|#0000000000000000000000000000000000000000000000000000000000000000#|g'
code="$(echo "verify $(cat $sig)" | guix authenticate | cut -f1 -d ' ')"
test "$code" -ne 0

# Make sure byte strings are correctly encoded.  The hash string below is
# "café" repeated 8 times.  Libgcrypt would normally choose to write it as a
# string rather than a hex sequence.  We want that string to be Latin-1
# encoded independently of the current locale: <https://bugs.gnu.org/43421>.
hash="636166e9636166e9636166e9636166e9636166e9636166e9636166e9636166e9"
latin1_cafe="caf$(printf '\351')"
echo "sign 26:tests/keys/signing-key.sec 64:$hash" | guix authenticate \
    | LC_ALL=C grep "hash sha256 \"$latin1_cafe"

# Test for <http://bugs.gnu.org/17312>: make sure 'guix authenticate' produces
# valid signatures when run in the C locale.
hash="5eff0b55c9c5f5e87b4e34cd60a2d5654ca1eb78c7b3c67c3179fed1cff07b4c"

LC_ALL=C
export LC_ALL

echo "sign $key_len:$key $hash_len:$hash" | guix authenticate > "$sig"

# Remove the leading "0".
sed -i "$sig" -e's/^0 //g'

echo "verify $(cat $sig)" | guix authenticate
hash2="$(echo "verify $(cat $sig)" | guix authenticate | cut -f2 -d ' ')"
test "$(echo $hash2 | cut -d : -f 2)" = "$hash"
