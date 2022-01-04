# GNU Guix --- Functional package management for GNU
# Copyright © 2013, 2014, 2016, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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
# Test the `guix hash' command-line utility.
#

guix hash --version

tmpdir="guix-hash-$$"
trap 'rm -rf "$tmpdir"' EXIT

test `guix hash /dev/null` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `echo -n | guix hash -` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `guix hash -f nix-base32 /dev/null` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `guix hash -f hex /dev/null` = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
test `guix hash -f base32 /dev/null` = 4oymiquy7qobjgx36tejs35zeqt24qpemsnzgtfeswmrw6csxbkq
test `guix hash -H sha512 -f hex /dev/null` = cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e
test `guix hash -H sha1 -f base64 /dev/null` = "2jmj7l5rSw0yVb/vlWAYkK/YBwk="

# Several files.
test "`guix hash /dev/null "$abs_top_srcdir/README"`" = "`guix hash /dev/null ; guix hash "$abs_top_srcdir/README"`"

# Zero files.
! guix hash

# idem as `cat /dev/null | git hash-object --stdin`
test `guix hash -S git -H sha1 -f hex  /dev/null` = e69de29bb2d1d6434b8b29ae775ad8c2e48c5391

! guix hash -H abcd1234 /dev/null

mkdir "$tmpdir"
echo -n executable > "$tmpdir/exe"
chmod +x "$tmpdir/exe"
( cd "$tmpdir" ; ln -s exe symlink )
mkdir "$tmpdir/subdir"

test `guix hash -S nar "$tmpdir"` = 10k1lw41wyrjf9mxydi0is5nkpynlsvgslinics4ppir13g7d74p
test `guix hash -S nar "$tmpdir" -H sha512` = 301ra58c2vahczzxiyfin41mpyb0ljh4dh9zn3ijvwviaw1j40sfzw5skh9x945da88n3785ggifzig7acd6k72h0mpsc20m1f66m9n
test `guix hash -S git "$tmpdir"` = 1m9yxz99g7askm88h6hzyv4g2bfv57rp5wvwp3iq5ypsplq1xkkk
test `guix hash -S git "$tmpdir" -H sha512` = 158b10d1bsdk4pm8ym9cg9ckfak1b0cgpw7365cl6s341ir380mh2f4ylicyh8khyrfnwq5cn9766d7m8fbfwwl94ndkv456v6a8knr

# Deprecated --recursive option
test `guix hash -r "$tmpdir" 2>/dev/null` = 10k1lw41wyrjf9mxydi0is5nkpynlsvgslinics4ppir13g7d74p
test `guix hash -r "$tmpdir" -H sha512 2>/dev/null` = 301ra58c2vahczzxiyfin41mpyb0ljh4dh9zn3ijvwviaw1j40sfzw5skh9x945da88n3785ggifzig7acd6k72h0mpsc20m1f66m9n

# Without '-r', this should fail.
! guix hash "$tmpdir"

# This should fail because /dev/null is a character device, which
# the archive format doesn't support.
! guix hash -S nar /dev/null

# Adding a .git directory
mkdir "$tmpdir/.git"
touch "$tmpdir/.git/foo"

# ...changes the hash
test `guix hash -S nar $tmpdir` = 0a50z04zyzf7pidwxv0nwbj82pgzbrhdy9562kncnvkcfvb48m59
test `guix hash -S git $tmpdir` = 0ghlpca9xaswa1ay1g55dknwd9q899mi3ahfr43pq083v8wisjc7

# ...but remains the same when using `-x'
test `guix hash -S nar $tmpdir -x` = 10k1lw41wyrjf9mxydi0is5nkpynlsvgslinics4ppir13g7d74p
test `guix hash -S git $tmpdir -x` = 1m9yxz99g7askm88h6hzyv4g2bfv57rp5wvwp3iq5ypsplq1xkkk

# Without '-r', this should fail.
! guix hash "$tmpdir"

