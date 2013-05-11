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
# Test the `guix hash' command-line utility.
#

guix hash --version

test `guix hash /dev/null` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `guix hash -f nix-base32 /dev/null` = 0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73
test `guix hash -f hex /dev/null` = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
test `guix hash -f base32 /dev/null` = 4oymiquy7qobjgx36tejs35zeqt24qpemsnzgtfeswmrw6csxbkq
