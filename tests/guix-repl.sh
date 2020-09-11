# GNU Guix --- Functional package management for GNU
# Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
# Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
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
# Test the `guix repl' command-line utility.
#

guix repl --version

test_directory="`mktemp -d`"
export test_directory
trap 'chmod -Rf +w "$test_directory"; rm -rf "$test_directory"' EXIT

tmpfile="$test_directory/foo.scm"
rm -f "$tmpfile"
trap 'rm -f "$tmpfile"' EXIT

module_dir="t-guix-repl-$$"
mkdir "$module_dir"
trap 'rm -rf "$module_dir"' EXIT


cat > "$tmpfile"<<EOF
(use-modules (guix packages)
             (gnu packages base))

(format #t "~a\n" (package-name coreutils))
EOF

test "`guix repl "$tmpfile"`" = "coreutils"

# Make sure that the file can also be loaded when passed as a relative file
# name.
(cd "$(dirname "$tmpfile")"; test "$(guix repl "$(basename "$tmpfile")")" = "coreutils")


cat > "$module_dir/foo.scm"<<EOF
(define-module (foo)
  #:use-module (guix packages)
  #:use-module (gnu packages base))

(define-public dummy
  (package (inherit hello)
    (name "dummy")
    (version "42")
    (synopsis "dummy package")
    (description "dummy package. Only used for testing purposes.")))
EOF

cat > "$tmpfile"<<EOF
(use-modules (guix packages)
             (foo))

(format #t "~a\n" (package-version dummy))
EOF

test "`guix repl "$tmpfile" -L "$module_dir"`" = "42"

cat > "$tmpfile"<<EOF
(format #t "~a\n" (cdr (command-line)))
EOF

test "`guix repl -- "$tmpfile" -a b --input=foo.txt`" = "(-a b --input=foo.txt)"

cat > "$tmpfile"<<EOF
#!$(type -P env) -S guix repl --
!#
(format #t "~a\n" (cdr (command-line)))
EOF
chmod 755 $tmpfile

test "`"$tmpfile" -a b --input=foo.txt`" = "(-a b --input=foo.txt)"
