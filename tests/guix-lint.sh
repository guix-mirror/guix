# GNU Guix --- Functional package management for GNU
# Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
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
# Test the `guix lint' command-line utility.
#

guix lint --version

# Choose a module directory not below any %LOAD-PATH component.  This is
# necessary when testing '-L' with a relative file name.
module_dir="$(mktemp -d)"

mkdir -p "$module_dir"
trap "rm -rf $module_dir" EXIT


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

GUIX_PACKAGE_PATH="$module_dir"
export GUIX_PACKAGE_PATH

grep_warning ()
{
    res=`echo "$1" | grep -E -c "(synopsis|description) should"`
    echo $res
}

# Three issues with the dummy package:
# 1) the synopsis starts with the package name;
# 2) the synopsis starts with a lower-case letter;
# 3) the description has a single space following the end-of-sentence period.

out=`guix lint -c synopsis,description dummy 2>&1`
test `grep_warning "$out"` -eq 3

out=`guix lint -c synopsis dummy 2>&1`
test `grep_warning "$out"` -eq 2

out=`guix lint -c description dummy 2>&1`
test `grep_warning "$out"` -eq 1

out=`guix lint -c description,synopsis dummy 2>&1`
test `grep_warning "$out"` -eq 3

guix lint -c synopsis,invalid-checker dummy 2>&1 | \
   grep -q 'invalid-checker: invalid checker'

# Make sure specifying multiple packages works.
guix lint -c inputs-should-be-native dummy dummy@42 dummy


# Use --load-path instead.
unset GUIX_PACKAGE_PATH

out=`guix lint -L $module_dir -c synopsis,description dummy 2>&1`
test `grep_warning "$out"` -eq 3

# Make sure specifying multiple packages works.
guix lint -L $module_dir -c inputs-should-be-native dummy dummy@42 dummy

# Test '-L' with a relative file name.  'guix lint' will see "t-xyz/foo.scm"
# (instead of "foo.scm") and will thus fail to find it in %LOAD-PATH.  Check
# that it does find it anyway.  See <https://bugs.gnu.org/42543>.
(cd "$module_dir"/.. ; guix lint -c formatting -L "$(basename "$module_dir")" dummy@42) 2>&1 > "$module_dir/out"
test -z "$(cat "$module_dir/out")"

# Likewise, when there's a warning, 'package-field-location' used to crash
# because it can't find "t-xyz/foo.scm".  See <https://bugs.gnu.org/46390>.
(cd "$module_dir"/.. ; guix lint -c synopsis -L "$(basename "$module_dir")" dummy@42) 2>&1 > "$module_dir/out"
grep_warning "`cat "$module_dir/out"`"
