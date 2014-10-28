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

module_dir="t-guix-lint-$$"
mkdir "$module_dir"
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

export GUIX_PACKAGE_PATH=$module_dir
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

out=`guix lint dummy 2>&1`
if [ `grep_warning "$out"` -ne 3 ]
then false; else true; fi

out=`guix lint -c synopsis dummy 2>&1`
if [ `grep_warning "$out"` -ne 2 ]
then false; else true; fi

out=`guix lint -c description dummy 2>&1`
if [ `grep_warning "$out"` -ne 1 ]
then false; else true; fi

out=`guix lint -c description,synopsis dummy 2>&1`
if [ `grep_warning "$out"` -ne 3 ]
then false; else true; fi

if guix lint -c synopsis,invalid-checker dummy 2>&1 | \
   grep -q 'invalid-checker: invalid checker'
then true; else false; fi
