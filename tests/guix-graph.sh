# GNU Guix --- Functional package management for GNU
# Copyright © 2015-2016, 2019-2020, 2022 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2019 Simon Tournier <zimon.toutoune@gmail.com>
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
# Test the 'guix graph' command-line utility.
#

module_dir="t-guix-graph-$$"
mkdir "$module_dir"

tmpfile1="$module_dir/t-guix-graph1-$$"
tmpfile2="$module_dir/t-guix-graph2-$$"
trap 'rm -r "$module_dir"' EXIT


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


guix graph --version

for package in guile-bootstrap coreutils python
do
    for graph in package bag-emerged bag bag-with-origins
    do
	guix graph -t "$graph" "$package" | grep "$package"
    done
done

guix build guile-bootstrap
guix graph -t references guile-bootstrap | grep guile-bootstrap

guix graph -e '(@ (gnu packages bootstrap) %bootstrap-guile)' \
    | grep guile-bootstrap

! guix graph -e +

# Try passing store file names.

guix graph -t references guile-bootstrap > "$tmpfile1"
guix graph -t references `guix build guile-bootstrap` > "$tmpfile2"
cmp "$tmpfile1" "$tmpfile2"

# XXX: Filter the file names in the graph to work around the fact that we get
# a mixture of relative and absolute file names.
guix graph -t derivation coreutils > "$tmpfile1"
guix graph -t derivation `guix build -d coreutils` > "$tmpfile2"
cmp "$tmpfile1" "$tmpfile2"

# Try package transformation options.
guix graph git | grep 'label = "openssl'
guix graph git --with-input=openssl=libressl | grep 'label = "libressl'
! guix graph git --with-input=openssl=libressl | grep 'label = "openssl'

# Try --load-path
guix graph -L $module_dir dummy | grep 'label = "dummy'

# Displaying shortest paths (or lack thereof).
! guix graph --path emacs vim

path="\
emacs
gnutls
guile
libffi"
test "`guix graph --path emacs libffi | cut -d '@' -f1`" = "$path"

# At the derivation level, there's a direct path because libffi is propagated
# via gtk+.
test "`guix graph --path -t derivation emacs libffi | wc -l`" -ge 2
