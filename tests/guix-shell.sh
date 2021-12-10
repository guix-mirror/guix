# GNU Guix --- Functional package management for GNU
# Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix shell' alias.
#

guix shell --version

configdir="t-guix-shell-config-$$"
tmpdir="t-guix-shell-$$"
trap 'rm -r "$tmpdir" "$configdir"' EXIT
mkdir "$tmpdir" "$configdir" "$configdir/guix"

XDG_CONFIG_HOME="$(realpath $configdir)"
export XDG_CONFIG_HOME

guix shell --bootstrap --pure guile-bootstrap -- guile --version

# '--ad-hoc' is a thing of the past.
! guix shell --ad-hoc guile-bootstrap

# Ignoring unauthorized files.
cat > "$tmpdir/guix.scm" <<EOF
This is a broken guix.scm file.
EOF
! (cd "$tmpdir"; SHELL="$(type -P true)" guix shell --bootstrap 2> "stderr")
grep "not authorized" "$tmpdir/stderr"
rm "$tmpdir/stderr"

# Authorize the directory.
echo "$(realpath "$tmpdir")" > "$configdir/guix/shell-authorized-directories"

# Ignoring 'manifest.scm' and 'guix.scm' in non-interactive use.
(cd "$tmpdir"; guix shell --bootstrap -- true)
mv "$tmpdir/guix.scm" "$tmpdir/manifest.scm"
(cd "$tmpdir"; guix shell --bootstrap -- true)
rm "$tmpdir/manifest.scm"

# Honoring the local 'manifest.scm' file.
cat > "$tmpdir/manifest.scm" <<EOF
(specifications->manifest '("guile-bootstrap"))
EOF
cat > "$tmpdir/fake-shell.sh" <<EOF
#!$SHELL
# This fake shell allows us to test interactive use.
exec echo "\$GUIX_ENVIRONMENT"
EOF
chmod +x "$tmpdir/fake-shell.sh"
profile1="$(cd "$tmpdir"; SHELL="$(realpath fake-shell.sh)" guix shell --bootstrap)"
profile2="$(guix shell --bootstrap guile-bootstrap -- "$SHELL" -c 'echo $GUIX_ENVIRONMENT')"
test -n "$profile1"
test "$profile1" = "$profile2"
rm "$tmpdir/manifest.scm"

# Do not read manifest when passed '-q'.
echo "Broken manifest." > "$tmpdir/manifest.scm"
(cd "$tmpdir"; SHELL="$(realpath fake-shell.sh)" guix shell --bootstrap -q)
rm "$tmpdir/manifest.scm"

# Make sure '-D' affects only the immediately following '-f', and not packages
# that appear later: <https://issues.guix.gnu.org/52093>.
cat > "$tmpdir/empty-package.scm" <<EOF
(use-modules (guix) (guix tests)
             (guix build-system trivial))

(dummy-package "empty-package"
  (build-system trivial-build-system))   ;zero inputs
EOF

guix shell --bootstrap --pure -D -f "$tmpdir/empty-package.scm" \
     guile-bootstrap -- guile --version
rm "$tmpdir/empty-package.scm"

if guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    # Compute the build environment for the initial GNU Make.
    guix shell --bootstrap --no-substitutes --search-paths --pure \
         -D -e '(@ (guix tests) gnu-make-for-tests)' > "$tmpdir/a"

    # Make sure bootstrap binaries are in the profile.
    profile=`grep "^export PATH" "$tmpdir/a" | sed -r 's|^.*="(.*)/bin"|\1|'`

    # Make sure the bootstrap binaries are all listed where they belong.
    grep -E "^export PATH=\"$profile/bin\""               "$tmpdir/a"
    grep -E "^export C_INCLUDE_PATH=\"$profile/include\"" "$tmpdir/a"
    grep -E "^export LIBRARY_PATH=\"$profile/lib\""       "$tmpdir/a"
    for dep in bootstrap-binaries-0 gcc-bootstrap-0 glibc-bootstrap-0
    do
	guix gc --references "$profile" | grep "$dep"
    done

    # 'make-boot0' itself must not be listed.
    ! guix gc --references "$profile" | grep make-boot0

    # Honoring the local 'guix.scm' file.
    echo '(@ (guix tests) gnu-make-for-tests)' > "$tmpdir/guix.scm"
    (cd "$tmpdir"; guix shell --bootstrap --search-paths --pure > "b")
    cmp "$tmpdir/a" "$tmpdir/b"
    rm "$tmpdir/guix.scm"
fi
