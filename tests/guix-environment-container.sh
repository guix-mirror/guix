# GNU Guix --- Functional package management for GNU
# Copyright © 2015 David Thompson <davet@gnu.org>
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
# Test 'guix environment'.
#

set -e

guix environment --version

if ! guile -c '((@@ (guix scripts environment) assert-container-features))'
then
    # User containers are not supported; skip this test.
    exit 77
fi

tmpdir="t-guix-environment-$$"
trap 'rm -r "$tmpdir"' EXIT

mkdir "$tmpdir"

# Make sure the exit value is preserved.
if guix environment --container --ad-hoc --bootstrap guile-bootstrap \
        -- guile -c '(exit 42)'
then
    false
else
    test $? = 42
fi

# By default, the UID inside the container should be the same as outside.
uid="`id -u`"
inner_uid="`guix environment -C --ad-hoc --bootstrap guile-bootstrap \
  -- guile -c '(display (getuid))'`"
test $inner_uid = $uid

# When '--user' is passed, the UID should be 1000.  (Note: Use a separate HOME
# so that we don't run into problems when the test directory is under /home.)
export tmpdir
inner_uid="`HOME=$tmpdir guix environment -C --ad-hoc --bootstrap guile-bootstrap \
  --user=gnu-guix -- guile -c '(display (getuid))'`"
test $inner_uid = 1000

if test "x$USER" = "x"; then USER="`id -un`"; fi

# Check whether /etc/passwd and /etc/group are valid.
guix environment -C --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c "(exit (string=? \"$USER\" (passwd:name (getpwuid (getuid)))))"
guix environment -C --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c '(exit (string? (group:name (getgrgid (getgid)))))'
guix environment -C --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c '(use-modules (srfi srfi-1))
                  (exit (every group:name
                               (map getgrgid (vector->list (getgroups)))))'

# Make sure file-not-found errors in mounts are reported.
if guix environment --container --ad-hoc --bootstrap guile-bootstrap \
	--expose=/does-not-exist -- guile -c 1 2> "$tmpdir/error"
then
    false
else
    grep "/does-not-exist" "$tmpdir/error"
    grep "[Nn]o such file" "$tmpdir/error"
fi

# Make sure that the right directories are mapped.
mount_test_code="
(use-modules (ice-9 rdelim)
             (ice-9 match)
             (srfi srfi-1))

(define mappings
  (filter-map (lambda (line)
                (match (string-split line #\space)
                  ;; Empty line.
                  ((\"\") #f)
                  ;; Ignore the root file system.
                  ((_ \"/\" _ _ _ _)
                   #f)
                  ;; Ignore these types of file systems, except if they
                  ;; correspond to a parent file system.
                  ((_ mount (or \"tmpfs\" \"proc\" \"sysfs\" \"devtmpfs\"
                                \"devpts\" \"cgroup\" \"mqueue\") _ _ _)
                   (and (string-prefix? (getcwd) mount)
		        mount))
                  ((_ mount _ _ _ _)
                   mount)))
              (string-split (call-with-input-file \"/proc/mounts\" read-string)
                            #\newline)))

(for-each (lambda (mount)
            (display mount)
            (newline))
          mappings)"

guix environment --container --ad-hoc --bootstrap guile-bootstrap \
     -- guile -c "$mount_test_code" > $tmpdir/mounts

cat "$tmpdir/mounts"
test `wc -l < $tmpdir/mounts` -eq 4

current_dir="`cd $PWD; pwd -P`"
grep -e "$current_dir$" $tmpdir/mounts # current directory
grep $(guix build guile-bootstrap) $tmpdir/mounts
grep -e "$NIX_STORE_DIR/.*-bash" $tmpdir/mounts # bootstrap bash

rm $tmpdir/mounts

# Make sure 'GUIX_ENVIRONMENT' is linked to '~/.guix-profile' when requested
# within a container.
(
  linktest='(exit (string=? (getenv "GUIX_ENVIRONMENT")
(readlink (string-append (getenv "HOME") "/.guix-profile"))))'

  cd "$tmpdir" \
     && guix environment --bootstrap --container --link-profile \
             --ad-hoc guile-bootstrap --pure \
             -- guile -c "$linktest"
)

# Test that user can be mocked.
usertest='(exit (and (string=? (getenv "HOME") "/home/foognu")
                     (string=? (passwd:name (getpwuid 1000)) "foognu")
                     (file-exists? "/home/foognu/umock")))'
touch "$tmpdir/umock"
HOME="$tmpdir" guix environment --bootstrap --container --user=foognu \
     --ad-hoc guile-bootstrap --pure \
     --share="$tmpdir/umock" \
     -- guile -c "$usertest"


# Check the exit code.

abnormal_exit_code="
(use-modules (system foreign))
;; Purposely make Guile crash with a segfault. :)
(pointer->string (make-pointer 123) 123)"

if guix environment --bootstrap --container \
	--ad-hoc guile-bootstrap -- guile -c "$abnormal_exit_code"
then false;
else
    test $? -gt 127
fi
