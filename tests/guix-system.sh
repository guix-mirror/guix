# GNU Guix --- Functional package management for GNU
# Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
# Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
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
# Test 'guix system', mostly error reporting.
#

set -e

guix system --version

tmpfile="t-guix-system-$$"
errorfile="t-guix-system-error-$$"

# Note: This directory is chosen outside $builddir so that relative file name
# canonicalization doesn't mess up with 'current-source-directory', used by
# 'local-file' ('load' forces 'relative' for
# %FILE-PORT-NAME-CANONICALIZATION.)
tmpdir="${TMPDIR:-/tmp}/t-guix-system-$$"
mkdir "$tmpdir"

trap 'rm -f "$tmpfile" "$errorfile" "$tmpdir"/*; rmdir "$tmpdir"' EXIT

# Reporting of syntax errors.

cat > "$tmpfile"<<EOF
;; This is line 1, and the next one is line 2.
   (operating-system)
;; The 'T' is at column 3.
EOF

if guix system vm "$tmpfile" 2> "$errorfile"
then
    # This must not succeed.
    exit 1
else
    grep "$tmpfile:2:3:.*missing.* initializers" "$errorfile"
fi


cat > "$tmpfile"<<EOF
;; This is line 1, and the next one is line 2.
   (operating-system
;; This is line 3, and there is no closing paren!
EOF

if guix system vm "$tmpfile" 2> "$errorfile"
then
    # This must not succeed.
    exit 1
else
    grep "$tmpfile:4:1: missing closing paren" "$errorfile"
fi


# Reporting of module not found errors.

cat > "$tmpfile" <<EOF
;; Line 1.
(use-modules (gnu))
  (use-service-modules openssh)
EOF

if guix system build "$tmpfile" -n 2> "$errorfile"
then false
else
    grep "$tmpfile:3:2: .*module .*openssh.*not found" "$errorfile"
    grep "Try.*use-service-modules ssh" "$errorfile"
fi

cat > "$tmpfile" <<EOF
;; Line 1.
(use-modules (gnu))
  (use-package-modules qemu)
EOF

if guix system build "$tmpfile" -n 2> "$errorfile"
then false
else
    grep "$tmpfile:3:2: .*module .*qemu.*not found" "$errorfile"
    grep "Try.*use-package-modules virtualization" "$errorfile"
fi

# Reporting of unbound variables.

cat > "$tmpfile" <<EOF
(use-modules (gnu))                                   ; 1
(use-service-modules networking)                      ; 2

(operating-system                                     ; 4
  (host-name "antelope")                              ; 5
  (timezone "Europe/Paris")                           ; 6
  (locale "en_US.UTF-8")                              ; 7

  (bootloader (GRUB-config (target "/dev/sdX")))      ; 9
  (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems)))
EOF

if guix system build "$tmpfile" -n 2> "$errorfile"
then false
else
    if test "`guile -c '(display (effective-version))'`" = 2.2
    then
	# FIXME: With Guile 2.2.0 the error is reported on line 4.
	# See <http://bugs.gnu.org/26107>.
	grep "$tmpfile:[49]:[0-9]\+:.*GRUB-config.*[Uu]nbound variable" "$errorfile"
    else
	grep "$tmpfile:9:[0-9]\+:.*GRUB-config.*[Uu]nbound variable" "$errorfile"
    fi
fi

OS_BASE='
  (host-name "antelope")
  (timezone "Europe/Paris")
  (locale "en_US.UTF-8")

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (target "/dev/sdX")))
  (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
'

# Reporting of duplicate service identifiers.

cat > "$tmpfile" <<EOF
(use-modules (gnu))
(use-service-modules networking)

(operating-system
  $OS_BASE
  (services (cons* (service dhcp-client-service-type)
                   (service dhcp-client-service-type) ;twice!
                   %base-services)))
EOF

if guix system vm "$tmpfile" 2> "$errorfile"
then
    # This must not succeed.
    exit 1
else
    grep "service 'networking'.*more than once" "$errorfile"
fi

# Reporting unmet shepherd requirements.

cat > "$tmpfile" <<EOF
(use-modules (gnu) (gnu services shepherd))
(use-service-modules networking)

(define buggy-service-type
  (shepherd-service-type
    'buggy
    (lambda _
      (shepherd-service
        (provision '(buggy!))
        (requirement '(does-not-exist))
        (start #t)))))

(operating-system
  $OS_BASE
  (services (cons (service buggy-service-type #t)
                  %base-services)))
EOF

if guix system build "$tmpfile" 2> "$errorfile"
then
    exit 1
else
    grep "service 'buggy!'.*'does-not-exist'.*not provided" "$errorfile"
fi

# Reporting inconsistent user accounts.

make_user_config ()
{
    cat > "$tmpfile" <<EOF
(use-modules (gnu))
(use-service-modules networking)

(operating-system
  (host-name "antelope")
  (timezone "Europe/Paris")
  (locale "en_US.UTF-8")

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/sdX")))
  (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  (users (list (user-account
                 (name "dave")
                 (home-directory "/home/dave")
                 (group "$1")
                 (supplementary-groups '("$2"))))))
EOF
}

make_user_config "users" "wheel"
guix system build "$tmpfile" -n       # succeeds

guix system build "$tmpfile" -d	      # succeeds
guix system build "$tmpfile" -d | grep '\.drv$'

guix system vm "$tmpfile" -d	      # succeeds
guix system vm "$tmpfile" -d | grep '\.drv$'

# Make sure the behavior is deterministic (<https://bugs.gnu.org/32652>).
drv1="`guix system vm "$tmpfile" -d`"
drv2="`guix system vm "$tmpfile" -d`"
test "$drv1" = "$drv2"
drv1="`guix system disk-image --file-system-type=iso9660 "$tmpfile" -d`"
drv2="`guix system disk-image --file-system-type=iso9660 "$tmpfile" -d`"
test "$drv1" = "$drv2"

make_user_config "group-that-does-not-exist" "users"
if guix system build "$tmpfile" -n 2> "$errorfile"
then false
else grep "primary group.*group-that-does-not-exist.*undeclared" "$errorfile"; fi

make_user_config "users" "group-that-does-not-exist"
if guix system build "$tmpfile" -n 2> "$errorfile"
then false
else grep "supplementary group.*group-that-does-not-exist.*undeclared" "$errorfile"; fi

# Try 'local-file' and relative file name resolution.

cat > "$tmpdir/config.scm"<<EOF
(use-modules (gnu))
(use-service-modules networking)

(operating-system
  $OS_BASE
  (services (cons (service tor-service-type
                           (tor-configuration
                             (config-file (local-file "my-torrc"))))
                  %base-services)))
EOF

cat > "$tmpdir/my-torrc"<<EOF
# This is an example file.
EOF

# In both cases 'my-torrc' should be properly resolved.
guix system build "$tmpdir/config.scm" -n
(cd "$tmpdir"; guix system build "config.scm" -n)

# Searching.
guix system search tor | grep "^name: tor"
guix system search tor | grep "^shepherdnames: tor"
guix system search anonym network | grep "^name: tor"

# Below, use -n (--dry-run) for the tests because if we actually tried to
# build these images, the commands would take hours to run in the worst case.

# Verify that the examples can be built.
for example in gnu/system/examples/*.tmpl; do
    guix system -n disk-image "$example"
done

# Verify that the disk image types can be built.
guix system -n vm gnu/system/examples/vm-image.tmpl
guix system -n vm-image gnu/system/examples/vm-image.tmpl
# This invocation was taken care of in the loop above:
# guix system -n disk-image gnu/system/examples/bare-bones.tmpl
guix system -n disk-image --file-system-type=iso9660 gnu/system/examples/bare-bones.tmpl
guix system -n docker-image gnu/system/examples/docker-image.tmpl
