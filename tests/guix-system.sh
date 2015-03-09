# GNU Guix --- Functional package management for GNU
# Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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
# Test the daemon and its interaction with 'guix substitute-binary'.
#

set -e

guix system --version

tmpfile="t-guix-system-$$"
errorfile="t-guix-system-error-$$"
trap 'rm -f "$tmpfile" "$errorfile"' EXIT

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


# Reporting of duplicate service identifiers.

cat > "$tmpfile" <<EOF
(use-modules (gnu))
(use-service-modules networking)

(operating-system
  (host-name "antelope")
  (timezone "Europe/Paris")
  (locale "en_US.UTF-8")

  (bootloader (grub-configuration (device "/dev/sdX")))
  (file-systems (cons (file-system
                        (device "root")
                        (title 'label)
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (services (cons* (dhcp-client-service)
                   (dhcp-client-service) ;twice!
                   %base-services)))
EOF

if guix system vm "$tmpfile" 2> "$errorfile"
then
    # This must not succeed.
    exit 1
else
    grep "service 'networking'.*more than once" "$errorfile"
fi
