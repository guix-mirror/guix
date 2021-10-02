
# GNU Guix --- Functional package management for GNU
# Copyright © 2021 Andrew Tropin <andrew@trop.in>
# Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
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
# Test the 'guix home' using the external store, if any.
#

set -e

guix home --version

NIX_STORE_DIR="$(guile -c '(use-modules (guix config))(display %storedir)')"
localstatedir="$(guile -c '(use-modules (guix config))(display %localstatedir)')"
GUIX_DAEMON_SOCKET="$localstatedir/guix/daemon-socket/socket"
export NIX_STORE_DIR GUIX_DAEMON_SOCKET

# Run tests only when a "real" daemon is available.
if ! guile -c '(use-modules (guix)) (exit (false-if-exception (open-connection)))'
then
    exit 77
fi

STORE_PARENT="$(dirname "$NIX_STORE_DIR")"
export STORE_PARENT
if test "$STORE_PARENT" = "/"; then exit 77; fi

test_directory="$(mktemp -d)"
trap 'chmod -Rf +w "$test_directory"; rm -rf "$test_directory"' EXIT

(
    cd "$test_directory" || exit 77

    HOME="$test_directory"
    export HOME

    #
    # Test 'guix home reconfigure'.
    #

    printf "# dot-bashrc test file for guix home" > "dot-bashrc"

    cat > "home.scm" <<'EOF'
(use-modules (guix gexp)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services))

(home-environment
 (services
  (list
   (simple-service 'test-config
                   home-files-service-type
                   (list `("config/test.conf"
                           ,(plain-file
                             "tmp-file.txt"
                             "the content of ~/.config/test.conf"))))

   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bashrc
              (list
               (local-file (string-append (dirname (current-filename))
                                          "/dot-bashrc"))))))

   (simple-service 'home-bash-service-extension-test
                   home-bash-service-type
                   (home-bash-extension
                    (bashrc
                     (list
                      (plain-file
                       "bashrc-test-config.sh"
                       "# the content of bashrc-test-config.sh"))))))))
EOF

    guix home reconfigure "${test_directory}/home.scm"
    test -d "${HOME}/.guix-home"
    test -h "${HOME}/.bash_profile"
    test -h "${HOME}/.bashrc"
    test "$(tail -n 2 "${HOME}/.bashrc")" == "\
# dot-bashrc test file for guix home
# the content of bashrc-test-config.sh"
    grep -q "the content of ~/.config/test.conf" "${HOME}/.config/test.conf"

    #
    # Test 'guix home describe'.
    #

    configuration_file()
    {
        guix home describe                      \
            | grep 'configuration file:'        \
            | cut -d : -f 2                     \
            | xargs echo
    }
    test "$(cat "$(configuration_file)")" == "$(cat home.scm)"

    canonical_file_name()
    {
        guix home describe                      \
            | grep 'canonical file name:'       \
            | cut -d : -f 2                     \
            | xargs echo
    }
    test "$(canonical_file_name)" == "$(readlink "${HOME}/.guix-home")"

    #
    # Test 'guix home search'.
    #

    guix home search mcron | grep "^name: home-mcron"
    guix home search job manager | grep "^name: home-mcron"
)
