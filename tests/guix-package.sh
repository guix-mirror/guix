# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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
# Test the `guix-package' command-line utility.
#

guix-package --version

readlink_base ()
{
    basename `readlink "$1"`
}

profile="t-profile-$$"
rm -f "$profile"

trap 'rm "$profile" "$profile-"[0-9]* ; rm -rf t-home-'"$$" EXIT

boot_guile="`guix-build -e '(@ (gnu packages bootstrap) %bootstrap-guile)'`"

guix-package --bootstrap -p "$profile" -i "$boot_guile"
test -L "$profile" && test -L "$profile-1-link"
test -f "$profile/bin/guile"

# Installing the same package a second time does nothing.
guix-package --bootstrap -p "$profile" -i "$boot_guile"
test -L "$profile" && test -L "$profile-1-link"
! test -f "$profile-2-link"
test -f "$profile/bin/guile"

# Check whether we have network access.
if guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    boot_make="`guix-build -e '(@@ (gnu packages base) gnu-make-boot0)'`"
    guix-package --bootstrap -p "$profile" -i "$boot_make"
    test -L "$profile-2-link"
    test -f "$profile/bin/make" && test -f "$profile/bin/guile"


    # Check whether `--list-installed' works.
    # XXX: Change the tests when `--install' properly extracts the package
    # name and version string.
    installed="`guix-package -p "$profile" --list-installed | cut -f1 | xargs echo | sort`"
    case "x$installed" in
	"guile-bootstrap make-boot0")
	    true;;
	"make-boot0 guile-bootstrap")
	    true;;
	"*")
            false;;
    esac

    test "`guix-package -p "$profile" -I 'g.*e' | cut -f1`" = "guile-bootstrap"

    # Search.
    test "`guix-package -s "GNU Hello" | cut -f1`" = "hello"
    test "`guix-package -s "n0t4r341p4ck4g3"`" = ""

    # Remove a package.
    guix-package --bootstrap -p "$profile" -r "guile-bootstrap"
    test -L "$profile-3-link"
    test -f "$profile/bin/make" && ! test -f "$profile/bin/guile"

    # Roll back.
    guix-package --roll-back -p "$profile"
    test "`readlink_base "$profile"`" = "$profile-2-link"
    test -x "$profile/bin/guile" && test -x "$profile/bin/make"
    guix-package --roll-back -p "$profile"
    test "`readlink_base "$profile"`" = "$profile-1-link"
    test -x "$profile/bin/guile" && ! test -x "$profile/bin/make"

    # Move to the empty profile.
    for i in `seq 1 3`
    do
	guix-package --bootstrap --roll-back -p "$profile"
	! test -f "$profile/bin"
	! test -f "$profile/lib"
	test "`readlink_base "$profile"`" = "$profile-0-link"
    done

    # Reinstall after roll-back to the empty profile.
    guix-package --bootstrap -p "$profile" -i "$boot_make"
    test "`readlink_base "$profile"`" = "$profile-1-link"
    test -x "$profile/bin/guile" && ! test -x "$profile/bin/make"

    # Roll-back to generation 0, and install---all at once.
    guix-package --bootstrap -p "$profile" --roll-back -i "$boot_guile"
    test "`readlink_base "$profile"`" = "$profile-1-link"
    test -x "$profile/bin/guile" && ! test -x "$profile/bin/make"

    # Install Make.
    guix-package --bootstrap -p "$profile" -i "$boot_make"
    test "`readlink_base "$profile"`" = "$profile-2-link"
    test -x "$profile/bin/guile" && test -x "$profile/bin/make"

    # Make a "hole" in the list of generations, and make sure we can
    # roll back "over" it.
    rm "$profile-1-link"
    guix-package --bootstrap -p "$profile" --roll-back
    test "`readlink_base "$profile"`" = "$profile-0-link"
fi

# Make sure the `:' syntax works.
guix-package --bootstrap -i "binutils:lib" -p "$profile" -n

# Make sure nonexistent outputs are reported.
guix-package --bootstrap -i "guile-bootstrap:out" -p "$profile" -n
if guix-package --bootstrap -i "guile-bootstrap:does-not-exist" -p "$profile" -n;
then false; else true; fi
if guix-package --bootstrap -i "guile-bootstrap:does-not-exist" -p "$profile";
then false; else true; fi

# Check whether `--list-available' returns something sensible.
guix-package -A 'gui.*e' | grep guile

#
# Try with the default profile.
#

XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_CACHE_HOME
HOME="t-home-$$"
export HOME

mkdir -p "$HOME"

guix-package --bootstrap -i "$boot_guile"
test -L "$HOME/.guix-profile"
test -f "$HOME/.guix-profile/bin/guile"

if guile -c '(getaddrinfo "www.gnu.org" "80" AI_NUMERICSERV)' 2> /dev/null
then
    guix-package --bootstrap -i "$boot_make"
    test -f "$HOME/.guix-profile/bin/make"
    first_environment="`cd $HOME/.guix-profile ; pwd`"

    guix-package --bootstrap --roll-back
    test -f "$HOME/.guix-profile/bin/guile"
    ! test -f "$HOME/.guix-profile/bin/make"
    test "`cd $HOME/.guix-profile ; pwd`" = "$first_environment"
fi

# Move to the empty profile.
default_profile="`readlink "$HOME/.guix-profile"`"
for i in `seq 1 3`
do
    guix-package --bootstrap --roll-back
    ! test -f "$HOME/.guix-profile/bin"
    ! test -f "$HOME/.guix-profile/lib"
    test "`readlink "$default_profile"`" = "$default_profile-0-link"
done

# Extraneous argument.
if guix-package install foo-bar;
then false; else true; fi
