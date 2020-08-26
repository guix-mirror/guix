# GNU Guix --- Functional package management for GNU
# Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
# Test the 'guix pack --relocatable' using the external store, if any.
#

guix pack --version

# 'guix pack --relocatable' requires a C compiler and libc.a, which our
# bootstrap binaries don't provide.  To make the test relatively inexpensive,
# run it on the user's global store if possible, on the grounds that binaries
# may already be there or can be built or downloaded inexpensively.

storedir="`guile -c '(use-modules (guix config))(display %storedir)'`"
localstatedir="`guile -c '(use-modules (guix config))(display %localstatedir)'`"
NIX_STORE_DIR="$storedir"
GUIX_DAEMON_SOCKET="$localstatedir/guix/daemon-socket/socket"
export NIX_STORE_DIR GUIX_DAEMON_SOCKET

if ! guile -c '(use-modules (guix)) (exit (false-if-exception (open-connection)))'
then
    exit 77
fi

# Attempt to run the given command in a namespace where the store is
# invisible.  This makes sure the presence of the store does not hide
# problems.
run_without_store ()
{
    if unshare -r true		# Are user namespaces supported?
    then
	# Run that relocatable executable in a user namespace where we "erase"
	# the store by mounting an empty file system on top of it.  That way,
	# we exercise the wrapper code that creates the user namespace and
	# bind-mounts the store.
	unshare -mrf sh -c 'mount -t tmpfs -o ro none "$NIX_STORE_DIR"; '"$*"
    else
	# Run the relocatable program in the current namespaces.  This is a
	# weak test because we're going to access store items from the host
	# store.
	$*
    fi
}

test_directory="`mktemp -d`"
export test_directory
trap 'chmod -Rf +w "$test_directory"; rm -rf "$test_directory"' EXIT

if unshare -r true
then
    # Test the 'userns' execution engine.
    tarball="`guix pack -R -S /Bin=bin sed`"
    (cd "$test_directory"; tar xvf "$tarball")

    run_without_store "$test_directory/Bin/sed" --version > "$test_directory/output"
    grep 'GNU sed' "$test_directory/output"

    # Same with an explicit engine.
    run_without_store GUIX_EXECUTION_ENGINE="userns" \
		      "$test_directory/Bin/sed" --version > "$test_directory/output"
    grep 'GNU sed' "$test_directory/output"

    # Check whether the exit code is preserved.
    if run_without_store "$test_directory/Bin/sed" --does-not-exist;
    then false; else true; fi

    chmod -Rf +w "$test_directory"; rm -rf "$test_directory"/*
else
    echo "'userns' execution tests skipped" >&2
fi

case "`uname -m`" in
    x86_64|i?86)
	# Try '-RR' and PRoot.
	tarball="`guix pack -RR -S /Bin=bin sed`"
	tar tvf "$tarball" | grep /bin/proot
	(cd "$test_directory"; tar xf "$tarball")
	run_without_store GUIX_EXECUTION_ENGINE="proot" \
	"$test_directory/Bin/sed" --version > "$test_directory/output"
	grep 'GNU sed' "$test_directory/output"

	# Now with fakechroot.
	run_without_store GUIX_EXECUTION_ENGINE="fakechroot" \
	"$test_directory/Bin/sed" --version > "$test_directory/output"
	grep 'GNU sed' "$test_directory/output"

	chmod -Rf +w "$test_directory"; rm -rf "$test_directory"/*

	if unshare -r true
	then
	    # Check whether the store contains everything it should.  Check
	    # once when erasing $STORE_PARENT ("/gnu") and once when erasing
	    # $NIX_STORE_DIR ("/gnu/store").
	    tarball="`guix pack -RR -S /bin=bin bash-minimal`"
	    (cd "$test_directory"; tar xf "$tarball")

	    STORE_PARENT="`dirname $NIX_STORE_DIR`"
	    export STORE_PARENT

	    for engine in userns proot fakechroot
	    do
		for i in $(guix gc -R $(guix build bash-minimal | grep -v -e '-doc$'))
		do
		    unshare -mrf sh -c "mount -t tmpfs none \"$NIX_STORE_DIR\"; GUIX_EXECUTION_ENGINE=$engine $test_directory/bin/sh -c 'echo $NIX_STORE_DIR/*'" | grep $(basename $i)
		    unshare -mrf sh -c "mount -t tmpfs none \"$STORE_PARENT\";  GUIX_EXECUTION_ENGINE=$engine $test_directory/bin/sh -c 'echo $NIX_STORE_DIR/*'" | grep $(basename $i)
		done
	    done

	    chmod -Rf +w "$test_directory"; rm -rf "$test_directory"/*
	fi
	;;
    *)
	echo "skipping PRoot and Fakechroot tests" >&2
	;;
esac

# Ensure '-R' works with outputs other than "out".
tarball="`guix pack -R -S /share=share groff:doc`"
(cd "$test_directory"; tar xf "$tarball")
test -d "$test_directory/share/doc/groff/html"

# Ensure '-R' applies to propagated inputs.  Failing to do that, it would fail
# with a profile collision error in this case because 'python-scipy'
# propagates 'python-numpy'.  See <https://bugs.gnu.org/42510>.
guix pack -RR python-numpy python-scipy --no-grafts -n
