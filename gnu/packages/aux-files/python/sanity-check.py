# -*- coding: utf-8 -*-
# GNU Guix --- Functional package management for GNU
# Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
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

from __future__ import print_function  # Python 2 support.
import importlib
import pkg_resources
import sys
import traceback

try:
    from importlib.machinery import PathFinder
except ImportError:
    PathFinder = None

ret = 0

# Only check site-packages installed by this package, but not dependencies
# (which pkg_resources.working_set would include). Path supplied via argv.
ws = pkg_resources.find_distributions(sys.argv[1])

for dist in ws:
    print('validating', repr(dist.project_name), dist.location)
    try:
        print('...checking requirements: ', end='')
        req = str(dist.as_requirement())
        # dist.activate() is not enough to actually check requirements, we
        # have to .require() it.
        pkg_resources.require(req)
        print('OK')
    except Exception as e:
        print('ERROR:', req, repr(e))
        ret = 1
        continue

    # Try to load top level modules. This should not have any side-effects.
    try:
        metalines = dist.get_metadata_lines('top_level.txt')
    except (KeyError, EnvironmentError):
        # distutils (i.e. #:use-setuptools? #f) will not install any metadata.
        # This file is also missing for packages built using a PEP 517 builder
        # such as poetry.
        print('WARNING: cannot determine top-level modules')
        continue
    for name in metalines:
        # Only available on Python 3.
        if PathFinder and PathFinder.find_spec(name) is None:
            # Ignore unavailable modules, often C modules, which were not
            # installed at the top-level. Cannot use ModuleNotFoundError,
            # because it is raised by failed imports too.
            continue
        try:
            print('...trying to load module', name, end=': ')
            importlib.import_module(name)
            print('OK')
        except Exception:
            print('ERROR:')
            traceback.print_exc(file=sys.stdout)
            ret = 1

    # Try to load entry points of console scripts too, making sure they
    # work. They should be removed if they don't. Other groups may not be
    # safe, as they can depend on optional packages.
    for group, v in dist.get_entry_map().items():
        if group not in {'console_scripts', 'gui_scripts'}:
            continue
        for name, ep in v.items():
            try:
                print('...trying to load endpoint', group, name, end=': ')
                ep.load()
                print('OK')
            except Exception:
                print('ERROR:')
                traceback.print_exc(file=sys.stdout)
                ret = 1

sys.exit(ret)
