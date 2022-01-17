# -*- coding: utf-8 -*-
# GNU Guix --- Functional package management for GNU
# Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

import os
import site
import sys

# Commentary:
#
# Site-specific customization for Guix.
#
# The program below honors the GUIX_PYTHONPATH environment variable to
# discover Python packages.  File names appearing in this variable that match
# a predefined versioned installation prefix are added to the sys.path.  To be
# considered, a Python package must be installed under the
# 'lib/pythonX.Y/site-packages' directory, where X and Y are the major and
# minor version numbers of the Python interpreter.
#
# Code:

major_minor = '{}.{}'.format(*sys.version_info)
site_packages_prefix = os.path.join(
    'lib', 'python' + major_minor, 'site-packages')
python_site = os.path.join(sys.prefix, site_packages_prefix)

try:
    all_sites_raw = os.environ['GUIX_PYTHONPATH'].split(os.path.pathsep)
except KeyError:
    all_sites_raw = []
# Normalize paths, otherwise a trailing slash would cause it to not match.
all_sites_norm = [os.path.normpath(p) for p in all_sites_raw]
matching_sites = [p for p in all_sites_norm
                  if p.endswith(site_packages_prefix)]

if matching_sites:
    # Deduplicate the entries, append them to sys.path, and handle any
    # .pth files they contain.
    for s in matching_sites:
        site.addsitedir(s)

    # Move the entries that were appended to sys.path in front of
    # Python's own site-packages directory.  This enables Guix
    # packages to override Python's bundled packages, such as 'pip'.
    python_site_index = sys.path.index(python_site)
    new_site_start_index = sys.path.index(matching_sites[0])
    if python_site_index < new_site_start_index:
        sys.path = (sys.path[:python_site_index]
                    + sys.path[new_site_start_index:]
                    + sys.path[python_site_index:new_site_start_index])
