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

# Insert sites matching the current version into sys.path, right before
# Python's own site.  This way, the user can override the libraries provided
# by Python itself.
sys_path_absolute = [os.path.realpath(p) for p in sys.path]
index = sys_path_absolute.index(python_site)
sys.path[index:index] = matching_sites
