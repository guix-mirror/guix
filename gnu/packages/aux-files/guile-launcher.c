/* GNU Guix --- Functional package management for GNU
   Copyright 1996-1997,2000-2001,2006,2008,2011,2013,2018
      Free Software Foundation, Inc.
   Copyright (C) 2020 Ludovic Courtès <ludo@gnu.org>

   This file is part of GNU Guix.

   GNU Guix is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   GNU Guix is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.  */

/* This file implements a variant of the 'guile' executable that does not
   complain about locale issues.  */

#include <locale.h>
#include <libguile.h>

static void
inner_main (void *unused, int argc, char **argv)
{
  scm_shell (argc, argv);
}

int
main (int argc, char **argv)
{
  /* Try to install the current locale; remain silent if it fails.  */
  if (setlocale (LC_ALL, "") == NULL)
    /* The 'guix pull'-provided 'guix' includes at least en_US.utf8 so use
       that.  That gives us UTF-8 support for 'scm_to_locale_string', etc.,
       which is always preferable over the C locale.  */
    setlocale (LC_ALL, "en_US.utf8");

  scm_install_gmp_memory_functions = 1;
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
