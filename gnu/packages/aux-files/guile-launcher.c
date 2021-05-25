/* GNU Guix --- Functional package management for GNU
   Copyright 1996-1997,2000-2001,2006,2008,2011,2013,2018,2020,2021
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
   complain about locale issues and arranges to reduce startup time by
   ignoring GUILE_LOAD_PATH and GUILE_LOAD_COMPILED_PATH until it has
   booted.  */

#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <libguile.h>

/* Saved values of GUILE_LOAD_PATH and GUILE_LOAD_COMPILED_PATH.  */
static const char *load_path, *load_compiled_path;

static void
inner_main (void *unused, int argc, char **argv)
{
  if (load_path != NULL)
    {
      setenv ("GUILE_LOAD_PATH", load_path, 1);
      SCM load_path_var =
	scm_c_public_lookup ("guile", "%load-path");
      SCM addition =
	scm_parse_path (scm_from_locale_string (load_path), SCM_EOL);
      scm_variable_set_x (load_path_var,
			  scm_append
			  (scm_list_2 (scm_variable_ref (load_path_var),
				       addition)));
    }

  if (load_compiled_path != NULL)
    {
      setenv ("GUILE_LOAD_COMPILED_PATH", load_compiled_path, 1);
      SCM load_compiled_path_var =
	scm_c_public_lookup ("guile", "%load-compiled-path");
      SCM addition =
	scm_parse_path (scm_from_locale_string (load_compiled_path), SCM_EOL);
      scm_variable_set_x (load_compiled_path_var,
			  scm_append
			  (scm_list_2 (scm_variable_ref (load_compiled_path_var),
				       addition)));
    }

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

  const char *str;
  str = getenv ("GUILE_LOAD_PATH");
  load_path = str != NULL ? strdup (str) : NULL;
  str = getenv ("GUILE_LOAD_COMPILED_PATH");
  load_compiled_path = str ? strdup (str) : NULL;

  unsetenv ("GUILE_LOAD_PATH");
  unsetenv ("GUILE_LOAD_COMPILED_PATH");

#if !SCM_ENABLE_MINI_GMP
  /* XXX: On Guile < 3.0.6 and Guile built without its bundled mini-GMP, do
     not let GMP allocate via libgc as this can lead to memory corruption in
     GnuTLS/Nettle since Nettle also uses GMP:
     <https://issues.guix.gnu.org/46330>.  */
  scm_install_gmp_memory_functions = 0;
#endif

  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
