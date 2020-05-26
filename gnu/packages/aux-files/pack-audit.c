/* GNU Guix --- Functional package management for GNU
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

/* This file implements part of the GNU ld.so audit interface.  It is used by
   the "fakechroot" engine of the 'guix pack -RR' wrappers to make sure the
   loader looks for shared objects under the "fake" root directory.  */

#define _GNU_SOURCE 1

#include <link.h>

#include <error.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* The pseudo root directory and store that we are relocating to.  */
static const char *root_directory;
static char *store;

/* The original store, "/gnu/store" by default.  */
static const char original_store[] = "@STORE_DIRECTORY@";

/* Like 'malloc', but abort if 'malloc' returns NULL.  */
static void *
xmalloc (size_t size)
{
  void *result = malloc (size);
  assert (result != NULL);
  return result;
}

unsigned int
la_version (unsigned int v)
{
  if (v != LAV_CURRENT)
    error (1, 0, "cannot handle interface version %u", v);

  root_directory = getenv ("FAKECHROOT_BASE");
  if (root_directory == NULL)
    error (1, 0, "'FAKECHROOT_BASE' is not set");

  store = xmalloc (strlen (root_directory) + sizeof original_store);
  strcpy (store, root_directory);
  strcat (store, original_store);

  return v;
}

/* Return NAME, a shared object file name, relocated under STORE.  This
   function is called by the loader whenever it looks for a shared object.  */
char *
la_objsearch (const char *name, uintptr_t *cookie, unsigned int flag)
{
  char *result;

  if (strncmp (name, original_store,
	       sizeof original_store - 1) == 0)
    {
      size_t len = strlen (name) - sizeof original_store
	+ strlen (store) + 1;
      result = xmalloc (len);
      strcpy (result, store);
      strcat (result, name + sizeof original_store - 1);
    }
  else
    result = strdup (name);

  return result;
}
