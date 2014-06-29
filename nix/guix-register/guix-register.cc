/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2013, 2014 Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012,
     2013 Eelco Dolstra <eelco.dolstra@logicblox.com>

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

/* This file derives from the implementation of 'nix-store
   --register-validity', by Eelco Dolstra, as found in the Nix package
   manager's src/nix-store/nix-store.cc.  */

#include <config.h>

#include <globals.hh>
#include <local-store.hh>

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>

#include <argp.h>
#include <gcrypt.h>

using namespace nix;

/* Input stream where we read closure descriptions.  */
static std::istream *input = &std::cin;



/* Command-line options.  */

const char *argp_program_version =
  "guix-register (" PACKAGE_NAME ") " PACKAGE_VERSION;
const char *argp_program_bug_address = PACKAGE_BUGREPORT;

static char doc[] =
"guix-register -- register a closure as valid in a store\
\v\
This program is used internally when populating a store with data \
from an existing store.  It updates the new store's database with \
information about which store files are valid, and what their \
references are.";

static const struct argp_option options[] =
  {
    { "prefix", 'p', "DIRECTORY", 0,
      "Open the store that lies under DIRECTORY" },
    { 0, 0, 0, 0, 0 }
  };


/* Prefix of the store being populated.  */
static std::string prefix;

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case 'p':
      {
	prefix = canonPath (arg);
	settings.nixStore = prefix + NIX_STORE_DIR;
	settings.nixDataDir = prefix + NIX_DATA_DIR;
	settings.nixLogDir = prefix + NIX_LOG_DIR;
	settings.nixStateDir = prefix + NIX_STATE_DIR;
	settings.nixDBPath = settings.nixStateDir + "/db";
	break;
      }

    case ARGP_KEY_ARG:
      {
	std::ifstream *file;

	if (state->arg_num >= 2)
	  /* Too many arguments. */
	  argp_usage (state);

	file = new std::ifstream ();
	file->open (arg);

	input = file;
      }
      break;

    default:
      return (error_t) ARGP_ERR_UNKNOWN;
    }

  return (error_t) 0;
}

/* Argument parsing.  */
static struct argp argp = { options, parse_opt, 0, doc };


/* Read from INPUT the description of a closure, and register it as valid in
   STORE.  The expected format on INPUT is that used by #:references-graphs:

     FILE
     DERIVER
     NUMBER-OF-REFERENCES
     REF1
     ...
     REFN

   This is really meant as an internal format.  */
static void
register_validity (LocalStore *store, std::istream &input,
		   bool reregister = true, bool hashGiven = false,
		   bool canonicalise = true)
{
  ValidPathInfos infos;

  while (1)
    {
      ValidPathInfo info = decodeValidPathInfo (input, hashGiven);
      if (info.path == "")
	break;

      if (!prefix.empty ())
	{
	  /* Rewrite the input to refer to the final name, as if we were in a
	     chroot under PREFIX.  */
	  std::string final_prefix (NIX_STORE_DIR "/");
	  info.path = final_prefix + baseNameOf (info.path);
	}

      /* Keep its real path to canonicalize it and compute its hash.  */
      std::string real_path;
      real_path = prefix + "/" + settings.nixStore + "/" + baseNameOf (info.path);

      if (!store->isValidPath (info.path) || reregister)
	{
	  /* !!! races */
	  if (canonicalise)
	    canonicalisePathMetaData (real_path, -1);

	  if (!hashGiven)
	    {
	      HashResult hash = hashPath (htSHA256, real_path);
	      info.hash = hash.first;
	      info.narSize = hash.second;
	    }
	  infos.push_back (info);
	}
    }

  store->registerValidPaths (infos);
}


int
main (int argc, char *argv[])
{
  /* Initialize libgcrypt, which is indirectly used.  */
  if (!gcry_check_version (GCRYPT_VERSION))
    {
      fprintf (stderr, "error: libgcrypt version mismatch\n");
      exit (EXIT_FAILURE);
    }

  /* Tell Libgcrypt that initialization has completed, as per the Libgcrypt
     1.6.0 manual (although this does not appear to be strictly needed.)  */
  gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);

  /* Honor the environment variables, and initialize the settings.  */
  settings.processEnvironment ();

  try
    {
      argp_parse (&argp, argc, argv, 0, 0, 0);

      /* Instantiate the store.  This creates any missing directories among
	 'settings.nixStore', 'settings.nixDBPath', etc.  */
      LocalStore store;

      if (!prefix.empty ())
	/* Under the --prefix tree, the final name of the store will be
	   NIX_STORE_DIR.  Set it here so that the database uses file names
	   prefixed by NIX_STORE_DIR and not PREFIX + NIX_STORE_DIR.  */
	settings.nixStore = NIX_STORE_DIR;

      register_validity (&store, *input);
    }
  catch (std::exception &e)
    {
      fprintf (stderr, "error: %s\n", e.what ());
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
