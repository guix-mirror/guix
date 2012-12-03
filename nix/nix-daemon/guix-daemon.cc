/* Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
   Copyright (C) 2012  Ludovic Courtès <ludo@gnu.org>

   This file is part of Guix.

   Guix is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   Guix is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Guix.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include <types.hh>
#include "shared.hh"
#include <globals.hh>

#include <stdlib.h>
#include <argp.h>

/* Variables used by `nix-daemon.cc'.  */
volatile ::sig_atomic_t blockInt;
char **argvSaved;

using namespace nix;

/* Entry point in `nix-daemon.cc'.  */
extern void run (Strings args);


/* Command-line options.  */

const char *argp_program_version =
  "guix-daemon (" PACKAGE_NAME ") " PACKAGE_VERSION;
const char *argp_program_bug_address = PACKAGE_BUGREPORT;

static char doc[] =
"guix-daemon -- perform derivation builds and store accesses\
\v\
This program is a daemon meant to run in the background.  It serves \
requests sent over a Unix-domain socket.  It accesses the store, and \
builds derivations on behalf of its clients.";

#define GUIX_OPT_SYSTEM 1
#define GUIX_OPT_DISABLE_CHROOT 2
#define GUIX_OPT_DISABLE_LOG_COMPRESSION 3

static const struct argp_option options[] =
  {
    { "system", GUIX_OPT_SYSTEM, "SYSTEM", 0,
      "Assume SYSTEM as the current system type" },
    { "build-cores", 'C', "N", 0,
      "Use N CPU cores to build each derivation; 0 means as many as available" },
    { "max-jobs", 'M', "N", 0,
      "Allow at most N build jobs" },
    { "disable-chroot", GUIX_OPT_DISABLE_CHROOT, 0, 0,
      "Disable chroot builds" },
    { "disable-log-compression", GUIX_OPT_DISABLE_LOG_COMPRESSION, 0, 0,
      "Disable compression of the build logs" },
    { 0, 0, 0, 0, 0 }
  };

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case GUIX_OPT_DISABLE_CHROOT:
      settings.useChroot = false;
      break;
    case GUIX_OPT_DISABLE_LOG_COMPRESSION:
      settings.compressLog = false;
      break;
    case 'C':
      settings.buildCores = atoi (arg);
      break;
    case 'M':
      settings.maxBuildJobs = atoi (arg);
      break;
    case GUIX_OPT_SYSTEM:
      settings.thisSystem = arg;
      break;
    default:
      return ARGP_ERR_UNKNOWN;
    }

  return 0;
}

/* Argument parsing.  */
static struct argp argp = { options, parse_opt, 0, doc };



int
main (int argc, char *argv[])
{
  Strings nothing;

  settings.useChroot = true;
  settings.processEnvironment ();

  argp_parse (&argp, argc, argv, 0, 0, 0);

  argvSaved = argv;
  run (nothing);
}
