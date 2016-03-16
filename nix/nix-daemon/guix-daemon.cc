/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>

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

#include <config.h>

#include <types.hh>
#include "shared.hh"
#include <globals.hh>
#include <util.hh>

#include <gcrypt.h>

#include <stdlib.h>
#include <argp.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <strings.h>
#include <exception>

#include <libintl.h>
#include <locale.h>

/* Variables used by `nix-daemon.cc'.  */
volatile ::sig_atomic_t blockInt;
char **argvSaved;

using namespace nix;

/* Entry point in `nix-daemon.cc'.  */
extern void run (Strings args);


/* Command-line options.  */

#define n_(str)  str
#define _(str)   gettext (str)
static const char guix_textdomain[] = "guix";


const char *argp_program_version =
  "guix-daemon (" PACKAGE_NAME ") " PACKAGE_VERSION;
const char *argp_program_bug_address = PACKAGE_BUGREPORT;

static char doc[] =
  n_("guix-daemon -- perform derivation builds and store accesses")
  "\v\n"
  n_("This program is a daemon meant to run in the background.  It serves \
requests sent over a Unix-domain socket.  It accesses the store, and \
builds derivations on behalf of its clients.");

#define GUIX_OPT_SYSTEM 1
#define GUIX_OPT_DISABLE_CHROOT 2
#define GUIX_OPT_BUILD_USERS_GROUP 3
#define GUIX_OPT_CACHE_FAILURES 4
#define GUIX_OPT_LOSE_LOGS 5
#define GUIX_OPT_DISABLE_LOG_COMPRESSION 6
#define GUIX_OPT_DISABLE_DEDUPLICATION 7
#define GUIX_OPT_IMPERSONATE_LINUX_26 8
#define GUIX_OPT_DEBUG 9
#define GUIX_OPT_CHROOT_DIR 10
#define GUIX_OPT_LISTEN 11
#define GUIX_OPT_NO_SUBSTITUTES 12
#define GUIX_OPT_SUBSTITUTE_URLS 13
#define GUIX_OPT_NO_BUILD_HOOK 14
#define GUIX_OPT_GC_KEEP_OUTPUTS 15
#define GUIX_OPT_GC_KEEP_DERIVATIONS 16
#define GUIX_OPT_BUILD_ROUNDS 17

static const struct argp_option options[] =
  {
    { "system", GUIX_OPT_SYSTEM, n_("SYSTEM"), 0,
      n_("assume SYSTEM as the current system type") },
    { "cores", 'c', n_("N"), 0,
      n_("use N CPU cores to build each derivation; 0 means as many as available")
    },
    { "max-jobs", 'M', n_("N"), 0,
      n_("allow at most N build jobs") },
    { "disable-chroot", GUIX_OPT_DISABLE_CHROOT, 0, 0,
      n_("disable chroot builds") },
    { "chroot-directory", GUIX_OPT_CHROOT_DIR, n_("DIR"), 0,
      n_("add DIR to the build chroot") },
    { "build-users-group", GUIX_OPT_BUILD_USERS_GROUP, n_("GROUP"), 0,
      n_("perform builds as a user of GROUP") },
    { "no-substitutes", GUIX_OPT_NO_SUBSTITUTES, 0, 0,
      n_("do not use substitutes") },
    { "substitute-urls", GUIX_OPT_SUBSTITUTE_URLS, n_("URLS"), 0,
      n_("use URLS as the default list of substitute providers") },
    { "no-build-hook", GUIX_OPT_NO_BUILD_HOOK, 0, 0,
      n_("do not use the 'build hook'") },
    { "cache-failures", GUIX_OPT_CACHE_FAILURES, 0, 0,
      n_("cache build failures") },
    { "rounds", GUIX_OPT_BUILD_ROUNDS, "N", 0,
      n_("build each derivation N times in a row") },
    { "lose-logs", GUIX_OPT_LOSE_LOGS, 0, 0,
      n_("do not keep build logs") },
    { "disable-log-compression", GUIX_OPT_DISABLE_LOG_COMPRESSION, 0, 0,
      n_("disable compression of the build logs") },

    /* '--disable-deduplication' was known as '--disable-store-optimization'
       up to Guix 0.7 included, so keep the alias around.  */
    { "disable-deduplication", GUIX_OPT_DISABLE_DEDUPLICATION, 0, 0,
      n_("disable automatic file \"deduplication\" in the store") },
    { "disable-store-optimization", GUIX_OPT_DISABLE_DEDUPLICATION, 0,
      OPTION_ALIAS | OPTION_HIDDEN, NULL },

    { "impersonate-linux-2.6", GUIX_OPT_IMPERSONATE_LINUX_26, 0,
#ifdef HAVE_SYS_PERSONALITY_H
      0,
#else
      OPTION_HIDDEN,
#endif
      n_("impersonate Linux 2.6")
    },
    { "gc-keep-outputs", GUIX_OPT_GC_KEEP_OUTPUTS,
      "yes/no", OPTION_ARG_OPTIONAL,
      n_("tell whether the GC must keep outputs of live derivations") },
    { "gc-keep-derivations", GUIX_OPT_GC_KEEP_DERIVATIONS,
      "yes/no", OPTION_ARG_OPTIONAL,
      n_("tell whether the GC must keep derivations corresponding \
to live outputs") },

    { "listen", GUIX_OPT_LISTEN, n_("SOCKET"), 0,
      n_("listen for connections on SOCKET") },
    { "debug", GUIX_OPT_DEBUG, 0, 0,
      n_("produce debugging output") },
    { 0, 0, 0, 0, 0 }
  };


/* Convert ARG to a Boolean value, or throw an error if it does not denote a
   Boolean.  */
static bool
string_to_bool (const char *arg, bool dflt = true)
{
  if (arg == NULL)
    return dflt;
  else if (strcasecmp (arg, "yes") == 0)
    return true;
  else if (strcasecmp (arg, "no") == 0)
    return false;
  else
    throw nix::Error (format ("'%1%': invalid Boolean value") % arg);
}

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  switch (key)
    {
    case GUIX_OPT_DISABLE_CHROOT:
      settings.useChroot = false;
      break;
    case GUIX_OPT_CHROOT_DIR:
      {
	std::string chroot_dirs;

	chroot_dirs = settings.get ("build-extra-chroot-dirs",
				    (std::string) "");
	if (chroot_dirs == "")
	  chroot_dirs = arg;
	else
	  chroot_dirs = chroot_dirs + " " + arg;
	settings.set("build-extra-chroot-dirs", chroot_dirs);
	break;
      }
    case GUIX_OPT_DISABLE_LOG_COMPRESSION:
      settings.compressLog = false;
      break;
    case GUIX_OPT_BUILD_USERS_GROUP:
      settings.buildUsersGroup = arg;
      break;
    case GUIX_OPT_DISABLE_DEDUPLICATION:
      settings.autoOptimiseStore = false;
      break;
    case GUIX_OPT_CACHE_FAILURES:
      settings.cacheFailure = true;
      break;
    case GUIX_OPT_BUILD_ROUNDS:
      {
	char *end;
	unsigned long n = strtoul (arg, &end, 10);
	if (end != arg + strlen (arg))
	  {
	    fprintf (stderr, _("error: %s: invalid number of rounds\n"), arg);
	    exit (EXIT_FAILURE);
	  }
	settings.set ("build-repeat", std::to_string (std::max (0UL, n - 1)));
	break;
      }
    case GUIX_OPT_IMPERSONATE_LINUX_26:
      settings.impersonateLinux26 = true;
      break;
    case GUIX_OPT_LOSE_LOGS:
      settings.keepLog = false;
      break;
    case GUIX_OPT_LISTEN:
      try
	{
	  settings.nixDaemonSocketFile = canonPath (arg);
	}
      catch (std::exception &e)
	{
	  fprintf (stderr, _("error: %s\n"), e.what ());
	  exit (EXIT_FAILURE);
	}
      break;
    case GUIX_OPT_SUBSTITUTE_URLS:
      settings.set ("substitute-urls", arg);
      break;
    case GUIX_OPT_NO_SUBSTITUTES:
      settings.set ("build-use-substitutes", "false");
      break;
    case GUIX_OPT_NO_BUILD_HOOK:
      settings.useBuildHook = false;
      break;
    case GUIX_OPT_DEBUG:
      verbosity = lvlDebug;
      break;
    case GUIX_OPT_GC_KEEP_OUTPUTS:
      settings.gcKeepOutputs = string_to_bool (arg);
      break;
    case GUIX_OPT_GC_KEEP_DERIVATIONS:
      settings.gcKeepDerivations = string_to_bool (arg);
      break;
    case 'c':
      settings.set ("build-cores", arg);
      break;
    case 'M':
      settings.set ("build-max-jobs", arg);
      break;
    case GUIX_OPT_SYSTEM:
      settings.thisSystem = arg;
      break;
    default:
      return (error_t) ARGP_ERR_UNKNOWN;
    }

  return (error_t) 0;
}

/* Argument parsing.  */
static const struct argp argp =
  {
    options, parse_opt,
    NULL, doc,
    NULL, NULL,					  // children and help_filter
    guix_textdomain
  };



int
main (int argc, char *argv[])
{
  static const Strings nothing;

  setlocale (LC_ALL, "");
  bindtextdomain (guix_textdomain, LOCALEDIR);
  textdomain (guix_textdomain);

  /* Initialize libgcrypt.  */
  if (!gcry_check_version (GCRYPT_VERSION))
    {
      fprintf (stderr, _("error: libgcrypt version mismatch\n"));
      exit (EXIT_FAILURE);
    }

  /* Tell Libgcrypt that initialization has completed, as per the Libgcrypt
     1.6.0 manual (although this does not appear to be strictly needed.)  */
  gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);

  /* Set the umask so that the daemon does not end up creating group-writable
     files, which would lead to "suspicious ownership or permission" errors.
     See <http://lists.gnu.org/archive/html/bug-guix/2013-07/msg00033.html>.  */
  umask (S_IWGRP | S_IWOTH);

#ifndef HAVE_CHROOT
# error chroot is assumed to be available
#endif

  /* Always use chroots by default.  */
  settings.useChroot = true;

  /* Turn automatic deduplication on by default.  */
  settings.autoOptimiseStore = true;

  /* Default to using as many cores as possible.  */
  settings.buildCores = 0;

  argvSaved = argv;

  try
    {
      settings.processEnvironment ();

      /* Hackily help 'local-store.cc' find our 'guix-authenticate' program, which
	 is known as 'OPENSSL_PATH' here.  */
      std::string search_path;
      search_path = settings.nixLibexecDir;
      if (getenv ("PATH") != NULL)
	{
	  search_path += ":";
	  search_path += getenv ("PATH");
	}

      setenv ("PATH", search_path.c_str (), 1);

      /* Use our substituter by default.  */
      settings.substituters.clear ();
      settings.set ("build-use-substitutes", "true");

      /* Use our substitute server by default.  */
      settings.set ("substitute-urls", GUIX_SUBSTITUTE_URLS);

#ifdef HAVE_DAEMON_OFFLOAD_HOOK
      /* Use our build hook for distributed builds by default.  */
      settings.useBuildHook = true;
      if (getenv ("NIX_BUILD_HOOK") == NULL)
	{
	  std::string build_hook;

	  build_hook = settings.nixLibexecDir + "/guix/offload";
	  setenv ("NIX_BUILD_HOOK", build_hook.c_str (), 1);
	}
#else
      /* We are not installing any build hook, so disable it.  */
      settings.useBuildHook = false;
#endif

      argp_parse (&argp, argc, argv, 0, 0, 0);

      /* Effect all the changes made via 'settings.set'.  */
      settings.update ();

      if (settings.useSubstitutes)
	{
	  string subs = getEnv ("NIX_SUBSTITUTERS", "default");

	  if (subs == "default")
	    {
	      string subst =
		settings.nixLibexecDir + "/guix/substitute";
	      setenv ("NIX_SUBSTITUTERS", subst.c_str (), 1);
	    }
	}
      else
	/* Clear the substituter list to make sure nothing ever gets
	   substituted, regardless of the client's settings.  */
	setenv ("NIX_SUBSTITUTERS", "", 1);

      /* Effect the $NIX_SUBSTITUTERS change.  */
      settings.update ();

      if (geteuid () == 0 && settings.buildUsersGroup.empty ())
	fprintf (stderr, _("warning: daemon is running as root, so \
using `--build-users-group' is highly recommended\n"));

      if (settings.useChroot)
	{
	  std::string chroot_dirs;

	  chroot_dirs = settings.get ("build-extra-chroot-dirs",
				      (std::string) "");
	  printMsg (lvlDebug,
		    format ("extra chroot directories: '%1%'") % chroot_dirs);
	}

      printMsg (lvlDebug,
		format ("automatic deduplication set to %1%")
		% settings.autoOptimiseStore);

      printMsg (lvlDebug,
		format ("listening on `%1%'") % settings.nixDaemonSocketFile);

      run (nothing);
    }
  catch (std::exception &e)
    {
      fprintf (stderr, _("error: %s\n"), e.what ());
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;				  /* never reached */
}
