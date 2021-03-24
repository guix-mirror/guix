/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2006, 2010, 2012, 2014 Eelco Dolstra <e.dolstra@tudelft.nl>

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
#include <sys/socket.h>
#include <sys/un.h>
#include <netdb.h>
#include <strings.h>
#include <exception>
#include <iostream>

#include <libintl.h>
#include <locale.h>

/* Variables used by `nix-daemon.cc'.  */
volatile ::sig_atomic_t blockInt;
char **argvSaved;

using namespace nix;

/* Entry point in `nix-daemon.cc'.  */
extern void run (const std::vector<int> &);


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
#define GUIX_OPT_TIMEOUT 18
#define GUIX_OPT_MAX_SILENT_TIME 19
#define GUIX_OPT_LOG_COMPRESSION 20
#define GUIX_OPT_DISCOVER 21

static const struct argp_option options[] =
  {
    { "system", GUIX_OPT_SYSTEM, n_("SYSTEM"), 0,
      n_("assume SYSTEM as the current system type") },
    { "cores", 'c', n_("N"), 0,
      n_("use N CPU cores to build each derivation; 0 means as many as available")
    },
    { "max-jobs", 'M', n_("N"), 0,
      n_("allow at most N build jobs") },
    { "timeout", GUIX_OPT_TIMEOUT, n_("SECONDS"), 0,
      n_("mark builds as failed after SECONDS of activity") },
    { "max-silent-time", GUIX_OPT_MAX_SILENT_TIME, n_("SECONDS"), 0,
      n_("mark builds as failed after SECONDS of silence") },
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
    { "no-offload", GUIX_OPT_NO_BUILD_HOOK, 0, 0,
      n_("do not attempt to offload builds") },
    { "no-build-hook", GUIX_OPT_NO_BUILD_HOOK, 0,
      OPTION_HIDDEN,				  // deprecated
      n_("do not attempt to offload builds") },
    { "cache-failures", GUIX_OPT_CACHE_FAILURES, 0, 0,
      n_("cache build failures") },
    { "rounds", GUIX_OPT_BUILD_ROUNDS, "N", 0,
      n_("build each derivation N times in a row") },
    { "lose-logs", GUIX_OPT_LOSE_LOGS, 0, 0,
      n_("do not keep build logs") },
    { "disable-log-compression", GUIX_OPT_DISABLE_LOG_COMPRESSION, 0,
      OPTION_HIDDEN,				  // deprecated
      n_("disable compression of the build logs") },
    { "log-compression", GUIX_OPT_LOG_COMPRESSION, "TYPE", 0,
      n_("use the specified compression type for build logs") },
    { "discover", GUIX_OPT_DISCOVER, "yes/no", OPTION_ARG_OPTIONAL,
      n_("use substitute servers discovered on the local network") },

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


/* Default port for '--listen' on TCP/IP.  */
#define DEFAULT_GUIX_PORT "44146"

/* List of '--listen' options.  */
static std::list<std::string> listen_options;

static bool useDiscover = false;

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
    case GUIX_OPT_LOG_COMPRESSION:
      if (strcmp (arg, "none") == 0)
	settings.logCompression = COMPRESSION_NONE;
      else if (strcmp (arg, "gzip") == 0)
	settings.logCompression = COMPRESSION_GZIP;
#if HAVE_BZLIB_H
      else if (strcmp (arg, "bzip2") == 0)
	settings.logCompression = COMPRESSION_BZIP2;
#endif
      else
	{
	  fprintf (stderr, _("error: %s: unknown compression type\n"), arg);
	  exit (EXIT_FAILURE);
	}
      break;
    case GUIX_OPT_DISABLE_LOG_COMPRESSION:
      settings.logCompression = COMPRESSION_NONE;
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
      listen_options.push_back (arg);
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
    case GUIX_OPT_DISCOVER:
      useDiscover = string_to_bool (arg);
      settings.set ("discover", useDiscover ? "true" : "false");
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
    case GUIX_OPT_TIMEOUT:
      settings.set ("build-timeout", arg);
      break;
    case GUIX_OPT_MAX_SILENT_TIME:
      settings.set ("build-max-silent-time", arg);
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


static int
open_unix_domain_socket (const char *file)
{
  /* Create and bind to a Unix domain socket. */
  AutoCloseFD fdSocket = socket (PF_UNIX, SOCK_STREAM, 0);
  if (fdSocket == -1)
    throw SysError (_("cannot create Unix domain socket"));

  createDirs (dirOf (file));

  /* Urgh, sockaddr_un allows path names of only 108 characters.
     So chdir to the socket directory so that we can pass a
     relative path name. */
  if (chdir (dirOf (file).c_str ()) == -1)
    throw SysError (_("cannot change current directory"));
  Path fileRel = "./" + baseNameOf (file);

  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  if (fileRel.size () >= sizeof (addr.sun_path))
    throw Error (format (_("socket file name '%1%' is too long")) % fileRel);
  strcpy (addr.sun_path, fileRel.c_str ());

  unlink (file);

  /* Make sure that the socket is created with 0666 permission
     (everybody can connect --- provided they have access to the
     directory containing the socket). */
  mode_t oldMode = umask (0111);
  int res = bind (fdSocket, (struct sockaddr *) &addr, sizeof addr);
  umask (oldMode);
  if (res == -1)
    throw SysError (format (_("cannot bind to socket '%1%'")) % file);

  if (chdir ("/") == -1) /* back to the root */
    throw SysError (_("cannot change current directory"));

  if (listen (fdSocket, 5) == -1)
    throw SysError (format (_("cannot listen on socket '%1%'")) % file);

  return fdSocket.borrow ();
}

/* Return a listening socket for ADDRESS, which has the given LENGTH.  */
static int
open_inet_socket (const struct sockaddr *address, socklen_t length)
{
  AutoCloseFD fd = socket (address->sa_family, SOCK_STREAM, 0);
  if (fd == -1)
    throw SysError (_("cannot create TCP socket"));

  int res = bind (fd, address, length);
  if (res == -1)
    throw SysError (_("cannot bind TCP socket"));

  if (listen (fd, 5) == -1)
    throw SysError (format (_("cannot listen on TCP socket")));

  return fd.borrow ();
}

/* Return a list of file descriptors of listening sockets.  */
static std::vector<int>
listening_sockets (const std::list<std::string> &options)
{
  std::vector<int> result;

  if (options.empty ())
    {
      /* Open the default Unix-domain socket.  */
      auto fd = open_unix_domain_socket (settings.nixDaemonSocketFile.c_str ());
      result.push_back (fd);
      return result;
    }

  /* Open the user-specified sockets.  */
  for (const std::string& option: options)
    {
      if (option[0] == '/')
	{
	  /* Assume OPTION is the file name of a Unix-domain socket.  */
	  settings.nixDaemonSocketFile = canonPath (option);
	  int fd =
	    open_unix_domain_socket (settings.nixDaemonSocketFile.c_str ());
	  result.push_back (fd);
	}
      else
	{
	  /* Assume OPTIONS has the form "HOST" or "HOST:PORT".  */
	  auto colon = option.find_last_of (":");
	  auto host = colon == std::string::npos
	    ? option : option.substr (0, colon);
	  auto port = colon == std::string::npos
	    ? DEFAULT_GUIX_PORT
	    : option.substr (colon + 1, option.size () - colon - 1);

	  struct addrinfo *res, hints;

	  memset (&hints, '\0', sizeof hints);
	  hints.ai_socktype = SOCK_STREAM;
	  hints.ai_flags = AI_NUMERICSERV | AI_ADDRCONFIG;

	  int err = getaddrinfo (host.c_str(), port.c_str (),
				 &hints, &res);

	  if (err != 0)
	    throw Error(format ("failed to look up '%1%': %2%")
			% option % gai_strerror (err));

	  printMsg (lvlDebug, format ("listening on '%1%', port '%2%'")
		    % host % port);

	  /* XXX: Pick the first result, RES.  */
	  result.push_back (open_inet_socket (res->ai_addr,
					      res->ai_addrlen));

	  freeaddrinfo (res);
	}
    }

  return result;
}


int
main (int argc, char *argv[])
{
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

  /* Default to using as many cores as possible and one job at a time.  */
  settings.buildCores = 0;
  settings.maxBuildJobs = 1;

  argvSaved = argv;

  try
    {
      settings.processEnvironment ();

      /* Enable substitutes by default.  */
      settings.set ("build-use-substitutes", "true");

      /* Use our substitute server by default.  */
      settings.set ("substitute-urls", GUIX_SUBSTITUTE_URLS);

#ifdef HAVE_DAEMON_OFFLOAD_HOOK
      /* Use 'guix offload' for distributed builds by default.  */
      settings.useBuildHook = true;
#else
      /* We are not installing any build hook, so disable it.  */
      settings.useBuildHook = false;
#endif

      argp_parse (&argp, argc, argv, 0, 0, 0);

      auto sockets = listening_sockets (listen_options);

      /* Effect all the changes made via 'settings.set'.  */
      settings.update ();
      printMsg(lvlDebug,
	       format ("build log compression: %1%") % settings.logCompression);

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

      if (useDiscover)
      {
        Strings args;

        args.push_back("guix");
        args.push_back("discover");

        startProcess([&]() {
          execv(settings.guixProgram.c_str(), stringsToCharPtrs(args).data());
        });
      }

      printMsg (lvlDebug,
		format ("automatic deduplication set to %1%")
		% settings.autoOptimiseStore);

      printMsg (lvlDebug,
		format ("listening on `%1%'") % settings.nixDaemonSocketFile);

      run (sockets);
    }
  catch (std::exception &e)
    {
      fprintf (stderr, _("error: %s\n"), e.what ());
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;				  /* never reached */
}
