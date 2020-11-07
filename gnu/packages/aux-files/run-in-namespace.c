/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>

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

/* Make the given @WRAPPED_PROGRAM@ relocatable by executing it in a separate
   mount namespace where the store is mounted in its right place.

   We would happily do that in Scheme using 'call-with-container'.  However,
   this very program needs to be relocatable, so it needs to be statically
   linked, which complicates things (Guile's modules can hardly be "linked"
   into a single executable.)  */

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sched.h>
#include <sys/mount.h>
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <string.h>
#include <assert.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/syscall.h>
#include <sys/prctl.h>

/* Whether we're building the ld.so/libfakechroot wrapper.  */
#define HAVE_EXEC_WITH_LOADER						\
  (defined PROGRAM_INTERPRETER) && (defined LOADER_AUDIT_MODULE)	\
  && (defined FAKECHROOT_LIBRARY)

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

/* Concatenate DIRECTORY, a slash, and FILE.  Return the result, which the
   caller must eventually free.  */
static char *
concat (const char *directory, const char *file)
{
  char *result = xmalloc (strlen (directory) + 2 + strlen (file));

  strcpy (result, directory);
  strcat (result, "/");
  strcat (result, file);
  return result;
}

static void
mkdir_p (const char *directory)
{
  if (strcmp (directory, "/") != 0)
    {
      char *parent = dirname (strdupa (directory));
      mkdir_p (parent);
      int err = mkdir (directory, 0700);
      if (err < 0 && errno != EEXIST)
	assert_perror (errno);
    }
}

static void
rm_rf (const char *directory)
{
  DIR *stream = opendir (directory);

  for (struct dirent *entry = readdir (stream);
       entry != NULL;
       entry = readdir (stream))
    {
      if (strcmp (entry->d_name, ".") == 0
	  || strcmp (entry->d_name, "..") == 0)
	continue;

      char *full = concat (directory, entry->d_name);

      int err = unlink (full);
      if (err < 0)
	{
	  if (errno == EISDIR)
	    /* Recurse (we expect a shallow directory structure so there's
	       little risk of stack overflow.)  */
	    rm_rf (full);
	  else
	    assert_perror (errno);
	}

      free (full);
    }

  closedir (stream);

  int err = rmdir (directory);
  if (err < 0 && errno != ENOENT)
    assert_perror (errno);
}

/* Make TARGET a bind-mount of SOURCE.  Take into account ENTRY's type, which
   corresponds to SOURCE.  */
static int
bind_mount (const char *source, const struct dirent *entry,
	    const char *target)
{
  if (entry->d_type == DT_DIR)
    {
      int err = mkdir (target, 0700);
      if (err != 0)
	return err;
    }
  else
    close (open (target, O_WRONLY | O_CREAT));

  return mount (source, target, "none",
		MS_BIND | MS_REC | MS_RDONLY, NULL);
}

#if HAVE_EXEC_WITH_LOADER

/* Make TARGET a symlink to SOURCE.  */
static int
make_symlink (const char *source, const struct dirent *entry,
	      const char *target)
{
  return symlink (source, target);
}

#endif

/* Mirror with FIRMLINK all the top-level entries in SOURCE to TARGET.  */
static void
mirror_directory (const char *source, const char *target,
		  int (* firmlink) (const char *, const struct dirent *,
				    const char *))
{
  DIR *stream = opendir (source);

  for (struct dirent *entry = readdir (stream);
       entry != NULL;
       entry = readdir (stream))
    {
      /* XXX: Some file systems may not report a useful 'd_type'.  Ignore them
	 for now.  */
      assert (entry->d_type != DT_UNKNOWN);

      if (strcmp (entry->d_name, ".") == 0
	  || strcmp (entry->d_name, "..") == 0)
	continue;

      char *abs_source = concat (source, entry->d_name);
      char *new_entry = concat (target, entry->d_name);

      if (entry->d_type == DT_LNK)
	{
	  char target[PATH_MAX];

	  ssize_t result = readlink (abs_source, target, sizeof target - 1);
	  if (result > 0)
	    {
	      target[result] = '\0';
	      int err = symlink (target, new_entry);
	      if (err < 0)
		assert_perror (errno);
	    }
	}
      else
	{
	  /* Create the mount point.  */
	  int err = firmlink (abs_source, entry, new_entry);

	  /* It used to be that only directories could be bind-mounted.  Thus,
	     keep going if we fail to bind-mount a non-directory entry.
	     That's OK because regular files in the root file system are
	     usually uninteresting.  */
	  if (err != 0 && entry->d_type != DT_DIR)
	    assert_perror (errno);

	  free (new_entry);
	  free (abs_source);
	}
    }

  closedir (stream);
}

/* Write the user/group ID map for PID to FILE, mapping ID to itself.  See
   user_namespaces(7).  */
static void
write_id_map (pid_t pid, const char *file, int id)
{
  char id_map_file[100];
  snprintf (id_map_file, sizeof id_map_file, "/proc/%d/%s", pid, file);

  char id_map[100];

  /* Map root and the current user.  */
  int len = snprintf (id_map, sizeof id_map, "%d %d 1\n", id, id);
  int fd = open (id_map_file, O_WRONLY);
  if (fd < 0)
    assert_perror (errno);

  int n = write (fd, id_map, len);
  if (n < 0)
    assert_perror (errno);

  close (fd);
}

/* Disallow setgroups(2) for PID.  */
static void
disallow_setgroups (pid_t pid)
{
  char file[100];

  snprintf (file, sizeof file, "/proc/%d/setgroups", pid);

  int fd = open (file, O_WRONLY);
  if (fd < 0)
    assert_perror (errno);

  int err = write (fd, "deny", 5);
  if (err < 0)
    assert_perror (errno);

  close (fd);
}

/* Run the wrapper program in a separate mount user namespace.  Return only
   upon failure.  */
static void
exec_in_user_namespace (const char *store, int argc, char *argv[])
{
  /* Spawn @WRAPPED_PROGRAM@ in a separate namespace where STORE is
     bind-mounted in the right place.  */
  int err, is_tmpfs;
  char *new_root = mkdtemp (strdup ("/tmp/guix-exec-XXXXXX"));
  char *new_store = concat (new_root, original_store);
  char *cwd = get_current_dir_name ();

  /* Become the new parent of grand-children when their parent dies.  */
  prctl (PR_SET_CHILD_SUBREAPER, 1);

  /* Optionally, make NEW_ROOT a tmpfs.  That way, if we have to leave it
     behind because there are sub-processes still running when this wrapper
     exits, it's OK.  */
  err = mount ("none", new_root, "tmpfs", 0, NULL);
  is_tmpfs = (err == 0);

  /* Create a child with separate namespaces and set up bind-mounts from
     there.  That way, bind-mounts automatically disappear when the child
     exits, which simplifies cleanup for the parent.  Note: clone is more
     convenient than fork + unshare since the parent can directly write
     the child uid_map/gid_map files.  */
  pid_t child = syscall (SYS_clone, SIGCHLD | CLONE_NEWNS | CLONE_NEWUSER,
			 NULL, NULL, NULL);
  switch (child)
    {
    case 0:
      /* Note: Due to <https://bugzilla.kernel.org/show_bug.cgi?id=183461>
	 we cannot make NEW_ROOT a tmpfs (which would have saved the need
	 for 'rm_rf'.)  */
      mirror_directory ("/", new_root, bind_mount);
      mkdir_p (new_store);
      err = mount (store, new_store, "none", MS_BIND | MS_REC | MS_RDONLY,
		   NULL);
      if (err < 0)
	assert_perror (errno);

      chdir (new_root);
      err = chroot (new_root);
      if (err < 0)
	assert_perror (errno);

      /* Change back to where we were before chroot'ing.  */
      chdir (cwd);

      int err = execv ("@WRAPPED_PROGRAM@", argv);
      if (err < 0)
	assert_perror (errno);
      break;

    case -1:
      /* Failure: user namespaces not supported.  */
      fprintf (stderr, "%s: error: 'clone' failed: %m\n", argv[0]);
      rm_rf (new_root);
      free (new_root);
      break;

    default:
      {
	/* Map the current user/group ID in the child's namespace (the
	   default is to get the "overflow UID", i.e., the UID of
	   "nobody").  We must first disallow 'setgroups' for that
	   process.  */
	disallow_setgroups (child);
	write_id_map (child, "uid_map", getuid ());
	write_id_map (child, "gid_map", getgid ());

	int status, status_other;
	waitpid (child, &status, 0);

	chdir ("/");				  /* avoid EBUSY */
	if (is_tmpfs)
	  {
	    /* NEW_ROOT lives on in child processes and we no longer need it
	       to exist as an empty directory in the global namespace.  */
	    umount (new_root);
	    rmdir (new_root);
	  }
	/* Check whether there are child processes left.  If there are none,
	   we can remove NEW_ROOT just fine.  Conversely, if there are
	   processes left (for example because this wrapper's child forked),
	   we have to leave NEW_ROOT behind so that those processes can still
	   access their root file system (XXX).  */
	else if (waitpid (-1 , &status_other, WNOHANG) == -1)
	  rm_rf (new_root);

	free (new_root);

	if (WIFEXITED (status))
	  exit (WEXITSTATUS (status));
	else
	  /* Abnormal termination cannot really be reproduced, so exit
	     with 255.  */
	  exit (255);
      }
    }
}


#ifdef PROOT_PROGRAM

/* Execute the wrapped program with PRoot, passing it ARGC and ARGV, and
   "bind-mounting" STORE in the right place.  */
static void
exec_with_proot (const char *store, int argc, char *argv[])
{
  int proot_specific_argc = 4;
  int proot_argc = argc + proot_specific_argc;
  char *proot_argv[proot_argc + 1], *proot;
  char bind_spec[strlen (store) + 1 + sizeof original_store];

  strcpy (bind_spec, store);
  strcat (bind_spec, ":");
  strcat (bind_spec, original_store);

  proot = concat (store, PROOT_PROGRAM);

  proot_argv[0] = proot;
  proot_argv[1] = "-b";
  proot_argv[2] = bind_spec;
  proot_argv[3] = "@WRAPPED_PROGRAM@";

  for (int i = 0; i < argc; i++)
    proot_argv[i + proot_specific_argc] = argv[i + 1];

  proot_argv[proot_argc] = NULL;

  /* Seccomp support seems to invariably lead to segfaults; disable it by
     default.  */
  setenv ("PROOT_NO_SECCOMP", "1", 0);

  int err = execv (proot, proot_argv);
  if (err < 0)
    assert_perror (errno);
}

#endif


#if HAVE_EXEC_WITH_LOADER

/* Traverse PATH, a NULL-terminated string array, and return a colon-separated
   search path where each item of PATH has been relocated to STORE.  The
   result is malloc'd.  */
static char *
relocated_search_path (const char *path[], const char *store)
{
  char *new_path;
  size_t size = 0;

  for (size_t i = 0; path[i] != NULL; i++)
    size += strlen (store) + strlen (path[i]) + 1;  /* upper bound */

  new_path = xmalloc (size + 1);
  new_path[0] = '\0';

  for (size_t i = 0; path[i] != NULL; i++)
    {
      if (strncmp (path[i], original_store,
		   sizeof original_store - 1) == 0)
	{
	  strcat (new_path, store);
	  strcat (new_path, path[i] + sizeof original_store - 1);
	}
      else
	strcat (new_path, path[i]);	  /* possibly $ORIGIN */

      strcat (new_path, ":");
    }

  new_path[strlen (new_path) - 1] = '\0'; /* Remove trailing colon.  */

  return new_path;
}

/* Concatenate PATH1 and PATH2 with a colon in between.  The result is
   potentially malloc'd.  */
static char *
concat_paths (const char *path1, const char *path2)
{
  if (path1[0] == '\0')
    return (char *) path2;
  else
    {
      char *result = xmalloc (strlen (path1) + strlen (path2) + 2);
      strcpy (result, path1);
      strcat (result, ":");
      strcat (result, path2);
      return result;
    }
}

/* Execute the wrapped program by invoking the loader (ld.so) directly,
   passing it the audit module and preloading libfakechroot.so.  */
static void
exec_with_loader (const char *store, int argc, char *argv[])
{
  static const char *audit_library_path[] = LOADER_AUDIT_RUNPATH;
  char *loader = concat (store,
			 PROGRAM_INTERPRETER + sizeof original_store);
  size_t loader_specific_argc = 8;
  size_t loader_argc = argc + loader_specific_argc;
  char *loader_argv[loader_argc + 1];
  loader_argv[0] = argv[0];
  loader_argv[1] = "--audit";
  loader_argv[2] = concat (store,
			   LOADER_AUDIT_MODULE + sizeof original_store);

  /* The audit module depends on libc.so and libgcc_s.so so honor
     AUDIT_LIBRARY_PATH.  Additionally, honor $LD_LIBRARY_PATH if set.  */
  loader_argv[3] = "--library-path";
  loader_argv[4] =
    concat_paths (getenv ("LD_LIBRARY_PATH") ?: "",
		  relocated_search_path (audit_library_path, store));

  loader_argv[5] = "--preload";
  loader_argv[6] = concat (store,
			   FAKECHROOT_LIBRARY + sizeof original_store);
  loader_argv[7] = concat (store,
			   "@WRAPPED_PROGRAM@" + sizeof original_store);

  for (size_t i = 0; i < argc; i++)
    loader_argv[i + loader_specific_argc] = argv[i + 1];

  loader_argv[loader_argc] = NULL;

  /* Set up the root directory.  */
  int err;
  char *new_root = mkdtemp (strdup ("/tmp/guix-exec-XXXXXX"));
  mirror_directory ("/", new_root, make_symlink);

  /* 'mirror_directory' created a symlink for the ancestor of ORIGINAL_STORE,
     typically "/gnu".  Remove that entry so we can create NEW_STORE
     below.  */
  const char *slash = strchr (original_store + 1, '/');
  const char *top = slash != NULL
    ? strndupa (original_store, slash - original_store)
    : original_store;
  char *new_store_top = concat (new_root, top);
  unlink (new_store_top);

  /* Now create the store under NEW_ROOT.  */
  char *new_store = concat (new_root, original_store);
  char *new_store_parent = dirname (strdup (new_store));
  mkdir_p (new_store_parent);
  err = symlink (store, new_store);
  if (err < 0)
    assert_perror (errno);

#ifdef GCONV_DIRECTORY
  /* Tell libc where to find its gconv modules.  This is necessary because
     gconv uses non-interposable 'open' calls.  */
  char *gconv_path = concat (store,
			     GCONV_DIRECTORY + sizeof original_store);
  setenv ("GCONV_PATH", gconv_path, 1);
  free (gconv_path);
#endif

  setenv ("FAKECHROOT_BASE", new_root, 1);

  /* Become the new parent of grand-children when their parent dies.  */
  prctl (PR_SET_CHILD_SUBREAPER, 1);

  pid_t child = fork ();
  switch (child)
    {
    case 0:
      err = execv (loader, loader_argv);
      if (err < 0)
	assert_perror (errno);
      exit (EXIT_FAILURE);
      break;

    case -1:
      assert_perror (errno);
      exit (EXIT_FAILURE);
      break;

    default:
      {
  	int status, status_other;
	waitpid (child, &status, 0);

	/* If there are child processes still running, leave NEW_ROOT around
	   so they can still access it.  XXX: In that case NEW_ROOT is left
	   behind.  */
	if (waitpid (-1 , &status_other, WNOHANG) == -1)
	  {
	    chdir ("/");			  /* avoid EBUSY */
	    rm_rf (new_root);
	  }

	free (new_root);
	close (2);			/* flushing stderr should be silent */

	if (WIFEXITED (status))
	  exit (WEXITSTATUS (status));
	else
	  /* Abnormal termination cannot really be reproduced, so exit
	     with 255.  */
	  exit (255);
      }
    }
}

#endif


/* Execution engines.  */

struct engine
{
  const char *name;
  void (* exec) (const char *, int, char **);
};

static void
buffer_stderr (void)
{
  static char stderr_buffer[4096];
  setvbuf (stderr, stderr_buffer, _IOFBF, sizeof stderr_buffer);
}

/* The default engine: choose a robust method.  */
static void
exec_default (const char *store, int argc, char *argv[])
{
  /* Buffer stderr so that nothing's displayed if 'exec_in_user_namespace'
     fails but 'exec_with_proot' works.  */
  buffer_stderr ();

  exec_in_user_namespace (store, argc, argv);
#ifdef PROOT_PROGRAM
  exec_with_proot (store, argc, argv);
#endif
}

/* The "performance" engine: choose performance over robustness.  */
static void
exec_performance (const char *store, int argc, char *argv[])
{
  buffer_stderr ();

  exec_in_user_namespace (store, argc, argv);
#if HAVE_EXEC_WITH_LOADER
  exec_with_loader (store, argc, argv);
#endif
}

/* List of supported engines.  */
static const struct engine engines[] =
  {
   { "default", exec_default },
   { "performance", exec_performance },
   { "userns", exec_in_user_namespace },
#ifdef PROOT_PROGRAM
   { "proot", exec_with_proot },
#endif
#if HAVE_EXEC_WITH_LOADER
   { "fakechroot", exec_with_loader },
#endif
   { NULL, NULL }
  };

/* Return the "execution engine" to use.  */
static const struct engine *
execution_engine (void)
{
  const char *str = getenv ("GUIX_EXECUTION_ENGINE");

  if (str == NULL)
    str = "default";

 try:
  for (const struct engine *engine = engines;
       engine->name != NULL;
       engine++)
    {
      if (strcmp (engine->name, str) == 0)
	return engine;
    }

  fprintf (stderr, "%s: unsupported Guix execution engine; ignoring\n",
	   str);
  str = "default";
  goto try;
}


int
main (int argc, char *argv[])
{
  ssize_t size;
  char self[PATH_MAX];
  size = readlink ("/proc/self/exe", self, sizeof self - 1);
  assert (size > 0);

  /* SELF is something like "/home/ludo/.local/gnu/store/…-foo/bin/ls" and we
     want to extract "/home/ludo/.local/gnu/store".  */
  size_t index = strlen (self)
    - strlen (WRAPPER_PROGRAM) + strlen (original_store);
  char *store = strdup (self);
  store[index] = '\0';

  struct stat statbuf;

  /* If STORE is already at the "right" place, we can execute
     @WRAPPED_PROGRAM@ right away.  This is not just an optimization: it's
     needed when running one of these wrappers from within an unshare'd
     namespace, because 'unshare' fails with EPERM in that context.  */
  if (strcmp (store, original_store) != 0
      && lstat ("@WRAPPED_PROGRAM@", &statbuf) != 0)
    {
      const struct engine *engine = execution_engine ();
      engine->exec (store, argc, argv);

      /* If we reach this point, that's because ENGINE failed to do the
	 job.  */
      fprintf (stderr, "\
This may be because \"user namespaces\" are not supported on this system.\n\
Consequently, we cannot run '@WRAPPED_PROGRAM@',\n\
unless you move it to the '@STORE_DIRECTORY@' directory.\n\
\n\
Please refer to the 'guix pack' documentation for more information.\n");
      return EXIT_FAILURE;
    }

  /* The executable is available under @STORE_DIRECTORY@, so we can now
     execute it.  */
  int err = execv ("@WRAPPED_PROGRAM@", argv);
  if (err < 0)
    assert_perror (errno);

  return EXIT_FAILURE;
}
