/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2018 Ludovic Courtès <ludo@gnu.org>

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

/* Concatenate DIRECTORY, a slash, and FILE.  Return the result, which the
   caller must eventually free.  */
static char *
concat (const char *directory, const char *file)
{
  char *result = malloc (strlen (directory) + 2 + strlen (file));
  assert (result != NULL);

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

/* Bind mount all the top-level entries in SOURCE to TARGET.  */
static void
bind_mount (const char *source, const char *target)
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
	  if (entry->d_type == DT_DIR)
	    {
	      int err = mkdir (new_entry, 0700);
	      if (err != 0)
		assert_perror (errno);
	    }
	  else
	    close (open (new_entry, O_WRONLY | O_CREAT));

	  int err = mount (abs_source, new_entry, "none",
			   MS_BIND | MS_REC | MS_RDONLY, NULL);

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
    - strlen ("@WRAPPED_PROGRAM@")
    + strlen ("@STORE_DIRECTORY@");
  char *store = strdup (self);
  store[index] = '\0';

  struct stat statbuf;

  /* If STORE is already at the "right" place, we can execute
     @WRAPPED_PROGRAM@ right away.  This is not just an optimization: it's
     needed when running one of these wrappers from within an unshare'd
     namespace, because 'unshare' fails with EPERM in that context.  */
  if (strcmp (store, "@STORE_DIRECTORY@") != 0
      && lstat ("@WRAPPED_PROGRAM@", &statbuf) != 0)
    {
      /* Spawn @WRAPPED_PROGRAM@ in a separate namespace where STORE is
	 bind-mounted in the right place.  */
      int err;
      char *new_root = mkdtemp (strdup ("/tmp/guix-exec-XXXXXX"));
      char *new_store = concat (new_root, "@STORE_DIRECTORY@");
      char *cwd = get_current_dir_name ();

      pid_t child = fork ();
      switch (child)
	{
	case 0:
	  /* Unshare namespaces in the child and set up bind-mounts from
	     there.  That way, bind-mounts automatically disappear when the
	     child exits, which simplifies cleanup for the parent.  */
	  err = unshare (CLONE_NEWNS | CLONE_NEWUSER);
	  if (err < 0)
	    {
	      fprintf (stderr, "%s: error: 'unshare' failed: %m\n", argv[0]);
	      fprintf (stderr, "\
This may be because \"user namespaces\" are not supported on this system.\n\
Consequently, we cannot run '@WRAPPED_PROGRAM@',\n\
unless you move it to the '@STORE_DIRECTORY@' directory.\n\
\n\
Please refer to the 'guix pack' documentation for more information.\n");
	      return EXIT_FAILURE;
	    }

	  /* Note: Due to <https://bugzilla.kernel.org/show_bug.cgi?id=183461>
	     we cannot make NEW_ROOT a tmpfs (which would have saved the need
	     for 'rm_rf'.)  */
	  bind_mount ("/", new_root);
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
	  break;
	case -1:
	  assert_perror (errno);
	  break;
	default:
	  {
	    int status;
	    waitpid (child, &status, 0);
	    chdir ("/");			  /* avoid EBUSY */
	    rm_rf (new_root);
	    free (new_root);
	    exit (status);
	  }
	}
    }

  /* The executable is available under @STORE_DIRECTORY@, so we can now
     execute it.  */
  int err = execv ("@WRAPPED_PROGRAM@", argv);
  if (err < 0)
    assert_perror (errno);

  return EXIT_FAILURE;
}
