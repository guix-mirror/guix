/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2012, 2013  Ludovic Courtès <ludo@gnu.org>

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

/* An OpenSSL-like interface to GNU libgcrypt cryptographic hash
   functions.  */

#pragma once
#include <gcrypt.h>
#include <unistd.h>

extern "C" {

struct guix_hash_context
{
  gcry_md_hd_t md_handle;
};

extern void guix_hash_init (struct guix_hash_context *ctx, int algo);
extern void guix_hash_update (struct guix_hash_context *ctx, const void *buffer,
			      size_t len);
extern void guix_hash_final (void *resbuf, struct guix_hash_context *ctx,
			     int algo);

}
