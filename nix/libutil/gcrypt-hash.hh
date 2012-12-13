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

extern void guix_hash_init (struct guix_hash_context *ctx, gcry_md_algo_t algo);
extern void guix_hash_update (struct guix_hash_context *ctx, const void *buffer,
			      size_t len);
extern void guix_hash_final (void *resbuf, struct guix_hash_context *ctx,
			     gcry_md_algo_t algo);

}
