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

#include <gcrypt-hash.hh>

#define MD5_CTX guix_hash_context

static inline void
MD5_Init (struct MD5_CTX *ctx)
{
  guix_hash_init (ctx, GCRY_MD_MD5);
}

#define MD5_Update guix_hash_update

static inline void
MD5_Final (void *resbuf, struct MD5_CTX *ctx)
{
  guix_hash_final (ctx, ctx, GCRY_MD_MD5);
}
