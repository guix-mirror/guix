/* Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
   Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>

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

/* Release file to build Guix with Nix.  Useful to bootstrap Guix on
   Guix-enabled Hydra instances.  */

let
  nixpkgs = <nixpkgs>;

  jobs = {
    tarball =
      let pkgs = import nixpkgs {}; in
      pkgs.releaseTools.sourceTarball {
        name = "guix-tarball";
        src = <guix>;
        buildInputs = with pkgs; [ guile ];
        buildNativeInputs = with pkgs; [ gettext cvs pkgconfig ];
        configureFlags =
          [ "--with-nixpkgs=${nixpkgs}" "--with-nix-prefix=${pkgs.nix}" ];
      };

    build =
      { system ? builtins.currentSystem }:

      let pkgs = import nixpkgs { inherit system; }; in
      pkgs.releaseTools.nixBuild {
        name = "guix";
        buildInputs = [ pkgs.guile ];
        buildNativeInputs = [ pkgs.pkgconfig ];
        src = jobs.tarball;
        configureFlags =
          [ "--with-nixpkgs=${nixpkgs}" "--with-nix-prefix=${pkgs.nix}" ];

        # XXX: The test suite cannot be run currently in the chroot, because
        # it expects to have a running nix-worker to talk to.
        doCheck = false;
      };
  };
in
  jobs
