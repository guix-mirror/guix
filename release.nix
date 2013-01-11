/* GNU Guix --- Functional package management for GNU
   Copyright (C) 2012  Ludovic Courtès <ludo@gnu.org>

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

/* Release file to build Guix with Nix.  Useful to bootstrap Guix on
   Guix-enabled Hydra instances.  */

let
  nixpkgs = <nixpkgs>;

  buildOutOfSourceTree = true;
  succeedOnFailure = true;
  keepBuildDirectory = true;

  # The Guile used to bootstrap the whole thing.  It's normally
  # downloaded by the build system, but here we download it via a
  # fixed-output derivation and stuff it into the build tree.
  bootstrap_guile =
    let pkgs = import nixpkgs {}; in {
      i686 = pkgs.fetchurl {
        url = http://www.fdn.fr/~lcourtes/software/guix/packages/i686-linux/20121219/guile-2.0.7.tar.xz;
        sha256 = "45d1f9bfb9e4531a8f1c5a105f7ab094cd481b8a179ccc63cbabb73ce6b8437f";
      };

      x86_64 = pkgs.fetchurl {
        url = http://www.fdn.fr/~lcourtes/software/guix/packages/x86_64-linux/20121219/guile-2.0.7.tar.xz;
        sha256 = "953fbcc8db6e310626be79b67319cf4141dc23b296447952a99d95425b3a4dc1";
      };
    };

  jobs = {
    tarball =
      let pkgs = import nixpkgs {}; in
      pkgs.releaseTools.sourceTarball {
        name = "guix-tarball";
        src = <guix>;
        buildInputs = with pkgs; [ guile sqlite bzip2 git libgcrypt ];
        buildNativeInputs = with pkgs; [ texinfo gettext cvs pkgconfig ];
        preAutoconf = ''git config submodule.nix.url "${<nix>}"'';
        configureFlags =
          [ "--with-libgcrypt-prefix=${pkgs.libgcrypt}"
            "--localstatedir=/nix/var"
          ];
      };

    build =
      { system ? builtins.currentSystem }:

      let pkgs = import nixpkgs { inherit system; }; in
      pkgs.releaseTools.nixBuild {
        name = "guix";
        buildInputs = with pkgs; [ guile sqlite bzip2 libgcrypt ];
        buildNativeInputs = [ pkgs.pkgconfig ];
        src = jobs.tarball;
        configureFlags =
          [ "--with-libgcrypt-prefix=${pkgs.libgcrypt}"
            "--localstatedir=/nix/var"
          ];

        preBuild =
          # Use our pre-downloaded bootstrap tarballs instead of letting
          # the build system download it over and over again.
          '' mkdir -p distro/packages/bootstrap/{i686,x86_64}-linux
             cp -v "${bootstrap_guile.i686}" \
               distro/packages/bootstrap/i686-linux/guile-2.0.7.tar.xz
             cp -v "${bootstrap_guile.x86_64}" \
               distro/packages/bootstrap/x86_64-linux/guile-2.0.7.tar.xz
          '';

        inherit succeedOnFailure keepBuildDirectory
          buildOutOfSourceTree;
      };


    build_disable_daemon =
      { system ? builtins.currentSystem }:

      let
        pkgs = import nixpkgs { inherit system; };
        build = jobs.build { inherit system; };
      in
        pkgs.lib.overrideDerivation build ({ configureFlags, ... }: {
          configureFlags = configureFlags ++ [ "--disable-daemon" ];
          buildInputs = with pkgs; [ guile nixUnstable pkgconfig ];

          # Since we need to talk to a running daemon, we need to escape
          # the chroot.
          preConfigure = "export NIX_REMOTE=daemon";
          __noChroot = true;
        });

    # Jobs to test the distro.
    distro = {
      hello =
        { system ? builtins.currentSystem }:

        let
          pkgs = import nixpkgs { inherit system; };
          guix = jobs.build { inherit system; };
        in
          # XXX: We have no way to tell the Nix code to swallow the .drv
          # produced by `guix-build', so we have a pointless indirection
          # here.  This could be worked around by generating Nix code
          # from the .drv, and importing that.
          pkgs.releaseTools.nixBuild {
            src = null;
            name = "guix-hello";
            phases = "buildPhase";
            buildPhase = "${guix}/bin/guix-build --no-substitutes hello | tee $out";
            __noChroot = true;
          };
    };
  };
in
  jobs
