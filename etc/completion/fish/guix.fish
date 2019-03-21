#
# GNU Guix --- Functional package management for GNU
# Copyright © 2017, 2018 ng0 <ng0@n0.is>
#
# This file is part of GNU Guix.
#
# GNU Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

# Guix commands covered:
# download, pull, system, build, package, size, refresh
# publish, lint, import, hash, graph, gc, environment,
# edit, copy, container, challenge, archive, pack,
# weather

# Existing commands provided by guix as of 2017-11-30:
# archive, challenge, copy, edit, gc, hash, lint, package
# pull, size, weather, build, container, download, environment
# graph, import, pack, publish, refresh, system

# TODO: a rewrite similar to rust.fish

# Use 'command guix' to avoid interactions for aliases.

## To be used later on:
# function __fish_guix_archive
# end
# function __fish_guix_build_file_ls
# end
# function __fish_guix_challenge
# end
# function __fish_guix_container
# end
# function __fish_guix_copy
# end
# function __fish_guix_download
# end
# function __fish_guix_edit
# end
# function __fish_guix_environment
# end
# function __fish_guix_gc
# end
# function __fish_guix_graph
# end
# function __fish_guix_hash
# end
# function __fish_guix_import
# end
# function __fish_guix_lint
# end
# function __fish_guix_package
# end
# function __fish_guix_publish
# end
# function __fish_guix_pull
# end
# function __fish_guix_refresh
# end
# function __fish_guix_size
# end
# function __fish_guix_system
# end

function __fish_guix_needs_command
    set cmd (commandline -opc)
    if [ (count $cmd) -eq 1 ]
        return 0
    else
        set -l skip_next 1
        # Skip first word because it's "guix"
        for c in $cmd[2..-1]
            test $skip_next -eq 0
            and set skip_next 1
            and continue
            switch $c
                # General options that can still take a command
                case "=*"
                    continue
                    # case --asdf
                    #     set skip_next 0
                    #     continue
                    # these behave like commands and everything after them is ignored
                case "--help" "--version"
                    return 1
                    # We assume that any other token that's not an argument to a general option is a command
                case "*"
                    echo $c
                    return 1
            end
        end
        return 0
    end
    return 1
end

function __fish_guix_using_command
    set -l cmd (__fish_guix_needs_command)
    test -z "$cmd"
    and return 1
    contains -- $cmd $argv
    and return 0
end

# general options
complete -f -c guix -s h -l help -d 'Display the manual of a guix command'
complete -f -c guix -s V -l version -d 'Display version information.'

# shared options
#complete -f -c guix -n '__fish_guix_using_command'

#### download
set -l remotecommands format no-check-certificate
complete -f -c guix -n '__fish_guix_needs_command' -a download -d 'Download object from source into the gnu store'
complete -f -c guix -n '__fish_guix_using_command download' -s f -l format -d'Write the hash in the given format'
complete -f -c guix -n '__fish_guix_using_command download' -l no-check-certificate -d 'Do not validate the certificate of HTTPS servers'

#### pull
set -l remotecommands verbose url bootstrap
complete -f -c guix -n '__fish_guix_needs_command' -a pull -d 'Download and deploy the latest version of Guix'
complete -f -c guix -n '__fish_guix_using_command pull' -l verbose -d 'produce verbose output'
complete -f -c guix -n '__fish_guix_using_command pull' -l url -d 'download the Guix tarball from URL'
complete -f -c guix -n '__fish_guix_using_command pull' -l bootstrap -d 'use the bootstrap Guile to build the new Guix'

#### system
set -l remotecommands reconfigure roll-back switch-generation list-generations build container vm vm-image disk-image init extension-graph shepherd-graph load-path keep-failed keep-going dry-run fallback no-substitutes substitutes-urls no-grafts no-build-hook max-silent-time timeout verbosity rounds cores max-jobs derivation on-error image-size no-grub share expose full-boot
complete -f -c guix -n '__fish_guix_needs_command' -a system -d 'Build the operating system declared in FILE according to ACTION.'
complete -f -c guix -n '__fish_guix_using_command system' -l reconfigure -d 'switch to a new operating system configuration'
complete -f -c guix -n '__fish_guix_using_command system' -l roll-back -d 'switch to the previous operating system configuration'
complete -f -c guix -n '__fish_guix_using_command system' -l switch-generation -d 'switch to an existing operating system configuration'
complete -f -c guix -n '__fish_guix_using_command system' -l list-generations -d 'list the system generations'
complete -f -c guix -n '__fish_guix_using_command system' -l build -d 'build the operating system without installing anything'
complete -f -c guix -n '__fish_guix_using_command system' -l container -d 'build a container that shares the host\'s store'
complete -f -c guix -n '__fish_guix_using_command system' -l vm -d 'build a virtual machine image that shares the host\'s store'
complete -f -c guix -n '__fish_guix_using_command system' -l vm-image -d 'build a freestanding virtual machine image'
complete -f -c guix -n '__fish_guix_using_command system' -l disk-image -d 'build a disk image, suitable for a USB stick'
complete -f -c guix -n '__fish_guix_using_command system' -l init -d 'initialize a root file system to run GNU'
complete -f -c guix -n '__fish_guix_using_command system' -l extension-graph -d 'emit the service extension graph in Dot format'
complete -f -c guix -n '__fish_guix_using_command system' -l shepherd-graph -d 'emit the graph of shepherd services in Dot format'
complete -f -c guix -n '__fish_guix_using_command system' -s L -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command system' -a "--load-path=" -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command system' -s K -l keep-failed -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command system' -s k -l keep-going -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command system' -s n -l dry-run -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command system' -l fallback -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command system' -l no-substitutes -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command system' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command system' -l no-grafts -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command system' -l no-build-hook -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command system' -a "--max-silent-time=" -d 'mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command system' -a "--timeout=" -d 'mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command system' -a "--verbosity=" -d 'use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command system' -a --"rounds=" -d 'build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command system' -s c -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command system' -a "--cores=" -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command system' -s M -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command system' -a "--max-jobs=" -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command system' -s d -l derivation -d 'return the derivation of the given system'
complete -f -c guix -n '__fish_guix_using_command system' -a "--on-error=" -d 'apply STRATEGY when an error occurs while reading FILE'
complete -f -c guix -n '__fish_guix_using_command system' -a "--image-size=" -d 'for \'vm-image\', produce an image of SIZE'
complete -f -c guix -n '__fish_guix_using_command system' -l no-grub -d 'for \'init\', do not install GRUB'
complete -f -c guix -n '__fish_guix_using_command system' -a "--share=" -d 'for \'vm\', share host file system according to SPEC'
complete -f -c guix -n '__fish_guix_using_command system' -a "--expose=" -d 'for \'vm\', expose host file system according to SPEC'
complete -f -c guix -n '__fish_guix_using_command system' -l full-boot -d 'for \'vm\', make a full boot sequence'

#### build
set -l remotecommands expression file source sources system target derivations check repair root quiet log-file load-path keep-failed keep-going dry-run fallback no-substitutes substitute-urls no-grafts no-build-hook max-silent-time timeout verbosity rounds cores max-jobs with-source with-input with-graft
complete -f -c guix -n '__fish_guix_needs_command' -a build -d 'Build the given PACKAGE-OR-DERIVATION and return their output paths.'
complete -f -c guix -n '__fish_guix_using_command build' -a "--expression=" -d 'build the package or derivation EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command build' -s f -d 'build the package or derivation that the code within FILE evaluates to' --exclusive --arguments "(ls -ap)"
# The command below is broken:
complete -f -c guix -n '__fish_guix_using_command build' -a '--file=' -a '(ls -ap)' -d 'build the package or derivation that the code within FILE evaluates to'
complete -f -c guix -n '__fish_guix_using_command build' -s S -l source -d 'build the packages\' source derivations'
complete -f -c guix -n '__fish_guix_using_command build' -l sources -d 'build source derivations, TYPE may optionally be one of "package", "all" (default), or "transitive"' -a "package all transitive" -a "package all transitive"
complete -f -c guix -n '__fish_guix_using_command build' -s s -d 'attempt to build for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command build' -a "--system=" -d 'attempt to build for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command build' -a "--target=" -d 'cross-build for TRIPLET--e.g., "armel-linux-gnu"'
complete -f -c guix -n '__fish_guix_using_command build' -s d -l derivations -d 'return the derivation paths of the given packages'
complete -f -c guix -n '__fish_guix_using_command build' -l check -d 'rebuild items to check for non-determinism issues'
complete -f -c guix -n '__fish_guix_using_command build' -l repair -d 'repair the specified items'
complete -f -c guix -n '__fish_guix_using_command build' -s r -d 'make FILE a symlink to the result, and register it as a garbage collector root'
complete -f -c guix -n '__fish_guix_using_command build' -a "--root=" -d 'make FILE a symlink to the result, and register it as a garbage collector root'
complete -f -c guix -n '__fish_guix_using_command build' -s q -l quiet -d 'do not show the build log'
complete -f -c guix -n '__fish_guix_using_command build' -l log-file -d 'return the log file names for the given derivations'
complete -f -c guix -n '__fish_guix_using_command build' -s L -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command build' -a "--load-path=" -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command build' -s K -l keep-failed -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command build' -s k -l keep-going -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command build' -s n -l dry-run -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command build' -l fallback -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command build' -l no-substitutes -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command build' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command build' -l no-grafts -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command build' -l no-build-hook -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command build' -a "--max-silent-time=" -d 'mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command build' -a "--timeout=" -d 'mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command build' -a "--verbosity=" -d 'use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command build' -a "--rounds=" -d 'build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command build' -s c -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command build' -a "--cores=" -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command build' -s M -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command build' -a "--max-jobs=" -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command build' -a "--with-source=" -d 'use SOURCE when building the corresponding package'
complete -f -c guix -n '__fish_guix_using_command build' -a "--with-input=" -d 'PACKAGE=REPLACEMENT .. replace dependency PACKAGE by REPLACEMENT'
complete -f -c guix -n '__fish_guix_using_command build' -a "--with-graft=" -d 'PACKAGE=REPLACEMENT .. graft REPLACEMENT on packages that refer to PACKAGE'

#### package
set -l remotecommands install install-from-expression install-from-file remove upgrade manifest do-no-upgrade roll-back search-paths list-generations delete-generations switch-generation profile bootstrap verbose search list-installed list-available show load-path keep-failed keep-going dry-run fallback no.substitutes substitute-urls no-grafts no-build-hook max-silent-time timenout verbosity rounds cores max-jobs with-source with-input with-graft
complete -f -c guix -n '__fish_guix_needs_command' -a package -d 'Install, remove, or upgrade packages in a single transaction.'
complete -f -c guix -n '__fish_guix_using_command package' -s i -l install -d 'install PACKAGEs'
complete -f -c guix -n '__fish_guix_using_command package' -s e -d 'install the package EXP evaluates to'
complete -f -c guix -n '__fish_guix_using_command package' -a "--install-from-expression=" -d 'install the package EXP evaluates to'
complete -f -c guix -n '__fish_guix_using_command package' -s f -d 'install the package that the code within FILE evaluates to'
complete -f -c guix -n '__fish_guix_using_command package' -a "--install-from-file=" -d 'install the package that the code within FILE evaluates to'
complete -f -c guix -n '__fish_guix_using_command package' -s r -l remove -d 'remove PACKAGEs'
complete -f -c guix -n '__fish_guix_using_command package' -s u -l upgrade -d '[=REGEXP] upgrade all the installed packages matching REGEXP'
complete -f -c guix -n '__fish_guix_using_command package' -s m -d 'create a new profile generation with the manifest from FILE'
complete -f -c guix -n '__fish_guix_using_command package' -a "--manifest=" -d 'create a new profile generation with the manifest from FILE'
complete -f -c guix -n '__fish_guix_using_command package' -l do-not-upgrade -d '[=REGEXP] do not upgrade any packages matching REGEXP'
complete -f -c guix -n '__fish_guix_using_command package' -l roll-back -d 'roll back to the previous generation'
complete -f -c guix -n '__fish_guix_using_command package' -l search-paths -d '[=KIND] display needed environment variable definitions'
complete -f -c guix -n '__fish_guix_using_command package' -s l -l list-generations -d '[=PATTERN] list generations matching PATTERN'
complete -f -c guix -n '__fish_guix_using_command package' -s d -l delete-generations -d '[=PATTERN] delete generations matching PATTERN'
complete -f -c guix -n '__fish_guix_using_command package' -s S -d 'PATTERN switch to a generation matching PATTERN'
complete -f -c guix -n '__fish_guix_using_command package' -a "--switch-generation=" -d 'PATTERN switch to a generation matching PATTERN'
complete -f -c guix -n '__fish_guix_using_command package' -s p -d 'use PROFILE instead of the user\'s default profile'
complete -f -c guix -n '__fish_guix_using_command package' -a "--profile=" -d 'use PROFILE instead of the user\'s default profile'
complete -f -c guix -n '__fish_guix_using_command package' -l bootstrap -d 'use the bootstrap Guile to build the profile'
complete -f -c guix -n '__fish_guix_using_command package' -l verbose -d 'produce verbose output'
complete -f -c guix -n '__fish_guix_using_command package' -s s -d 'REGEXP search in synopsis and description using REGEXP'
complete -f -c guix -n '__fish_guix_using_command package' -a "--search=" -d 'REGEXP search in synopsis and description using REGEXP'
complete -f -c guix -n '__fish_guix_using_command package' -s I -l list-installed -d '[=REGEXP] list installed packages matching REGEXP'
#complete -c guix -n '__fish_guix_using_command package' -s I -l list-installed --exclusive --arguments "(guix package --list-installed)" --description 'List installed packages matching REGEXP'
complete -f -c guix -n '__fish_guix_using_command package' -s A -l list-available -d '[=REGEXP] list available packages matching REGEXP'
complete -f -c guix -n '__fish_guix_using_command package' -a "--show=" -d 'PACKAGE show details about PACKAGE'
complete -f -c guix -n '__fish_guix_using_command package' -s L -d 'DIR prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command package' -a "--load-path=" -d 'DIR prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command package' -s K -l keep-failed -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command package' -s k -l keep-going -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command package' -s n -l dry-run -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command package' -l fallback -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command package' -l no-substitutes -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command package' -a "--substitute-urls=" -d 'URLS fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command package' -l no-grafts -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command package' -l no-build-hook -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command package' -a "--max-silent-time=" -d 'SECONDS mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command package' -a "--timeout=" -d 'SECONDS mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command package' -a "--verbosity=" -d 'LEVEL use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command package' -a "--rounds=" -d 'N build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command package' -s c -d 'N allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command package' -a "--cores=" -d 'N allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command package' -s M -l max-jobs= -d 'N allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command package' -a "--max-jobs=" -d 'N allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command package' -a "--with-source=" -d 'SOURCE use SOURCE when building the corresponding package'
complete -f -c guix -n '__fish_guix_using_command package' -a "--with-input=" -d 'PACKAGE=REPLACEMENT replace dependency PACKAGE by REPLACEMENT'
complete -f -c guix -n '__fish_guix_using_command package' -a "--with-graft=" -d 'PACKAGE=REPLACEMENT graft REPLACEMENT on packages that refer to PACKAGE'

#### size
set -l remotecommands substitute-urls= system= map-file=
complete -f -c guix -n '__fish_guix_needs_command' -a size -d 'Report the size of PACKAGE and its dependencies.'
complete -f -c guix -n '__fish_guix_using_command size' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command size' -a "--system=" -d 'consider packages for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command size' -a "--map-file=" -d 'write to FILE a graphical map of disk usage'

#### refresh
set -l remotecommands expression update select type list-updaters list-dependent key-server gpg key-download
complete -f -c guix -n '__fish_guix_needs_command' -a refresh -d 'Update package definitions to match the latest upstream version'
# FIXME: Too long. When PACKAGE... is given, update only the specified packages. Otherwise update all the packages of the distribution, or the subset thereof specified with `--select`.'
complete -f -c guix -n '__fish_guix_using_command refresh' -a "--expression=" -d 'consider the package EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command refresh' -l update -d 'update source files in place'
#complete -f -c guix -n '__fish_guix_using_command refresh' -l select= -d 'select all the packages in SUBSET, one of `core` or `non-core`' --exclusive --arguments "core non-core"
complete -f -c guix -n '__fish_guix_using_command refresh' -a "--select=" -d 'select all the packages in SUBSET, one of `core` or `non-core`' --exclusive --arguments "core non-core"
complete -f -c guix -n '__fish_guix_using_command refresh' -a "--type=" -d 'restrict to updates from the specified updaters (e.g., \'gnu\')' --exclusive --arguments "gnu gnome kde xorg kernel.org elpa cran bioconductor cpan pypi gem github hackage crate"
complete -f -c guix -n '__fish_guix_using_command refresh' -l list-updaters -d 'list available updaters and exit'
complete -f -c guix -n '__fish_guix_using_command refresh' -l list-dependent -d 'list top-level dependent packages that would need to be rebuilt as a result of upgrading PACKAGE'
complete -f -c guix -n '__fish_guix_using_command refresh' -a "--key-server=" -d 'use HOST as the OpenPGP key server'
complete -f -c guix -n '__fish_guix_using_command refresh' -a "--gpg=" -d 'use COMMAND as the GnuPG 2.x command'
complete -f -c guix -n '__fish_guix_using_command refresh' -a "--key-download=" -d 'handle missing OpenPGP keys according to POLICY.' --exclusive --arguments "always never interactive"

#### publish
set -l remotecommands port= listen= user= compression ttl= repl
complete -f -c guix -n '__fish_guix_needs_command' -a publish -d 'Publish /gnu/store over HTTP.'
complete -f -c guix -n '__fish_guix_using_command publish' -a "--port=" -d 'listen on PORT'
complete -f -c guix -n '__fish_guix_using_command publish' -a "--listen=" -d 'listen on the network interface for HOST'
complete -f -c guix -n '__fish_guix_using_command publish' -a "--user=" -d 'change privileges to USER as soon as possible'
complete -f -c guix -n '__fish_guix_using_command publish' -l compression -d '[=LEVEL] compress archives at LEVEL'
complete -f -c guix -n '__fish_guix_using_command publish' -a "--ttl=" -d 'announce narinfos can be cached for TTL seconds'
complete -f -c guix -n '__fish_guix_using_command publish' -l repl -d '[=PORT] spawn REPL server on PORT'

#### lint
set -l remotecommands checkers list-checkers
complete -f -c guix -n '__fish_guix_needs_command' -a lint -d 'Run a set of checkers on the specificied package.'
complete -f -c guix -n '__fish_guix_using_command lint' -l list-checkers -d 'Display the list of available lint checkers.'
complete -f -c guix -n '__fish_guix_using_command lint' -l checkers -d 'Only run the specified checkers.'
complete -f -c guix -n '__fish_guix_using_command lint' -l description -d 'Validate package descriptions.'
complete -f -c guix -n '__fish_guix_using_command lint' -l gnu-description -d 'Validate synopsis and descriptions of the GNU packages.'
complete -f -c guix -n '__fish_guix_using_command lint' -l inputs-should-be-native -d 'Identify inputs that should be native inputs.'
complete -f -c guix -n '__fish_guix_using_command lint' -l inputs-should-not-be-inputs -d 'Identify inputs that should not be inputs at all.'
complete -f -c guix -n '__fish_guix_using_command lint' -l patch-file-names -d 'Validate file names anda availability of patches.'
complete -f -c guix -n '__fish_guix_using_command lint' -l home-page -d 'Validate home-page URLs'
complete -f -c guix -n '__fish_guix_using_command lint' -l license -d 'Make sure the "license" field is a <license > or a list thereof'
complete -f -c guix -n '__fish_guix_using_command lint' -l source -d 'Validate source URLs'
complete -f -c guix -n '__fish_guix_using_command lint' -l mirror-url -d 'Suggest "mirror://" URLs'
complete -f -c guix -n '__fish_guix_using_command lint' -l source-file-name -d 'Validate file names of sources'
complete -f -c guix -n '__fish_guix_using_command lint' -l derivation -d 'Report failure to compile a package to a derivation'
complete -f -c guix -n '__fish_guix_using_command lint' -l synopsis -d 'Validate package synopses'
complete -f -c guix -n '__fish_guix_using_command lint' -l cve -d 'Check the Common Vulnerabilities and Exposures (CVE) database'
complete -f -c guix -n '__fish_guix_using_command lint' -l formatting -d 'Look for formatting issues in the source'

#### import
set -l remotecommands import gnu nix pypi cpan hackage elpa gem cran crate texlive json
complete -f -c guix -n '__fish_guix_needs_command' -a import -d 'Run IMPORTER with ARGS'
##### import gnu
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a gnu -d 'Return a package declaration template for PACKAGE, a GNU package.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from gnu' -a "--key-download=" -d 'handle missing OpenPGP keys according to POLICY: "always", "never", and "interactive", which is also used when "key-download" is not specified.'
##### import nix
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a nix -d 'Import and convert the Nix expression ATTRIBUTE of NIXPKGS.'
##### import pypi
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a pypi -d 'Import and convert the PyPI package for PACKAGE-NAME.'
##### import cpan
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a cpan -d 'Import and convert the CPAN package for PACKAGE-NAME.'
##### import hackage
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a hackage -d 'Import and convert the Hackage package for PACKAGE-NAME.  If PACKAGE-NAME includes a suffix constituted by a at-sign followed by a numerical version (as used with Guix packages), then a definition for the specified version of the package will be generated.  If no version suffix is pecified, then the generated package definition will correspond to the latest available version.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from hackage' -s e -d 'ALIST specify environment for Cabal evaluation.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from hackage' -a "--cabal-environment=" -d 'ALIST specify environment for Cabal evaluation.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from hackage' -s s -l stdin -d 'Read from standard input.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from hackage' -s t -l no-test-dependencies -d 'don\'t include test-only dependencies.'
##### import elpa
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a elpa -d 'Import the latest package named PACKAGE-NAME from an ELPA repository.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from elpa' -s a -d 'specify the archive repository' --exclusive --arguments "gnu melpa-stable melpa"
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from elpa' -a "--archive=" -d 'specify the archive repository' --exclusive --arguments "gnu melpa-stable melpa"
##### import gem
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a gem -d 'Import and convert the RubyGems package for PACKAGE-NAME.'
##### import cran
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a cran -d 'Import and convert the CRAN package for PACKAGE-NAME.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from cran' -s a -d 'specify the archive repository' --exclusive --arguments "bioconductor cran"
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from cran' -a "--archive=" -d 'specify the archive repository' --exclusive --arguments "bioconductor cran"
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from cran' -l recursive -d 'traverse the dependency graph of the given package recursively and generate package definitions for all those packages that are not yet in Guix'
##### import crate
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a crate -d 'Import and convert the crate.io package for PACKAGE-NAME.'
##### import json
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a json -d 'Import and convert the JSON package definition in PACKAGE-FILE.'
##### import texlive
complete -f -c guix -n '__fish_guix_using_command import; and not __fish_seen_subcommand_from $remotecommands' -a texlive -d 'Import and convert the Texlive package for PACKAGE-NAME.'
complete -f -c guix -n '__fish_guix_using_command import; and __fish_seen_subcommand_from texlive' -s a -l "--archive=" -d 'specify the archive repository'

#### hash
set -l remotecommands exclude-vcs format= recursive
complete -f -c guix -n '__fish_guix_needs_command' -a hash -d 'Return the cryptographic hash of a FILE.'
complete -f -c guix -n '__fish_guix_using_command hash' -s x -l exclude-vcs -d 'Exclude version control directories.'
complete -f -c guix -n '__fish_guix_using_command hash' -s f -d 'Write the hash in the given format.' --exclusive --arguments "nix-base32 base32 base16 hex hexadecimal"
complete -f -c guix -n '__fish_guix_using_command hash' -a "--format=" -d 'Write the hash in the given format.' --exclusive --arguments "nix-base32 base32 base16 hex hexadecimal"
complete -f -c guix -n '__fish_guix_using_command hash' -s r -l recursive -d 'Compute the hash on FILE recursively.'

#### graph
set -l remotecommands backend list-backends type list-types expression
complete -f -c guix -n '__fish_guix_needs_command' -a graph -d 'Emit a Graphviz (dot) representation of the dependencies of a PACKAGE.'
complete -f -c guix -n '__fish_guix_using_command graph' -l backend -d 'Produce a graph with the given backend TYPE'
complete -f -c guix -n '__fish_guix_using_command graph' -l list-backends -d 'list the available graph backends'
complete -f -c guix -n '__fish_guix_using_command graph' -l type -d 'represent nodes of the given TYPE'
complete -f -c guix -n '__fish_guix_using_command graph' -l list-types -d 'list the available graph types'
complete -f -c guix -n '__fish_guix_using_command graph' -l expression -d 'consider the package EXPR evaluates to'

#### gc
set -l remotecommands collect-garbage free-space delete optimize list-dead list-live references requisites referrers verify list-failures clear-failures
complete -f -c guix -n '__fish_guix_needs_command' -a gc -d 'Invoke the garbage collector.'
complete -f -c guix -n '__fish_guix_using_command gc' -s C -d 'collect at least MIN bytes of garbage'
complete -f -c guix -n '__fish_guix_using_command gc' -a "--collect-garbage=" -d 'collect at least MIN bytes of garbage'
complete -f -c guix -n '__fish_guix_using_command gc' -s F -d 'attempt to reach FREE available space in the store'
complete -f -c guix -n '__fish_guix_using_command gc' -a "--free-space=" -d 'attempt to reach FREE available space in the store'
complete -f -c guix -n '__fish_guix_using_command gc' -s d -l delete -d 'attempt to delete PATHS'
complete -f -c guix -n '__fish_guix_using_command gc' -l optimize -d 'optimize the store by deduplicating identical files'
complete -f -c guix -n '__fish_guix_using_command gc' -l list-dead -d 'list dead paths'
complete -f -c guix -n '__fish_guix_using_command gc' -l list-live -d 'list live paths'
complete -f -c guix -n '__fish_guix_using_command gc' -l references -d 'list the references of PATHS'
complete -f -c guix -n '__fish_guix_using_command gc' -s R -l requisites -d 'list the requisites of PATHS'
complete -f -c guix -n '__fish_guix_using_command gc' -l referrers -d 'list the referrers of PATHS'
complete -f -c guix -n '__fish_guix_using_command gc' -l verify -d 'verify the integrity of the store
OPTS is a comma-separated combination of \'repair\' and \'contents\''
complete -f -c guix -n '__fish_guix_using_command gc' -l list-failures -d 'list cached build failures'
complete -f -c guix -n '__fish_guix_using_command gc' -l clear-failures -d 'remove PATHS from the set of cached failures'

#### environment
set -l remotecommands expression load ad-hoc pure search-paths system root container network share expose bootstrap load-path keep-failed keep-going dry-run fallback no-substitutes substitute-urls no-grafts no-build-hook max-silent-time timeout verbosity rounds cores max-jobs
complete -f -c guix -n '__fish_guix_needs_command' -a environment -d 'Build an environment that includes the dependencies of PACKAGE and execute COMMAND or an interactive shell in that environment.'
complete -f -c guix -n '__fish_guix_using_command environment' -s e -d 'Create environment for the package that EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--expression=" -d 'Create environment for the package that EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command environment' -s l -d 'create environment for the package that the code within FILE evaluates to.'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--load=" -d 'create environment for the package that the code within FILE evaluates to.'
complete -f -c guix -n '__fish_guix_using_command environment' -l ad-hoc -d 'include all specified packages in the environment instead of only their inputs'
complete -f -c guix -n '__fish_guix_using_command environment' -l pure -d 'unset existing environment variables'
complete -f -c guix -n '__fish_guix_using_command environment' -l search-paths -d 'display needed environment variable definitions'
complete -f -c guix -n '__fish_guix_using_command environment' -s s -d 'attempt to build for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--system=" -d 'attempt to build for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command environment' -s r -d 'make FILE a symlink to the result, and register it as a garbage collector root'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--root=" -d 'make FILE a symlink to the result, and register it as a garbage collector root'
complete -f -c guix -n '__fish_guix_using_command environment' -s C -l container -d 'run command within an isolated container'
complete -f -c guix -n '__fish_guix_using_command environment' -s N -l network -d 'allow containers to access the network'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--share=" -d 'for containers, share writable host file system according to SPEC'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--expose=" -d 'for containers, expose read-only host file system according to SPEC'
complete -f -c guix -n '__fish_guix_using_command environment' -l bootstrap -d 'use bootstrap binaries to build the environment'
complete -f -c guix -n '__fish_guix_using_command environment' -s L -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--load-path=" -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command environment' -s K -l keep-failed -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command environment' -s k -l keep-going -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command environment' -s n -l dry-run -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command environment' -l fallback -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command environment' -l no-substitutes -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command environment' -l no-grafts -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command environment' -l no-build-hook -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--max-silent-time=" -d 'mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--timeout=" -d 'mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--verbosity=" -d 'use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--rounds=" -d 'build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command environment' -s c -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--cores=" -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command environment' -s M -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command environment' -a "--max-jobs=" -d 'allow at most N build jobs'

#### edit
complete -f -c guix -n '__fish_guix_needs_command' -a edit -d 'Start $VISUAL or $EDITOR to edit the definitions of PACKAGE.'

#### copy
set -l remotecommands to= from= load-path= keep-failed keep-going dry-run fallback no-substitutes substitute-urls= no-grafts no-build-hook max-silent-time= timeout= verbosity= rounds= cores= max-jobs=
complete -f -c guix -n '__fish_guix_needs_command' -a copy -d 'Copy ITEMS to or from the specified host over SSH.'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--to=" -d 'send ITEMS to HOST'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--from=" -d 'receive ITEMS from HOST'
complete -f -c guix -n '__fish_guix_using_command copy' -s L -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--load-path=" -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command copy' -s K -l keep-failed -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command copy' -s k -l keep-going -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command copy' -s n -l dry-run -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command copy' -l fallback -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command copy' -l no-substitutes -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command copy' -l no-grafts -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command copy' -l no-build-hook -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--max-silent-time=" -d 'mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--timeout=" -d 'mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--verbosity=" -d 'use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--rounds=" -d 'build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command copy' -s c -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--cores=" -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command copy' -s M -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command copy' -a "--max-jobs=" -d 'allow at most N build jobs'

#### container
set -l remotecommands exec
complete -f -c guix -n '__fish_guix_needs_command' -a container -d 'Build and manipulate Linux containers.'
complete -f -c guix -n '__fish_guix_using_command container' -l exec -d 'Execute a command inside of an existing container.'

#### challenge
set -l remotecommands substitute-urls verbose
complete -f -c guix -n '__fish_guix_needs_command' -a challenge -d 'Challenge the substitutes for PACKAGE provided by one or more servers.'
complete -f -c guix -n '__fish_guix_using_command challenge' -a "--substitute-urls=" -d 'compare build results with those at URLS'
complete -f -c guix -n '__fish_guix_using_command challenge' -s v -l verbose -d 'show details about successful comparisons'

#### archive
set -l remotecommands export format= recursive import missing extract= generate-key authorize expression= source system= target= load-path= keep-failed keep-going dry-run fallback no-substitutes substitute-urls= no-grafts no-build-hook max-silent-time= timeout= verbosity= rounds= cores= max-jobs=
complete -f -c guix -n '__fish_guix_needs_command' -a archive -d 'Export/import one or more packages from/to the store.'
complete -f -c guix -n '__fish_guix_using_command archive' -l export -d 'export the specified files/packages to stdout'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--format=" -d 'export files/packages in the specified format FMT'
complete -f -c guix -n '__fish_guix_using_command archive' -l recursive -d 'combined with \'--export\', include dependencies'
complete -f -c guix -n '__fish_guix_using_command archive' -l import -d 'import from the archive passed on stdin'
complete -f -c guix -n '__fish_guix_using_command archive' -l missing -d 'print the files from stdin that are missing'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--extract=" -d 'extract the archive on stdin to DIR'
complete -f -c guix -n '__fish_guix_using_command archive' -l generate-key -d 'generate a key pair with the given parameters'
complete -f -c guix -n '__fish_guix_using_command archive' -l authorize -d 'authorize imports signed by the public key on stdin'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--expression=" -d 'build the package or derivation EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command archive' -l source -d 'build the packages\' source derivations'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--system=" -d 'attempt to build for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--target=" -d 'cross-build for TRIPLET--e.g., "armel-linux-gnu"'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--load-path=" -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command archive' -l keep-failed -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command archive' -l keep-going -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command archive' -l dry-run -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command archive' -l fallback -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command archive' -l no-substitutes -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command archive' -l no-grafts -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command archive' -l no-build-hook -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--max-silent-time=" -d 'mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--timeout=" -f -d 'mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--verbosity=" -d 'use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--rounds=" -d 'build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--cores=" -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command archive' -a "--max-jobs=" -d 'allow at most N build jobs'

#### pack
set -l remotecommands --load-path= --keep-failed --keep-going --dry-run --fallback --no-substitutes --substitute-urls= --no-grafts --no-build-hook --max-silent-time= --timeout= --verbosity= --rounds= --cores= --max-jobs= --with-source= --with-input= --with-graft= --format= --expression= --system= --target= --compression= --symlink= --localstatedir --help --version
complete -f -c guix -n '__fish_guix_needs_command' -a pack -d 'Create a bundle of PACKAGE.'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--load-path=" -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command pack' -s L -d 'prepend DIR to the package module search path'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--keep-failed" -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command pack' -s K -d 'keep build tree of failed builds'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--keep-going" -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command pack' -s k -d 'keep going when some of the derivations fail'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--dry-run" -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command pack' -s n -d 'do not build the derivations'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--fallback" -d 'fall back to building when the substituter fails'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--no-substitutes" -d 'build instead of resorting to pre-built substitutes'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--substitute-urls=" -d 'fetch substitute from URLS if they are authorized'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--no-grafts" -d 'do not graft packages'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--no-build-hook" -d 'do not attempt to offload builds via the build hook'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--max-silent-time=" -d 'mark the build as failed after SECONDS of silence'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--timeout=" -d 'mark the build as failed after SECONDS of activity'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--verbosity=" -d 'use the given verbosity LEVEL'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--rounds=" -d 'build N times in a row to detect non-determinism'
complete -f -c guix -n '__fish_guix_using_command pack' -s c -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--cores=" -d 'allow the use of up to N CPU cores for the build'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--max-jobs=" -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command pack' -s M -d 'allow at most N build jobs'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--with-source=" -d 'use SOURCE when building the corresponding package'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--with-input=PACKAGE=REPLACEMENT" -d 'replace dependency PACKAGE by REPLACEMENT'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--with-graft=PACKAGE=REPLACEMENT" -d 'graft REPLACEMENT on packages that refer to PACKAGE'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--format=" -d 'build a pack in the given FORMAT'
complete -f -c guix -n '__fish_guix_using_command pack' -s f -d 'build a pack in the given FORMAT'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--expression=" -d 'consider the package EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command pack' -s e -d 'consider the package EXPR evaluates to'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--system=" -d 'attempt to build for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command pack' -s s -d 'attempt to build for SYSTEM--eg., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--target=" -d 'cross-build for TRIPLET--e.g., "armel-linux-gnu"'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--compression=" -d 'compress using TOOL--e.g., "lzip"'
complete -f -c guix -n '__fish_guix_using_command pack' -s C -d 'compress using TOOL--e.g., "lzip"'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--symlink=" -d 'create symlinks to the profile according to SPEC'
complete -f -c guix -n '__fish_guix_using_command pack' -s S -d 'create symlinks to the profile according to SPEC'
complete -f -c guix -n '__fish_guix_using_command pack' -a "--localstatedir" -d 'include /var/guix in the resulting pack'


## weather
set -l remotecommands substitute-urls manifest system
complete -f -c guix -n '__fish_guix_needs_command' -a weather -d 'Report the availability of substitutes-'
complete -f -c guix -n '__fish_guix_using_command weather' -a "--substitute-urls=" -d 'check for available substitutes at URLS'
complete -f -c guix -n '__fish_guix_using_command weather' -s m -d 'look up substitutes for packages specified in MANIFEST'
complete -f -c guix -n '__fish_guix_using_command weather' -a "--manifest=" -d 'look up substitutes for packages specified in MANIFEST'
complete -f -c guix -n '__fish_guix_using_command weather' -s s -d 'consider substitutes for SYSTEM--e.g., "i686-linux"'
complete -f -c guix -n '__fish_guix_using_command weather' -a "--system=" -d 'consider substitutes for SYSTEM--e.g., "i686-linux"'
