#!/bin/sh
# GNU Guix --- Functional package management for GNU
# Copyright © 2017 sharlatan <sharlatanus@gmail.com>
# Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
# Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

# We require Bash but for portability we'd rather not use /bin/bash or
# /usr/bin/env in the shebang, hence this hack.
if [ "x$BASH_VERSION" = "x" ]
then
    exec bash "$0" "$@"
fi

set -e

[ "$UID" -eq 0 ] || { echo "This script must be run as root."; exit 1; }

REQUIRE=(
    "dirname"
    "readlink"
    "wget"
    "gpg"
    "grep"
    "which"
    "sed"
    "sort"
    "getent"
    "mktemp"
    "rm"
    "chmod"
    "uname"
    "groupadd"
    "tail"
    "tr"
)

PAS=$'[ \033[32;1mPASS\033[0m ] '
ERR=$'[ \033[31;1mFAIL\033[0m ] '
INF="[ INFO ] "

DEBUG=0
GNU_URL="https://ftp.gnu.org/gnu/guix/"
OPENPGP_SIGNING_KEY_ID="3CE464558A84FDC69DB40CFB090B11993D9AEBB5"

# This script needs to know where root's home directory is.  However, we
# cannot simply use the HOME environment variable, since there is no guarantee
# that it points to root's home directory.
ROOT_HOME="$(echo ~root)"

# ------------------------------------------------------------------------------
#+UTILITIES

_err()
{ # All errors go to stderr.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_msg()
{ # Default message to stdout.
    printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
}

_debug()
{
    if [ "${DEBUG}" = '1' ]; then
        printf "[%s]: %s\n" "$(date +%s.%3N)" "$1"
    fi
}


chk_require()
{ # Check that every required command is available.
    declare -a cmds
    declare -a warn

    cmds=(${1})

    _debug "--- [ $FUNCNAME ] ---"

    for c in ${cmds[@]}; do
        command -v "$c" &>/dev/null || warn+=("$c")
    done

    [ "${#warn}" -ne 0 ] &&
        { _err "${ERR}Missing commands: ${warn[*]}.";
          return 1; }
    
    _msg "${PAS}verification of required commands completed"

    gpg --list-keys ${OPENPGP_SIGNING_KEY_ID} >/dev/null 2>&1 || (
        _err "${ERR}Missing OpenPGP public key.  Fetch it with this command:"
        echo "  gpg --keyserver pool.sks-keyservers.net --recv-keys ${OPENPGP_SIGNING_KEY_ID}"
        exit 1
    )
}

chk_term()
{ # Check for ANSI terminal for color printing.
    local ansi_term

    if [ -t 2 ]; then
        if [ "${TERM+set}" = 'set' ]; then
            case "$TERM" in
                xterm*|rxvt*|urxvt*|linux*|vt*|eterm*|screen*)
                    ansi_term=true
                    ;;
                *)
                    ansi_term=false
                    ERR="[ FAIL ] "
                    PAS="[ PASS ] "
                    ;;
            esac
        fi
    fi
}

chk_init_sys()
{ # Return init system type name.
    if [[ $(/sbin/init --version 2>/dev/null) =~ upstart ]]; then
        _msg "${INF}init system is: upstart"
        INIT_SYS="upstart"
        return 0
    elif [[ $(systemctl) =~ -\.mount ]]; then
        _msg "${INF}init system is: systemd"
        INIT_SYS="systemd"
        return 0
    elif [[ -f /etc/init.d/cron && ! -h /etc/init.d/cron ]]; then
        _msg "${INF}init system is: sysv-init"
        INIT_SYS="sysv-init"
        return 0
    else
        INIT_SYS="NA"
        _err "${ERR}Init system could not be detected."
    fi
}

chk_sys_arch()
{ # Check for operating system and architecture type.
    local os
    local arch

    os="$(uname -s)"
    arch="$(uname -m)"

    case "$arch" in
        i386 | i486 | i686 | i786 | x86)
            local arch=i686
            ;;
        x86_64 | x86-64 | x64 | amd64)
            local arch=x86_64
            ;;
        aarch64)
            local arch=aarch64
            ;;
	armv7l)
	    local arch=armhf
	    ;;
        *)
            _err "${ERR}Unsupported CPU type: ${arch}"
            exit 1
    esac

    case "$os" in
        Linux | linux)
            local os=linux
            ;;
        *)
            _err "${ERR}Your operation system (${os}) is not supported."
            exit 1
    esac

    ARCH_OS="${arch}-${os}"
}

# ------------------------------------------------------------------------------
#+MAIN

guix_get_bin_list()
{ # Scan GNU archive and save list of binaries
    local gnu_url="$1"
    local -a bin_ver_ls
    local latest_ver
    local default_ver

    _debug "--- [ $FUNCNAME ] ---"

    # Filter only version and architecture
    bin_ver_ls=("$(wget -qO- "$gnu_url" \
        | sed -n -e 's/.*guix-binary-\([0-9.]*\)\..*.tar.xz.*/\1/p' \
        | sort -Vu)")

    latest_ver="$(echo "$bin_ver_ls" \
                       | grep -oP "([0-9]{1,2}\.){2}[0-9]{1,2}" \
                       | tail -n1)"

    default_ver="guix-binary-${latest_ver}.${ARCH_OS}"

    if [[ "${#bin_ver_ls}" -ne "0" ]]; then
        _msg "${PAS}Release for your system: ${default_ver}"
    else
        _err "${ERR}Could not obtain list of Guix releases."
        exit 1
    fi

    # Use default to download according to the list and local ARCH_OS.
    BIN_VER="$default_ver"
}

guix_get_bin()
{ # Download and verify binary package.
    local url="$1"
    local bin_ver="$2"
    local dl_path="$3"

    _debug "--- [ $FUNCNAME ] ---"

    _msg "${INF}Downloading Guix release archive"

    wget --help | grep -q '\--show-progress' && \
        _PROGRESS_OPT="-q --show-progress" || _PROGRESS_OPT=""
    wget $_PROGRESS_OPT -P "$dl_path" "${url}/${bin_ver}.tar.xz" "${url}/${bin_ver}.tar.xz.sig"

    if [[ "$?" -eq 0 ]]; then
       _msg "${PAS}download completed."
    else
        _err "${ERR}could not download ${url}/${bin_ver}.tar.xz."
        exit 1
    fi

    pushd $dl_path >/dev/null
    gpg --verify "${bin_ver}.tar.xz.sig" >/dev/null 2>&1
    if [[ "$?" -eq 0 ]]; then
        _msg "${PAS}Signature is valid."
        popd >/dev/null
    else
        _err "${ERR}could not verify the signature."
        exit 1
    fi
}

sys_create_store()
{ # Unpack and install /gnu/store and /var/guix
    local pkg="$1"
    local tmp_path="$2"

    _debug "--- [ $FUNCNAME ] ---"

    cd "$tmp_path"
    tar --warning=no-timestamp \
        --extract \
        --file "$pkg" &&
    _msg "${PAS}unpacked archive"

    if [[ -e "/var/guix" || -e "/gnu" ]]; then
        _err "${ERR}A previous Guix installation was found.  Refusing to overwrite."
        exit 1
    else
        _msg "${INF}Installing /var/guix and /gnu..."
        mv "${tmp_path}/var/guix" /var/
        mv "${tmp_path}/gnu" /
    fi

    _msg "${INF}Linking the root user's profile"
    mkdir -p "${ROOT_HOME}/.config/guix"
    ln -sf /var/guix/profiles/per-user/root/current-guix \
       "${ROOT_HOME}/.config/guix/current"

    GUIX_PROFILE="${ROOT_HOME}/.config/guix/current"
    source "${GUIX_PROFILE}/etc/profile"
    _msg "${PAS}activated root profile at ${ROOT_HOME}/.config/guix/current"
}

sys_create_build_user()
{ # Create the group and user accounts for build users.

    _debug "--- [ $FUNCNAME ] ---"

    if [ $(getent group guixbuild) ]; then
        _msg "${INF}group guixbuild exists"
    else
        groupadd --system guixbuild
        _msg "${PAS}group <guixbuild> created"
    fi

    for i in $(seq -w 1 10); do
        if id "guixbuilder${i}" &>/dev/null; then
            _msg "${INF}user is already in the system, reset"
            usermod -g guixbuild -G guixbuild           \
                    -d /var/empty -s "$(which nologin)" \
                    -c "Guix build user $i"             \
                    "guixbuilder${i}";
        else
            useradd -g guixbuild -G guixbuild           \
                    -d /var/empty -s "$(which nologin)" \
                    -c "Guix build user $i" --system    \
                    "guixbuilder${i}";
            _msg "${PAS}user added <guixbuilder${i}>"
        fi
    done
}

sys_enable_guix_daemon()
{ # Run the daemon, and set it to automatically start on boot.

    local info_path
    local local_bin
    local var_guix

    _debug "--- [ $FUNCNAME ] ---"

    info_path="/usr/local/share/info"
    local_bin="/usr/local/bin"
    var_guix="/var/guix/profiles/per-user/root/current-guix"

    case "$INIT_SYS" in
        upstart)
            { initctl reload-configuration;
              cp "${ROOT_HOME}/.config/guix/current/lib/upstart/system/guix-daemon.conf" \
                 /etc/init/ &&
                  start guix-daemon; } &&
                _msg "${PAS}enabled Guix daemon via upstart"
            ;;
        systemd)
            { cp "${ROOT_HOME}/.config/guix/current/lib/systemd/system/guix-daemon.service" \
                 /etc/systemd/system/;
              chmod 664 /etc/systemd/system/guix-daemon.service;
              systemctl daemon-reload &&
                  systemctl start guix-daemon &&
                  systemctl enable guix-daemon; } &&
                _msg "${PAS}enabled Guix daemon via systemd"
            ;;
        NA|*)
            _msg "${ERR}unsupported init system; run the daemon manually:"
            echo "  ${ROOT_HOME}/.config/guix/current/bin/guix-daemon --build-users-group=guixbuild"
            ;;
    esac

    _msg "${INF}making the guix command available to other users"

    [ -e "$local_bin" ] || mkdir -p "$local_bin"
    ln -sf "${var_guix}/bin/guix"  "$local_bin"

    [ -e "$info_path" ] || mkdir -p "$info_path"
    for i in ${var_guix}/share/info/*; do
        ln -sf "$i" "$info_path"
    done
}

sys_authorize_build_farms()
{ # authorize the public keys of the two build farms
    while true; do
        read -p "Permit downloading pre-built package binaries from the project's build farms? (yes/no) " yn
        case $yn in
            [Yy]*) guix archive --authorize < "${ROOT_HOME}/.config/guix/current/share/guix/hydra.gnu.org.pub" &&
                         _msg "${PAS}Authorized public key for hydra.gnu.org";
                   guix archive --authorize < "${ROOT_HOME}/.config/guix/current/share/guix/ci.guix.info.pub" &&
                       _msg "${PAS}Authorized public key for ci.guix.info";
                   break;;
            [Nn]*) _msg "${INF}Skipped authorizing build farm public keys"
                   break;;
            *) _msg "Please answer yes or no.";
        esac
    done
}

welcome()
{
    cat<<"EOF"
    ░░░                                     ░░░
    ░░▒▒░░░░░░░░░               ░░░░░░░░░▒▒░░
     ░░▒▒▒▒▒░░░░░░░           ░░░░░░░▒▒▒▒▒░
         ░▒▒▒░░▒▒▒▒▒         ░░░░░░░▒▒░
               ░▒▒▒▒░       ░░░░░░
                ▒▒▒▒▒      ░░░░░░
                 ▒▒▒▒▒     ░░░░░
                 ░▒▒▒▒▒   ░░░░░
                  ▒▒▒▒▒   ░░░░░
                   ▒▒▒▒▒ ░░░░░
                   ░▒▒▒▒▒░░░░░
                    ▒▒▒▒▒▒░░░
                     ▒▒▒▒▒▒░
     _____ _   _ _    _    _____       _
    / ____| \ | | |  | |  / ____|     (_)
   | |  __|  \| | |  | | | |  __ _   _ ___  __
   | | |_ | . ' | |  | | | | |_ | | | | \ \/ /
   | |__| | |\  | |__| | | |__| | |_| | |>  <
    \_____|_| \_|\____/   \_____|\__,_|_/_/\_\

This script installs GNU Guix on your system

https://www.gnu.org/software/guix/
EOF
    echo -n "Press return to continue..."
    read -r  ANSWER
}

main()
{
    local tmp_path
    welcome

    _msg "Starting installation ($(date))"

    chk_term
    chk_require "${REQUIRE[*]}"
    chk_init_sys
    chk_sys_arch

    _msg "${INF}system is ${ARCH_OS}"

    tmp_path="$(mktemp -t -d guix.XXX)"

    guix_get_bin_list "${GNU_URL}"
    guix_get_bin "${GNU_URL}" "${BIN_VER}" "$tmp_path"

    sys_create_store "${BIN_VER}.tar.xz" "${tmp_path}"
    sys_create_build_user
    sys_enable_guix_daemon
    sys_authorize_build_farms

    _msg "${INF}cleaning up ${tmp_path}"
    rm -r "${tmp_path}"

    _msg "${PAS}Guix has successfully been installed!"
    _msg "${INF}Run 'info guix' to read the manual."
 }

main "$@"
