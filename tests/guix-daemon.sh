# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

#
# Test the daemon and its interaction with 'guix substitute'.
#

set -e

guix-daemon --version
guix build --version

drv="`guix build emacs -d`"
out="`guile -c '								\
  (use-modules (guix) (guix grafts) (gnu packages emacs))			\
  (define store (open-connection))						\
  (%graft? #f)
  (display (derivation->output-path (package-derivation store emacs)))'`"

hash_part="`basename $out | cut -c 1-32`"
narinfo="$hash_part.narinfo"
substitute_dir="`echo $GUIX_BINARY_SUBSTITUTE_URL | sed -'es,file://,,g'`"

cat > "$substitute_dir/nix-cache-info"<<EOF
StoreDir: `dirname $drv`
WantMassQuery: 0
EOF

cat > "$substitute_dir/$narinfo"<<EOF
StorePath: $out
URL: /nowhere/example.nar
Compression: none
NarSize: 1234
References: 
System: `guile -c '(use-modules (guix)) (display (%current-system))'`
Deriver: $drv
EOF

# Remove the cached narinfo.
rm -f "$XDG_CACHE_HOME/guix/substitute/$hash_part"

# Make sure we see the substitute.
guile -c "
  (use-modules (guix))
  (define store (open-connection))
  (set-build-options store #:use-substitutes? #t
                     #:substitute-urls (list \"$GUIX_BINARY_SUBSTITUTE_URL\"))
  (exit (has-substitutes? store \"$out\"))"

# Now, run guix-daemon --no-substitutes.
socket="$NIX_STATE_DIR/alternate-socket"
guix-daemon --no-substitutes --listen="$socket" --disable-chroot &
daemon_pid=$!
trap 'kill $daemon_pid' EXIT

# Make sure we DON'T see the substitute.
guile -c "
  (use-modules (guix))
  (define store (open-connection \"$socket\"))

  ;; This setting MUST NOT override the daemon's --no-substitutes.
  (set-build-options store #:use-substitutes? #t
                     #:substitute-urls (list \"$GUIX_BINARY_SUBSTITUTE_URL\"))

  (exit (not (has-substitutes? store \"$out\")))"

kill "$daemon_pid"


# Check the failed build cache.

guix-daemon --no-substitutes --listen="$socket" --disable-chroot	\
  --cache-failures &
daemon_pid=$!

guile -c "
  (use-modules (guix) (guix grafts) (guix tests) (srfi srfi-34))
  (define store (open-connection-for-tests \"$socket\"))

  ;; Disable grafts to avoid building more than needed.
  (%graft? #f)

  (define (build-without-failing drv)
    (lambda (store)
      (guard (c ((nix-protocol-error? c) (values #t store)))
        (build-derivations store (list drv))
        (values #f store))))

  ;; Make sure failed builds are cached and can be removed from
  ;; the cache.
  (run-with-store store
    (mlet* %store-monad ((drv (gexp->derivation \"failure\"
                                                #~(begin
                                                    (ungexp output)
                                                     #f)))
                         (out -> (derivation->output-path drv))
                         (ok?    (build-without-failing drv)))
      ;; Note the mixture of monadic and direct style.  Don't try
      ;; this at home!
      (return (exit (and ok?
                         (equal? (query-failed-paths store) (list out))
                         (begin
                           (clear-failed-paths store (list out))
                           (null? (query-failed-paths store)))))))
    #:guile-for-build (%guile-for-build)) "
