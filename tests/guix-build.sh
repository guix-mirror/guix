# GNU Guix --- Functional package management for GNU
# Copyright © 2012, 2013, 2014, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
# Test the `guix build' command-line utility.
#

guix build --version

# Should fail.
if guix build -e +;
then false; else true; fi

# Source-less packages are accepted; they just return nothing.
guix build -e '(@ (gnu packages bootstrap) %bootstrap-glibc)' -S
test "`guix build -e '(@ (gnu packages bootstrap) %bootstrap-glibc)' -S`" = ""

# Should pass.
guix build -e '(@@ (gnu packages bootstrap) %bootstrap-guile)' |	\
    grep -e '-guile-'
guix build hello -d |				\
    grep -e '-hello-[0-9\.]\+\.drv$'

# Passing a URI.
GUIX_DAEMON_SOCKET="file://$GUIX_STATE_DIRECTORY/daemon-socket/socket"	\
guix build -e '(@@ (gnu packages bootstrap) %bootstrap-guile)'

( if GUIX_DAEMON_SOCKET="weird://uri"					\
     guix build -e '(@@ (gnu packages bootstrap) %bootstrap-guile)';	\
  then exit 1; fi )

# Passing one '-s' flag.
test `guix build sed -s x86_64-linux -d | wc -l` = 1

# Passing multiple '-s' flags.
all_systems="-s x86_64-linux -s i686-linux -s armhf-linux -s aarch64-linux"
test `guix build sed $all_systems -d | sort -u | wc -l` = 4

# Check --sources option with its arguments
module_dir="t-guix-build-$$"
mkdir "$module_dir"
trap "rm -rf $module_dir" EXIT

cat > "$module_dir/foo.scm"<<EOF
(define-module (foo)
  #:use-module (guix tests)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial))

(define-public foo
  (package
    (name "foo")
    (version "42")
    (source (origin
              (method url-fetch)
              (uri "http://www.example.com/foo.tar.gz")
              (sha256
               (base32
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))))
    (build-system trivial-build-system)
    (inputs
     (quasiquote (("bar" ,bar))))
    (home-page "www.example.com")
    (synopsis "Dummy package")
    (description "foo is a dummy package for testing.")
    (license #f)))

(define-public bar
  (package
    (name "bar")
    (version "9001")
    (source (origin
              (method url-fetch)
              (uri "http://www.example.com/bar.tar.gz")
              (sha256
               (base32
                "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"))))
    (build-system trivial-build-system)
    (inputs
     (quasiquote
      (("data" ,(origin
                 (method url-fetch)
                 (uri "http://www.example.com/bar.dat")
                 (sha256
                  (base32
                   "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")))))))
    (home-page "www.example.com")
    (synopsis "Dummy package")
    (description "bar is a dummy package for testing.")
    (license #f)))

(define-public baz
  (dummy-package "baz" (replacement foo)))

(define-public superseded
  (deprecated-package "superseded" bar))

EOF

GUIX_PACKAGE_PATH="$module_dir"
export GUIX_PACKAGE_PATH

# foo.tar.gz
guix build -d -S foo
guix build -d -S foo | grep -e 'foo\.tar\.gz'

# 'baz' has a replacement so we should be getting the replacement's source.
(unset GUIX_BUILD_OPTIONS;
 test "`guix build -d -S baz`" = "`guix build -d -S foo`")

guix build -d --sources=package foo
guix build -d --sources=package foo | grep -e 'foo\.tar\.gz'

# bar.tar.gz and bar.dat
guix build -d --sources bar
test `guix build -d --sources bar \
      | grep -e 'bar\.tar\.gz' -e 'bar\.dat' \
      | wc -l` -eq 2

# bar.tar.gz and bar.dat
guix build -d --sources=all bar
test `guix build -d --sources bar \
      | grep -e 'bar\.tar\.gz' -e 'bar\.dat' \
      | wc -l` -eq 2

# Should include foo.tar.gz, bar.tar.gz, and bar.dat
guix build -d --sources=transitive foo
test `guix build -d --sources=transitive foo \
      | grep -e 'foo\.tar\.gz' -e 'bar\.tar\.gz' -e 'bar\.dat' \
      | wc -l` -eq 3


# Unbound variables.
cat > "$module_dir/foo.scm"<<EOF
(define-module (foo)
  #:use-module (guix tests)
  #:use-module (guix build-system trivial))

(define-public foo
  (dummy-package "package-with-something-wrong"
    (build-system trivial-build-system)
    (inputs (quasiquote (("sed" ,sed))))))  ;unbound variable
EOF

if guix build package-with-something-wrong -n; then false; else true; fi
guix build package-with-something-wrong -n 2> "$module_dir/err" || true
grep "unbound" "$module_dir/err"		     # actual error
grep "forget.*(gnu packages base)" "$module_dir/err" # hint
rm -f "$module_dir"/*

# Should all return valid log files.
drv="`guix build -d -e '(@@ (gnu packages bootstrap) %bootstrap-guile)'`"
out="`guix build -e '(@@ (gnu packages bootstrap) %bootstrap-guile)'`"
log="`guix build --log-file $drv`"
echo "$log" | grep log/.*guile.*drv
test -f "$log"
test "`guix build -e '(@@ (gnu packages bootstrap) %bootstrap-guile)' --log-file`" \
    = "$log"
test "`guix build --log-file guile-bootstrap`" = "$log"
test "`guix build --log-file $out`" = "$log"

# Should fail because the name/version combination could not be found.
if guix build hello-0.0.1 -n; then false; else true; fi

# Keep a symlink to the result, registered as a root.
result="t-result-$$"
guix build -r "$result"					\
    -e '(@@ (gnu packages bootstrap) %bootstrap-guile)'
test -x "$result/bin/guile"

# Should fail, because $result already exists.
if guix build -r "$result" -e '(@@ (gnu packages bootstrap) %bootstrap-guile)'
then false; else true; fi

rm -f "$result"

# Check relative file name canonicalization: <https://bugs.gnu.org/35271>.
mkdir "$result"
guix build -r "$result/x" -e '(@@ (gnu packages bootstrap) %bootstrap-guile)'
test -x "$result/x/bin/guile"
rm "$result/x"
rmdir "$result"

# Cross building.
guix build coreutils --target=mips64el-linux-gnu --dry-run --no-substitutes

# Replacements.
drv1=`guix build guix --with-input=guile@2.0=guile@2.2 -d`
drv2=`guix build guix -d`
test "$drv1" != "$drv2"

drv1=`guix build guile -d`
drv2=`guix build guile --with-input=gimp=ruby -d`
test "$drv1" = "$drv2"

if guix build guile --with-input=libunistring=something-really-silly
then false; else true; fi

# Deprecated/superseded packages.
test "`guix build superseded -d`" = "`guix build bar -d`"

# Parsing package names and versions.
guix build -n time		# PASS
guix build -n time@1.9		# PASS, version found
if guix build -n time@3.2;	# FAIL, version not found
then false; else true; fi
if guix build -n something-that-will-never-exist; # FAIL
then false; else true; fi

# Invoking a monadic procedure.
guix build -e "(begin
                 (use-modules (guix gexp))
                 (lambda ()
                   (gexp->derivation \"test\"
                                     (gexp (mkdir (ungexp output))))))" \
   --dry-run

# Running a gexp.
guix build -e '#~(mkdir #$output)' -d
guix build -e '#~(mkdir #$output)' -d | grep 'gexp\.drv'

# Same with a file-like object.
guix build -e '(computed-file "foo" #~(mkdir #$output))' -d
guix build -e '(computed-file "foo" #~(mkdir #$output))' -d | grep 'foo\.drv'

# Building from a package file.
cat > "$module_dir/package.scm"<<EOF
(use-modules (gnu))
(use-package-modules bootstrap)

%bootstrap-guile
EOF
guix build --file="$module_dir/package.scm"

# Building from a monadic procedure file.
cat > "$module_dir/proc.scm"<<EOF
(use-modules (guix gexp))
(lambda ()
  (gexp->derivation "test"
                    (gexp (mkdir (ungexp output)))))
EOF
guix build --file="$module_dir/proc.scm" --dry-run

# Building from a gexp file.
cat > "$module_dir/gexp.scm"<<EOF
(use-modules (guix gexp))

(gexp (mkdir (ungexp output)))
EOF
guix build --file="$module_dir/gexp.scm" -d
guix build --file="$module_dir/gexp.scm" -d | grep 'gexp\.drv'

# Using 'GUIX_BUILD_OPTIONS'.
GUIX_BUILD_OPTIONS="--dry-run --no-grafts"
export GUIX_BUILD_OPTIONS

guix build emacs

GUIX_BUILD_OPTIONS="--something-completely-crazy"
if guix build emacs;
then false; else true; fi
