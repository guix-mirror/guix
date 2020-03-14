From 46339bdf619b93dfa05d5f004d062671d3dc26d2 Mon Sep 17 00:00:00 2001
From: Mathieu Othacehe <m.othacehe@gmail.com>
Date: Tue, 24 Mar 2020 13:50:56 +0100
Subject: [PATCH] toolexec

---
 gcc/doc/install.texi           |  4 ++++
 libatomic/configure            | 33 ++++++++++++++++++++++---
 libffi/configure               | 33 ++++++++++++++++++++++---
 libgcc/configure               | 38 +++++++++++++++++++++++++++--
 libgomp/configure              | 33 ++++++++++++++++++++++---
 libhsail-rt/configure          | 33 ++++++++++++++++++++++---
 libitm/configure               | 33 ++++++++++++++++++++++---
 liboffloadmic/configure        | 33 ++++++++++++++++++++++---
 liboffloadmic/plugin/configure | 33 ++++++++++++++++++++++---
 libquadmath/configure          | 33 ++++++++++++++++++++++---
 libsanitizer/configure         | 33 ++++++++++++++++++++++---
 libssp/configure               | 33 ++++++++++++++++++++++---
 libstdc++-v3/configure         | 44 ++++++++++++++++++++++++++--------
 13 files changed, 374 insertions(+), 42 deletions(-)

diff --git a/gcc/doc/install.texi b/gcc/doc/install.texi
index 77ba08d3f21..28cb6d88da8 100644
--- a/gcc/doc/install.texi
+++ b/gcc/doc/install.texi
@@ -2083,6 +2083,10 @@ shorthand for
 The following options only apply to building cross compilers.
 
 @table @code
+@item --with-toolexeclibdir=@var{dir}
+Specify the installation directory for libraries built with a cross compiler.
+The default is @option{$@{gcc_tooldir@}/lib}.
+
 @item --with-sysroot
 @itemx --with-sysroot=@var{dir}
 Tells GCC to consider @var{dir} as the root of a tree that contains
diff --git a/libatomic/configure b/libatomic/configure
index 2ae9b8d40f3..0fa531ec4a3 100755
--- a/libatomic/configure
+++ b/libatomic/configure
@@ -755,6 +755,7 @@ enable_option_checking
 enable_version_specific_runtime_libs
 enable_generated_files_in_srcdir
 enable_multilib
+with_toolexeclibdir
 enable_dependency_tracking
 enable_shared
 enable_static
@@ -1414,6 +1415,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -3185,6 +3189,22 @@ fi
 ac_config_commands="$ac_config_commands default-1"
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${enable_version_specific_runtime_libs} in
@@ -3200,7 +3220,14 @@ case ${enable_version_specific_runtime_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -11115,7 +11142,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11118 "configure"
+#line 11145 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11221,7 +11248,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11224 "configure"
+#line 11251 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/libffi/configure b/libffi/configure
index 790a291011f..6e37039e84c 100755
--- a/libffi/configure
+++ b/libffi/configure
@@ -777,6 +777,7 @@ enable_debug
 enable_structs
 enable_raw_api
 enable_purify_safety
+with_toolexeclibdir
 enable_symvers
 with_gcc_major_version_only
 '
@@ -1436,6 +1437,9 @@ Optional Packages:
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-gcc-major-version-only
                           use only GCC major number in filesystem paths
 
@@ -11390,7 +11394,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11393 "configure"
+#line 11397 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11496,7 +11500,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11499 "configure"
+#line 11507 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -16002,10 +16006,33 @@ $as_echo "#define USING_PURIFY 1" >>confdefs.h
 fi
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 if test -n "$with_cross_host" &&
    test x"$with_cross_host" != x"no"; then
   toolexecdir='$(exec_prefix)/$(target_alias)'
-  toolexeclibdir='$(toolexecdir)/lib'
+  case ${with_toolexeclibdir} in
+    no)
+      toolexeclibdir='$(toolexecdir)/lib'
+      ;;
+    *)
+      toolexeclibdir=${with_toolexeclibdir}
+      ;;
+  esac
 else
   toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
   toolexeclibdir='$(libdir)'
diff --git a/libgcc/configure b/libgcc/configure
index 441601a1f76..976827dc57e 100644
--- a/libgcc/configure
+++ b/libgcc/configure
@@ -669,6 +669,7 @@ enable_shared
 enable_vtable_verify
 with_aix_soname
 enable_version_specific_runtime_libs
+with_toolexeclibdir
 with_slibdir
 enable_maintainer_mode
 with_build_libsubdir
@@ -1329,6 +1330,9 @@ Optional Packages:
   --with-aix-soname=aix|svr4|both
                           shared library versioning (aka "SONAME") variant to
                           provide on AIX
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-slibdir=DIR      shared libraries in DIR LIBDIR
   --with-build-libsubdir=DIR  Directory where to find libraries for build system
   --with-system-libunwind use installed libunwind
@@ -2403,6 +2407,22 @@ fi
 $as_echo "$version_specific_libs" >&6; }
 
 
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
+
 # Check whether --with-slibdir was given.
 if test "${with_slibdir+set}" = set; then :
   withval=$with_slibdir; slibdir="$with_slibdir"
@@ -2410,7 +2430,14 @@ else
   if test "${version_specific_libs}" = yes; then
   slibdir='$(libsubdir)'
 elif test -n "$with_cross_host" && test x"$with_cross_host" != x"no"; then
-  slibdir='$(exec_prefix)/$(host_noncanonical)/lib'
+  case ${with_toolexeclibdir} in
+    no)
+      slibdir='$(exec_prefix)/$(host_noncanonical)/lib'
+      ;;
+    *)
+      slibdir=${with_toolexeclibdir}
+      ;;
+  esac
 else
   slibdir='$(libdir)'
 fi
@@ -2640,7 +2667,14 @@ case ${version_specific_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_noncanonical)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
       toolexeclibdir='$(libdir)'
diff --git a/libgomp/configure b/libgomp/configure
index 06166c66120..6b3beae0f63 100755
--- a/libgomp/configure
+++ b/libgomp/configure
@@ -782,6 +782,7 @@ enable_option_checking
 enable_version_specific_runtime_libs
 enable_generated_files_in_srcdir
 enable_multilib
+with_toolexeclibdir
 enable_dependency_tracking
 enable_shared
 enable_static
@@ -1455,6 +1456,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -3338,6 +3342,22 @@ fi
 ac_config_commands="$ac_config_commands default-1"
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${enable_version_specific_runtime_libs} in
@@ -3353,7 +3373,14 @@ case ${enable_version_specific_runtime_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -11155,7 +11182,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11158 "configure"
+#line 11185 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11261,7 +11288,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11264 "configure"
+#line 11295 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/libhsail-rt/configure b/libhsail-rt/configure
index a4fcc10c1f9..1b4f2a953d0 100755
--- a/libhsail-rt/configure
+++ b/libhsail-rt/configure
@@ -737,6 +737,7 @@ enable_option_checking
 enable_maintainer_mode
 enable_dependency_tracking
 enable_version_specific_runtime_libs
+with_toolexeclibdir
 enable_shared
 enable_static
 with_pic
@@ -1395,6 +1396,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -4418,6 +4422,22 @@ fi
 { $as_echo "$as_me:${as_lineno-$LINENO}: result: $enable_version_specific_runtime_libs" >&5
 $as_echo "$enable_version_specific_runtime_libs" >&6; }
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${enable_version_specific_runtime_libs} in
@@ -4433,7 +4453,14 @@ case ${enable_version_specific_runtime_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -10973,7 +11000,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10976 "configure"
+#line 11003 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11079,7 +11106,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11082 "configure"
+#line 11113 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/libitm/configure b/libitm/configure
index 96c494d4a3f..ed47fab3c83 100644
--- a/libitm/configure
+++ b/libitm/configure
@@ -766,6 +766,7 @@ enable_option_checking
 enable_version_specific_runtime_libs
 enable_generated_files_in_srcdir
 enable_multilib
+with_toolexeclibdir
 enable_dependency_tracking
 enable_shared
 enable_static
@@ -1430,6 +1431,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -3371,6 +3375,22 @@ fi
 ac_config_commands="$ac_config_commands default-1"
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${enable_version_specific_runtime_libs} in
@@ -3386,7 +3406,14 @@ case ${enable_version_specific_runtime_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -11794,7 +11821,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11797 "configure"
+#line 11824 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11900,7 +11927,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11903 "configure"
+#line 11934 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/liboffloadmic/configure b/liboffloadmic/configure
index f873716991b..6dfe9e37642 100644
--- a/liboffloadmic/configure
+++ b/liboffloadmic/configure
@@ -739,6 +739,7 @@ enable_maintainer_mode
 enable_dependency_tracking
 enable_multilib
 enable_version_specific_runtime_libs
+with_toolexeclibdir
 enable_shared
 enable_static
 with_pic
@@ -1397,6 +1398,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -5003,6 +5007,22 @@ else
 fi
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir.
 # Also toolexecdir, though it's only used in toolexeclibdir.
 case ${enable_version_specific_runtime_libs} in
@@ -5018,7 +5038,14 @@ case ${enable_version_specific_runtime_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -11108,7 +11135,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11111 "configure"
+#line 11138 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11214,7 +11241,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11217 "configure"
+#line 11248 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/liboffloadmic/plugin/configure b/liboffloadmic/plugin/configure
index c031eb3e7fa..570758344b4 100644
--- a/liboffloadmic/plugin/configure
+++ b/liboffloadmic/plugin/configure
@@ -735,6 +735,7 @@ enable_maintainer_mode
 enable_dependency_tracking
 enable_multilib
 enable_version_specific_runtime_libs
+with_toolexeclibdir
 enable_shared
 enable_static
 with_pic
@@ -1394,6 +1395,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -4311,6 +4315,22 @@ fi
 $as_echo "$enable_version_specific_runtime_libs" >&6; }
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir.
 # Also toolexecdir, though it's only used in toolexeclibdir.
 case ${enable_version_specific_runtime_libs} in
@@ -4326,7 +4346,14 @@ case ${enable_version_specific_runtime_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -10815,7 +10842,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10818 "configure"
+#line 10845 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -10921,7 +10948,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10924 "configure"
+#line 10955 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/libquadmath/configure b/libquadmath/configure
index 76a2c20b7e1..e887071aeb2 100755
--- a/libquadmath/configure
+++ b/libquadmath/configure
@@ -749,6 +749,7 @@ enable_fast_install
 with_gnu_ld
 enable_libtool_lock
 enable_maintainer_mode
+with_toolexeclibdir
 enable_symvers
 enable_generated_files_in_srcdir
 with_gcc_major_version_only
@@ -1408,6 +1409,9 @@ Optional Packages:
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-gcc-major-version-only
                           use only GCC major number in filesystem paths
 
@@ -10572,7 +10576,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10575 "configure"
+#line 10579 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -10678,7 +10682,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10681 "configure"
+#line 10689 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11917,6 +11921,22 @@ ac_link='$CC -o conftest$ac_exeext $CFLAGS $CPPFLAGS $LDFLAGS conftest.$ac_ext $
 ac_compiler_gnu=$ac_cv_c_compiler_gnu
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${version_specific_libs} in
@@ -11932,7 +11952,14 @@ case ${version_specific_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
diff --git a/libsanitizer/configure b/libsanitizer/configure
index a3a08d635f4..5f4cdcad38d 100755
--- a/libsanitizer/configure
+++ b/libsanitizer/configure
@@ -767,6 +767,7 @@ enable_multilib
 enable_version_specific_runtime_libs
 enable_dependency_tracking
 enable_maintainer_mode
+with_toolexeclibdir
 enable_shared
 enable_static
 with_pic
@@ -1425,6 +1426,9 @@ Optional Features:
 Optional Packages:
   --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
   --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
@@ -4773,6 +4777,22 @@ fi
 
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${version_specific_libs} in
@@ -4788,7 +4808,14 @@ case ${version_specific_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
@@ -12032,7 +12059,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 12035 "configure"
+#line 12062 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -12138,7 +12165,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 12141 "configure"
+#line 12168 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
diff --git a/libssp/configure b/libssp/configure
index ee1751d20db..3273cd40ab1 100755
--- a/libssp/configure
+++ b/libssp/configure
@@ -743,6 +743,7 @@ with_pic
 enable_fast_install
 with_gnu_ld
 enable_libtool_lock
+with_toolexeclibdir
 with_gcc_major_version_only
 '
       ac_precious_vars='build_alias
@@ -1389,6 +1390,9 @@ Optional Packages:
   --with-pic              try to use only PIC/non-PIC objects [default=use
                           both]
   --with-gnu-ld           assume the C compiler uses GNU ld [default=no]
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-gcc-major-version-only
                           use only GCC major number in filesystem paths
 
@@ -10671,7 +10675,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10674 "configure"
+#line 10678 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -10777,7 +10781,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 10780 "configure"
+#line 10784 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11039,6 +11043,22 @@ esac
 
 
 
+
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
+
+
 # Calculate toolexeclibdir
 # Also toolexecdir, though it's only used in toolexeclibdir
 case ${version_specific_libs} in
@@ -11054,7 +11074,14 @@ case ${version_specific_libs} in
        test x"$with_cross_host" != x"no"; then
       # Install a library built with a cross compiler in tooldir, not libdir.
       toolexecdir='$(exec_prefix)/$(target_alias)'
-      toolexeclibdir='$(toolexecdir)/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  toolexeclibdir='$(toolexecdir)/lib'
+	  ;;
+	*)
+	  toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
       toolexeclibdir='$(libdir)'
diff --git a/libstdc++-v3/configure b/libstdc++-v3/configure
index de8390703e2..88de3f728d4 100755
--- a/libstdc++-v3/configure
+++ b/libstdc++-v3/configure
@@ -903,6 +903,7 @@ enable_libstdcxx_threads
 enable_libstdcxx_filesystem_ts
 with_gxx_include_dir
 enable_version_specific_runtime_libs
+with_toolexeclibdir
 with_gcc_major_version_only
 '
       ac_precious_vars='build_alias
@@ -1623,6 +1624,9 @@ Optional Packages:
                           set the std::string ABI to use by default
   --with-gxx-include-dir=DIR
                           installation directory for include files
+  --with-toolexeclibdir=DIR
+                          install libraries built with a cross compiler within
+                          DIR
   --with-gcc-major-version-only
                           use only GCC major number in filesystem paths
 
@@ -11606,7 +11610,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11609 "configure"
+#line 11613 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -11712,7 +11716,7 @@ else
   lt_dlunknown=0; lt_dlno_uscore=1; lt_dlneed_uscore=2
   lt_status=$lt_dlunknown
   cat > conftest.$ac_ext <<_LT_EOF
-#line 11715 "configure"
+#line 11723 "configure"
 #include "confdefs.h"
 
 #if HAVE_DLFCN_H
@@ -15398,7 +15402,7 @@ $as_echo "$glibcxx_cv_atomic_long_long" >&6; }
   # Fake what AC_TRY_COMPILE does.
 
     cat > conftest.$ac_ext << EOF
-#line 15401 "configure"
+#line 15409 "configure"
 int main()
 {
   typedef bool atomic_type;
@@ -15433,7 +15437,7 @@ $as_echo "$glibcxx_cv_atomic_bool" >&6; }
     rm -f conftest*
 
     cat > conftest.$ac_ext << EOF
-#line 15436 "configure"
+#line 15440 "configure"
 int main()
 {
   typedef short atomic_type;
@@ -15468,7 +15472,7 @@ $as_echo "$glibcxx_cv_atomic_short" >&6; }
     rm -f conftest*
 
     cat > conftest.$ac_ext << EOF
-#line 15471 "configure"
+#line 15475 "configure"
 int main()
 {
   // NB: _Atomic_word not necessarily int.
@@ -15504,7 +15508,7 @@ $as_echo "$glibcxx_cv_atomic_int" >&6; }
     rm -f conftest*
 
     cat > conftest.$ac_ext << EOF
-#line 15507 "configure"
+#line 15511 "configure"
 int main()
 {
   typedef long long atomic_type;
@@ -15585,7 +15589,7 @@ $as_echo "$as_me: WARNING: Performance of certain classes will degrade as a resu
   # unnecessary for this test.
 
     cat > conftest.$ac_ext << EOF
-#line 15588 "configure"
+#line 15592 "configure"
 int main()
 {
   _Decimal32 d1;
@@ -15627,7 +15631,7 @@ ac_compiler_gnu=$ac_cv_cxx_compiler_gnu
   # unnecessary for this test.
 
     cat > conftest.$ac_ext << EOF
-#line 15630 "configure"
+#line 15634 "configure"
 template<typename T1, typename T2>
   struct same
   { typedef T2 type; };
@@ -15661,7 +15665,7 @@ $as_echo "$enable_int128" >&6; }
     rm -f conftest*
 
     cat > conftest.$ac_ext << EOF
-#line 15664 "configure"
+#line 15668 "configure"
 template<typename T1, typename T2>
   struct same
   { typedef T2 type; };
@@ -81674,6 +81678,19 @@ fi
   { $as_echo "$as_me:${as_lineno-$LINENO}: result: $version_specific_libs" >&5
 $as_echo "$version_specific_libs" >&6; }
 
+# Check whether --with-toolexeclibdir was given.
+if test "${with_toolexeclibdir+set}" = set; then :
+  withval=$with_toolexeclibdir; case ${with_toolexeclibdir} in
+  /)
+    ;;
+  */)
+    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
+    ;;
+esac
+else
+  with_toolexeclibdir=no
+fi
+
   # Default case for install directory for include files.
   if test $version_specific_libs = no && test $gxx_include_dir = no; then
     gxx_include_dir='include/c++/${gcc_version}'
@@ -81704,7 +81721,14 @@ $as_echo "$version_specific_libs" >&6; }
     if test -n "$with_cross_host" &&
        test x"$with_cross_host" != x"no"; then
       glibcxx_toolexecdir='${exec_prefix}/${host_alias}'
-      glibcxx_toolexeclibdir='${toolexecdir}/lib'
+      case ${with_toolexeclibdir} in
+	no)
+	  glibcxx_toolexeclibdir='${toolexecdir}/lib'
+	  ;;
+	*)
+	  glibcxx_toolexeclibdir=${with_toolexeclibdir}
+	  ;;
+      esac
     else
       glibcxx_toolexecdir='${libdir}/gcc/${host_alias}'
       glibcxx_toolexeclibdir='${libdir}'
-- 
2.24.0

