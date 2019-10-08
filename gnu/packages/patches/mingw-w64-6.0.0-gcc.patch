This patch includes

   * mingw-w64-headers/include/winnt.h: compile fixes for1 gcc-4.9.3
   * mingw-w64-headers/crt/math.h: Likewise
   * mingw-w64-headers/crt/float.h (FLT_EPSILON,DBL_EPSILON,LDBL_EPSILON): Add
   symbols.
   * mingw-w64-headers/crt/stat.h (S_ISLNK,S_ISSOCK,S_ISUID,S_ISGID,S_ISLINK):
   Add symbols.
   (lstat): Add function.
   * mingw-w64-headers/crt/_mingw_stat64.h: Likewise
   * mingw-w64-headers/crt/stdlib.h (realpath): Add function.

Needed for building with gcc-4.9.3 and using with cross-libtool-2.4.6.

Upstream status: not yet presented upstream.

diff --git a/mingw-w64-headers/crt/float.h b/mingw-w64-headers/crt/float.h
index 5874f4e..bdf4ead 100644
--- a/mingw-w64-headers/crt/float.h
+++ b/mingw-w64-headers/crt/float.h
@@ -22,6 +22,15 @@
 #if (__GNUC__ < 4)
 #error Corrupt install of gcc-s internal headers, or search order was changed.
 #else
+
+        /* From gcc-4.9.3 float.h.  */
+        #undef FLT_EPSILON
+        #undef DBL_EPSILON
+        #undef LDBL_EPSILON
+        #define FLT_EPSILON	__FLT_EPSILON__
+        #define DBL_EPSILON	__DBL_EPSILON__
+        #define LDBL_EPSILON	__LDBL_EPSILON__
+
 	/* #include_next <float_ginclude.h> */

    	/* Number of decimal digits, q, such that any floating-point number with q
diff --git a/mingw-w64-headers/crt/math.h b/mingw-w64-headers/crt/math.h
index 1e970f4..99a332f 100644
--- a/mingw-w64-headers/crt/math.h
+++ b/mingw-w64-headers/crt/math.h
@@ -216,6 +216,7 @@ extern "C" {
 #endif
   }

+#if 0
   __CRT_INLINE long double __cdecl fabsl (long double x)
   {
 #ifdef __arm__
@@ -226,6 +227,7 @@ extern "C" {
     return res;
 #endif
   }
+#endif

   __CRT_INLINE double __cdecl fabs (double x)
   {
@@ -905,7 +907,7 @@ __mingw_choose_expr (                                         \
 /* 7.12.7.3  */
   extern double __cdecl hypot (double, double) __MINGW_ATTRIB_DEPRECATED_MSVC2005; /* in libmoldname.a */
   extern float __cdecl hypotf (float x, float y);
-#ifndef __CRT__NO_INLINE
+#if 0 //ndef __CRT__NO_INLINE
   __CRT_INLINE float __cdecl hypotf (float x, float y) { return (float) hypot ((double)x, (double)y);}
 #endif
   extern long double __cdecl hypotl (long double, long double);
