From d22db2048534bdf3d9615117291f9d86564ff10d Mon Sep 17 00:00:00 2001
From: Julien Lepiller <julien@lepiller.eu>
Date: Sat, 19 Sep 2020 21:05:48 +0200
Subject: [PATCH] Separate addPropertyAliases in two methods

The quantity of data used to generate addPropertyAliases creates a big
method, that is too big for java and results in "error: code too large".
This is most likely due to added data between the expected version of
icu and the actual version of icu in Guix.
---
 .../org/antlr/v4/tool/templates/unicodedata.st | 17 ++++++++++++++---
 .../unicode/UnicodeDataTemplateController.java | 18 ++++++++++--------
 2 files changed, 24 insertions(+), 11 deletions(-)

diff --git a/tool/resources/org/antlr/v4/tool/templates/unicodedata.st b/tool/resources/org/antlr/v4/tool/templates/unicodedata.st
index 0f22c73..3573873 100644
--- a/tool/resources/org/antlr/v4/tool/templates/unicodedata.st
+++ b/tool/resources/org/antlr/v4/tool/templates/unicodedata.st
@@ -1,4 +1,4 @@
-unicodedata(propertyCodePointRanges, propertyAliases) ::= <<
+unicodedata(propertyCodePointRanges, propertyAliasesA, propertyAliasesB) ::= <<
 package org.antlr.v4.unicode;
 
 import java.util.Arrays;
@@ -15,7 +15,7 @@ import org.antlr.v4.runtime.misc.Interval;
  */
 public abstract class UnicodeData {
        private static final Map\<String, IntervalSet\> propertyCodePointRanges = new HashMap\<\>(<length(propertyCodePointRanges)>);
-       private static final Map\<String, String\> propertyAliases = new HashMap\<\>(<length(propertyAliases)>);
+       private static final Map\<String, String\> propertyAliases = new HashMap\<\>(<length(propertyAliasesA)> + <length(propertyAliasesB)>);
 
        // Work around Java 64k bytecode method limit by splitting up static
        // initialization into one method per Unicode property
@@ -30,9 +30,20 @@ static private void addProperty<i>() {
        propertyCodePointRanges.put("<k>".toLowerCase(Locale.US), codePointRanges);
 \}}; separator="\n\n">
 
+       // Property aliases
+       static private void addPropertyAliases1() {
+              <propertyAliasesA.keys:{ k | propertyAliases.put("<k>".toLowerCase(Locale.US), "<propertyAliasesA.(k)>".toLowerCase(Locale.US)); }; separator="\n">
+       }
+
+       // Property aliases
+       static private void addPropertyAliases2() {
+              <propertyAliasesB.keys:{ k | propertyAliases.put("<k>".toLowerCase(Locale.US), "<propertyAliasesB.(k)>".toLowerCase(Locale.US)); }; separator="\n">
+       }
+
        // Property aliases
        static private void addPropertyAliases() {
-              <propertyAliases.keys:{ k | propertyAliases.put("<k>".toLowerCase(Locale.US), "<propertyAliases.(k)>".toLowerCase(Locale.US)); }; separator="\n">
+             addPropertyAliases1();
+             addPropertyAliases2();
        }
 
        // Put it all together
diff --git a/tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java b/tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java
index da244a3..dc591cb 100644
--- a/tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java
+++ b/tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java
@@ -78,17 +78,19 @@ public abstract class UnicodeDataTemplateController {
 		addTR35ExtendedPictographicPropertyCodesToCodePointRanges(propertyCodePointRanges);
 		addEmojiPresentationPropertyCodesToCodePointRanges(propertyCodePointRanges);
 
-		Map<String, String> propertyAliases = new LinkedHashMap<>();
-		addUnicodeCategoryCodesToNames(propertyAliases);
-		addUnicodeBinaryPropertyCodesToNames(propertyAliases);
-		addUnicodeScriptCodesToNames(propertyAliases);
-		addUnicodeBlocksToNames(propertyAliases);
-		addUnicodeIntPropertyCodesToNames(propertyAliases);
-		propertyAliases.put("EP", "Extended_Pictographic");
+		Map<String, String> propertyAliases1 = new LinkedHashMap<>();
+		Map<String, String> propertyAliases2 = new LinkedHashMap<>();
+		addUnicodeCategoryCodesToNames(propertyAliases1);
+		addUnicodeBinaryPropertyCodesToNames(propertyAliases1);
+		addUnicodeScriptCodesToNames(propertyAliases1);
+		addUnicodeBlocksToNames(propertyAliases2);
+		addUnicodeIntPropertyCodesToNames(propertyAliases2);
+		propertyAliases2.put("EP", "Extended_Pictographic");
 
 		Map<String, Object> properties = new LinkedHashMap<>();
 		properties.put("propertyCodePointRanges", propertyCodePointRanges);
-		properties.put("propertyAliases", propertyAliases);
+		properties.put("propertyAliasesA", propertyAliases1);
+		properties.put("propertyAliasesB", propertyAliases2);
 		return properties;
 	}
 
-- 
2.28.0

