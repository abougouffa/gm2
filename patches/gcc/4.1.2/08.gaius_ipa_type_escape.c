--- gcc-4.1.2-orig/gcc/ipa-type-escape.c	2005-09-25 06:28:01.000000000 +0100
+++ gcc-4.1.2/gcc/ipa-type-escape.c	2009-07-20 10:36:11.000000000 +0100
@@ -259,10 +259,33 @@
 static bool
 type_to_consider (tree type)
 {
+  int stackSize = 1;
+  int stackUsed = 1;
+  int oldSize;
+  int i;
+  tree *oldStack;
+  tree *stack = (tree *) alloca (sizeof (tree) * stackSize);
+  stack[0] = type;
+
+  /* return false if we detect a cyclic declaration of "array of pointer to ..." */
+
   /* Strip the *'s off.  */
   type = TYPE_MAIN_VARIANT (type);
-  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
+  while (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE) {
     type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
+    for (i=0; i<stackUsed; i++)
+      if (stack[i] == type)
+	return false;
+    if (stackSize == stackUsed) {
+      oldSize = stackSize;
+      stackSize *= 2;
+      oldStack = stack;
+      stack = (tree *) alloca (sizeof (tree) * stackSize);
+      memcpy (stack, oldStack, oldSize);
+    }
+    stack[stackUsed] = type;
+    stackUsed++;
+  }
 
   switch (TREE_CODE (type))
     {
