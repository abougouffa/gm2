#if defined(ORIG)
tree
gccgm2_DeclareKnownType (name, type)
     char *name;
     tree type;
{
    tree id = get_identifier (name);
    tree attributes, prefix_attributes;
    tree decl, tem, typedef_decl;

#if 0
    /* firstly check for types of types */
    if (TREE_CODE (type) == TYPE_DECL) {
      type = TREE_TYPE (type);
      typedef_decl = type;
    }
    /* Built-in types come as identifiers.  */
    else if (TREE_CODE (type) == IDENTIFIER_NODE) {
      tree t = lookup_name (type);

      if (TREE_TYPE (t) == error_mark_node)
	;
      else if (!t || TREE_CODE (t) != TYPE_DECL)
	error ("`%s' fails to be a typedef or built in type",
	       IDENTIFIER_POINTER (type));
      else {
	type = TREE_TYPE (t);
	typedef_decl = t;
      }
    }
#endif
    decl = build_decl (TYPE_DECL, id, type);
    DECL_COMMON (decl) = 1;

    /* The corresponding pop_obstacks is in finish_decl.  */
    push_obstacks_nochange ();

#if 1
    attributes        = NULL_TREE;
    prefix_attributes = NULL_TREE;
    /* Set attributes here so if duplicate decl, will have proper attributes.  */
    decl_attributes (decl, attributes, prefix_attributes);
#endif

  /* Add this decl to the current binding level.
     TEM may equal DECL or it may be a previous decl of the same name.  */
    tem = pushdecl (decl);

    DECL_SOURCE_LINE(decl) = lineno;
    layout_type (type);
    layout_type (TREE_TYPE(decl));
#if 1
    if (DECL_SIZE(decl) == 0) {
      error_with_decl (decl, "storage size of `%s' hasn't been resolved");
    }
#endif
#if 1
    /* For a local variable, define the RTL now.  */
    if (current_binding_level != global_binding_level
	/* But not if this is a duplicate decl
	   and we preserved the rtl from the previous one
	   (which may or may not happen).  */
	&& DECL_RTL (tem) == 0)
      {
	if (TYPE_SIZE (TREE_TYPE (tem)) != 0)
	  expand_decl (tem);
	else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
		 && DECL_INITIAL (tem) != 0)
	  expand_decl (tem);
      }
#endif

#if 0    
    /* Avoid warnings if the standard decls are unused */
    TREE_USED    (decl) = 1;
#endif

    finish_decl (decl, NULL_TREE, NULL_TREE);
    return( id );
}
#endif
