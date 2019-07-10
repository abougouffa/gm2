/* rtegraph.c graph and nodes used by m2rte.

Copyright (C) 2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING.  If not,
see <https://www.gnu.org/licenses/>.  */

#ifndef RTE_GRAPH_H
#define RTE_GRAPH_H


class GTY(()) rtenode
{
 public:
  bool constructor_reachable;   /* this is guarenteed to be reachable by a constructor?  */
  bool export_reachable;  /* this is reachable via exported functions.  */
  bool exception_routine;   /* is this a exception routine?  */
  bool constructor_final;   /* walked this rtenode during constructor testing?  */
  bool export_final;   /* walked this rtenode during exported testing?  */
  bool is_call;    /* is this a function call?  */
  gimple *grtenode;
  tree func;
  rtenode *reachable_src;  /* if this is reachable which src function will call us?  */

  rtenode ();
  rtenode (gimple *g, tree fndecl, bool is_func_call);
  ~rtenode ();
  rtenode (const rtenode &from);
  rtenode& operator= (const rtenode &from);

  auto_vec<rtenode *> function_call;
  auto_vec<rtenode *> rts_calls;
  void dump (void);
  void dump_vec (const char *title, vec<rtenode *> &list);

  void propagate_constructor_reachable (rtenode *);
  void propagate_export_reachable (rtenode *);
  void error_message (void);
  void warning_message (void);
  void note_message (void);
  const char *get_func_name (void);
  const char *create_message (const char *with_name, const char *without_name);
};


class GTY(()) rtegraph
{
 private:

 public:
  rtegraph (void);
  ~rtegraph (void);
  rtegraph (const rtegraph &from);
  rtegraph& operator= (const rtegraph &from);

  auto_vec<rtenode *> all_rtenodes;
  auto_vec<rtenode *> candidates;
  auto_vec<rtenode *> externs;
  auto_vec<rtenode *> constructors;

  void determine_reachable (void);
  void issue_messages (void);
  rtenode *lookup (gimple *g, tree fndecl, bool is_call);
  void dump (void);
  void dump_vec (const char *title, vec<rtenode *> &list);
};


#if !defined (RTEGRAPH_C)
extern rtegraph *m2rte_graph;
extern rtenode *m2rte_current_function_rtenode;
extern tree m2rte_current_function;
#endif

#endif
