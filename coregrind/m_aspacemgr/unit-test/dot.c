/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2015-2015  Florian Krohm

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

/* Write out a segment tree for visualisation with "dot" */

#include "tree.h"
#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

static const NSegment *subtree_root;
static const NSegment *inserted_node;

static char *
node_name(const NSegment *node)
{
   static char label[100];  // large enough
   sprintf(label, "N_%ld_%ld", node->start, node->end);
   return label;
}

static void
write_node_aux(FILE *fp, const NSegment *node, const char *label, 
	       const char *color, const char *shape, const char *style,
               const char *fillcolor, unsigned num)
{
   assert(node != NULL);
   assert(fp   != NULL);

   fprintf(fp, "%s", node_name(node));
   if (label) {
      fprintf(fp, "[label=\"%s\"]", label);
   }
   if (color) {
      fprintf(fp, "[color=\"%s\"]", color);
   }
   if (shape) {
      fprintf(fp, "[shape=\"%s\"]", shape);
   }
   if (style) {
      fprintf(fp, "[style=\"%s\"]", style);
   }
   if (fillcolor) {
      fprintf(fp, "[fillcolor=\"%s\"]", fillcolor);
   }
   if (num != 0) {
      fprintf(fp, "[peripheries=%u]", num);
   }
}

static void
write_node(FILE *fp, const NSegment *node)
{
   assert(fp != NULL);
   assert(node != NULL);

   const char *color = NULL;
   const char *shape = NULL;
   const char *style = NULL;
   const char *fill_color = NULL;
   unsigned    num = 0;

   if (node == subtree_root) {
      style = "filled";
      fill_color = "grey";
   }
   if (node == inserted_node) {
      style = "filled";
      fill_color = "yellow";
   }

   static char label[100];  // large enough
   sprintf(label, "%ld:%ld", node->start, node->end);
   write_node_aux(fp, node, label, color, shape,
                  style, fill_color, num);
   fprintf(fp, ";\n");
}

static void
write_nodes(FILE *fp, const NSegment *tree)
{
   if (tree == NULL) return;

   write_node(fp, tree);
   write_nodes(fp, tree->left);
   write_nodes(fp, tree->right);
}

static void
write_edge(FILE *fp, const NSegment *from, const NSegment *to)
{
   assert(fp != NULL);
   assert(from != NULL);
   assert(to != NULL);

   fprintf(fp, "%s", node_name(from));
   fprintf(fp, " -> ");
   fprintf(fp, "%s", node_name(to));
   fprintf(fp, ";\n");
}

static void
write_edges(FILE *fp, const NSegment *tree)
{
   if (tree == NULL) return;

   const NSegment *left  = tree->left;
   const NSegment *right = tree->right;

   if (left)  {
      write_edges(fp, left);
      write_edge(fp, tree, left);
   }
   if (right) {
      write_edges(fp, right);
      write_edge(fp, tree, right);
   }
#if 0
   const NSegment *parent = tree->up;
   if (parent) {
      write_edge(fp, tree, parent);
   }
#endif
}

void
write_dot(const NSegment *node, const NSegment *subtree,
          const NSegment *inserted, const char *name)
{
   assert(node != NULL);
   assert(name != NULL);

   subtree_root = subtree;    // globalise
   inserted_node = inserted;  // globalise

   char file[strlen(name) + 10];
   sprintf(file, "%s.dot", name);    // construct file name
   FILE *fp = fopen(file, "w");
   assert(fp);

   fprintf(fp, "digraph \"%s\" {\n", name);
   fprintf(fp, "\n/* Nodes */\n\n");
   write_nodes(fp, node);
   fprintf(fp, "\n/* Edges */\n\n");
   write_edges(fp, node);
   fprintf(fp, "}\n");

   fclose(fp);
}
