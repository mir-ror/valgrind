/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Address space manager segment visualisation                  ---*/
/*---                                              aspacemgr-dot.c ---*/
/*--------------------------------------------------------------------*/

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

/* Writes the segment tree to a file that can be visualised with the
   'dot' program (which is part of the graphviz package). 
   Function ML_(am_write_dot) is the single entry point here. */

#include "priv_aspacemgr.h"

/* Output a character. Characters are written to a buffer which is flushed
   when full or when C is the NUL character. */
static void output( HChar c, void *p )
{
   static HChar buf[512];
   static UInt  num_char;

   if (num_char == sizeof buf || c == '\0') {
      VG_(do_syscall3)(__NR_write, *(Int *)p, (UWord)buf, num_char);
      num_char = 0;
   }
   if (c != '\0') buf[num_char++] = c;
}

static void am_fprintf( Int fd, const HChar *format, ... )
{
   va_list vargs;

   va_start(vargs,format);
   VG_(debugLog_vprintf)( output, &fd, format, vargs );
   va_end(vargs);
}

static void
write_node_name( Int fd, const NSegment *node )
{
   am_fprintf(fd, "N_%lx_%lx", node->start, node->end);
}

static void
write_node_aux( Int fd, const NSegment *node, Bool label, 
                const HChar *color, const HChar *shape, const HChar *style,
                const HChar *fillcolor, unsigned num)
{
   aspacem_assert(node != NULL);

   HChar info[6];
   info[0] = node->hasR ? 'r' : '-';
   info[1] = node->hasW ? 'w' : '-';
   info[2] = node->hasX ? 'x' : '-';
   info[3] = node->hasT ? 'T' : '-';
   switch (node->whatsit) {
   case WiClientHeap:  info[4] = 'H'; break;
   case WiClientStack: info[4] = 'S'; break;
   case WiClientBreak: info[4] = 'B'; break;
   case WiUnknown:     info[4] = '-'; break;
   default: aspacem_assert(0);
   }
   info[5] = '\0';

   write_node_name(fd, node);
   if (label) {
      am_fprintf(fd, "[label=\"%lx:%lx\n%s\"]", node->start, node->end, info);
   }
   if (color) {
      am_fprintf(fd, "[color=\"%s\"]", color);
   }
   if (shape) {
      am_fprintf(fd, "[shape=\"%s\"]", shape);
   }
   if (style) {
      am_fprintf(fd, "[style=\"%s\"]", style);
   }
   if (fillcolor) {
      am_fprintf(fd, "[fillcolor=\"%s\"]", fillcolor);
   }
   if (num != 0) {
      am_fprintf(fd, "[peripheries=%u]", num);
   }
}

static void
write_node( Int fd, const NSegment *node )
{
   aspacem_assert(node != NULL);

   const HChar *color = NULL;
   const HChar *shape = NULL;
   const HChar *style = NULL;
   const HChar *fill_color = NULL;
   unsigned    num = 0;

   if (node->left == NULL && node->right == NULL) {  // leaf
      style = "filled";
      switch (node->kind) {
      case SkResvn:
         color = "GreenYellow";
         break;
      case SkFree:
         color = "SteelBlue1";
         break;
      case SkAnonC:
         color = "yellow";
         break;
      case SkFileC:
         color = "yellow";
         shape = "rectangle";
         break;
      case SkAnonV:
         color = "tomato";
         break;
      case SkFileV:
         color = "tomato";
         shape = "rectangle";
         break;
      default:
         aspacem_assert(0);
      }
   }

   write_node_aux(fd, node, True, color, shape, style, fill_color, num);
   am_fprintf(fd, ";\n");
}

static void
write_nodes( Int fd, const NSegment *tree )
{
   if (tree == NULL) return;

   write_node(fd, tree);
   write_nodes(fd, tree->left);
   write_nodes(fd, tree->right);
}

static void
write_edge( Int fd, const NSegment *from, const NSegment *to )
{
   aspacem_assert(from != NULL);
   aspacem_assert(to != NULL);

   write_node_name(fd, from);
   am_fprintf(fd, " -> ");
   write_node_name(fd, to);
   am_fprintf(fd, ";\n");
}

static void
write_edges( Int fd, const NSegment *tree )
{
   if (tree == NULL) return;

   const NSegment *left  = tree->left;
   const NSegment *right = tree->right;

   if (left)  {
      write_edges(fd, left);
      write_edge(fd, tree, left);
   }
   if (right) {
      write_edges(fd, right);
      write_edge(fd, tree, right);
   }
#if 0
   const NSegment *parent = tree->up;
   if (parent) {
      write_edge(fd, tree, parent);
   }
#endif
}

void ML_(am_write_dot)( const NSegment *node, const HChar *file )
{
   aspacem_assert(node != NULL);
   aspacem_assert(file != NULL);

   SysRes sres = ML_(am_open)(file, VKI_O_CREAT | VKI_O_WRONLY, 0644);
   if (sr_isError(sres))
      ML_(am_barf)("can't open dot file");
   Int fd = sr_Res(sres);

   am_fprintf(fd, "digraph \"%s\" {\n", "whatever");
   am_fprintf(fd, "\n/* Nodes */\n\n");
   write_nodes(fd, node);
   am_fprintf(fd, "\n/* Edges */\n\n");
   write_edges(fd, node);
   am_fprintf(fd, "}\n");
   output('\0', &fd);
   ML_(am_close)(fd);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
