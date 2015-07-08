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

#include "tree.h"
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>

/* Read in a tree structure from a file. Like this:

      0:100              # root node
      0:100 -> 0:50      # edge from parent -> child
      0:100 -> 51:100    # ditto

   Edges can appear in any order. Root node needs to come first.
   '#' begins a comment that extends to next '\n'
   Empty lines are allowed.
*/

static NSegment *
get_node_aux(Addr from, Addr to)
{
   for(int i = 0; i < nsegments_used; ++i) {
      NSegment *seg = nsegments + i;
      if (seg->start == from && seg->end == to) return seg;
   }
   return new_leaf(NULL, SkResvn, from, to);
}

static NSegment *
get_node(Addr from, Addr to)
{
   NSegment *node = get_node_aux(from, to);
   //   printf("%ld:%ld --> %d\n", from, to, node - nsegments);
   return node;
}

static int
slurpline(FILE *fp, char *line)
{
   int n;

   n = 0;
   while (1) {
      int c = getc(fp);
      if (c == EOF || c == '\n') {
         line[n] = '\0';
//         printf("read: '%s'\n", line);
         return c == EOF ? EOF : 0;
      }
      line[n++] = c;
   }
}

/* Read a line; skip empty lines and lines containing comments only.
   Return EOF, if input is exhausted. */
static int
readline(FILE *fp, char *line)
{
   int n, skip;
   char *p;

 again:
   n = slurpline(fp, line);
   if (n == EOF) return EOF;

   skip = 1;
   for(p = line; *p; ++p) {
      if (isspace(*p)) continue;
      if (*p == '#') goto again;    // comment line
      skip = 0;
   }
   if (skip) goto again;
   return 0;
}

NSegment *
read_file(const char *file)
{
   NSegment *tree;
   char line[300];   // assumed long enough
   FILE *fp = fopen(file, "r");
   assert(fp);

   /* Add root node */
   Addr s, e;
   int n;
   n = readline(fp, line);
   assert(n != EOF);
   n = sscanf(line, "%ld:%ld", &s, &e);
   assert(n == 2);
   tree = get_node(s, e);

   while (1) {
      Addr fs, fe, ts, te;
      n = readline(fp, line);
      if (n == EOF) break;
      n = sscanf(line, "%ld:%ld -> %ld:%ld\n", &fs, &fe, &ts, &te);
      assert(n == 4);
   
      NSegment *from = get_node(fs, fe);
      NSegment *to   = get_node(ts, te);

      if (from->left == NULL) {
         from->left = to;
         to->up = from;
      } else if (from->right == NULL) {
         from->right = to;
         to->up = from;
      } else
         fprintf(stderr, "ERROR: %ld:%ld -> %ld:%ld\n", fs, fe, ts, te);
      /* Make sure the left subtree is the one with the interval at lower
         addresses */
      if (from->left != NULL && from->right != NULL) {
         if (from->left->start > from->right->start) {
            /* Swap subtrees */
            NSegment *tmp = from->left;
            from->left  = from->right;
            from->right = tmp;
         }
      }
   }
   fclose(fp);

   return tree;
}
