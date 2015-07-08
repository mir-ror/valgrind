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
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "../aspacemgr-segments.c"
#include "read.c"

int main(int argc, char *argv[])
{
   int do_insert = 0;
   int from = -1, to = -1;

   expensive_checking = True;  // we always want this here

   const char *file = NULL;
   for (int i=1; argv[i]; ++i) {
      if (strcmp(argv[i], "-v") == 0) {
         verbose = 1;
      } else if (strcmp(argv[i], "-n") == 0) {  // no post processing
         nopost = 1;
      } else if (strcmp(argv[i], "--no-post") == 0) {  // no post processing
         nopost = 1;
      } else if (strcmp(argv[i], "--no-rot") == 0) {  // no rotate
         norotate = 1;
      } else if (strcmp(argv[i], "-i") == 0) {
         ++i;
         assert(argv[i]);
         do_insert = 1;
         int n = sscanf(argv[i], "%d:%d", &from, &to);
         assert(n == 2);
      } else {
         assert(file == NULL);
         file = argv[i];
      }
   }
   assert(file);

   NSegment *tree = read_file(file);
   check_tree(tree);

// Write out the tree prior to inserting something
//   write_dot(tree, NULL, NULL, file);

   /* insert a segment */
   if (do_insert) {
      NSegment *subtree = locate_subtree_containing(root_segment(), from, to);
      NSegment *inserted = insert_node(from, to);
      check_tree(root_segment());
      char buf[strlen(file) + 40];   // large enough
      sprintf(buf, "%s-i%d:%d", file, from, to);
      write_dot(root_segment(), subtree, inserted, buf);
   }

   return 0;
}
