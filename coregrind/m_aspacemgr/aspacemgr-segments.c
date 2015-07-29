/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Address space manager segments                               ---*/
/*---                                         aspacemgr-segments.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2015-2015 Florian Krohm
   Copyright (C) 2000-2013 Julian Seward 
      jseward@acm.org

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

#include "priv_aspacemgr.h"

#ifdef ASPACEMGR_UNIT_TEST
/* Remove the alignment test so we can have segments with intervals that
   are small integers */
#undef  VG_IS_PAGE_ALIGNED
#define VG_IS_PAGE_ALIGNED(x) (1)
#define INTERVAL_FMT "[%lu:%lu]"
#define ADDR_FMT "%lu"

static Bool norotate = False;
static Bool nopost = False;
#else

#define INTERVAL_FMT "[%lx:%lx]"
#define ADDR_FMT "%lx"

#endif

/* Max number of segments we can track.  On Android, virtual address
   space is limited, so keep a low limit -- 5000 x sizef(NSegment) is
   360KB. */
#if defined(VGPV_arm_linux_android) \
    || defined(VGPV_x86_linux_android) \
    || defined(VGPV_mips32_linux_android) \
    || defined(VGPV_arm64_linux_android)
# define VG_N_SEGMENTS 5000
#else
# define VG_N_SEGMENTS 30000
#endif

/* Array [0 .. nsegments_used-1] of all mappings. */
/* I: zero-length segments are not allowed. */
/* I: overlapping segments are not allowed. */
/* I: the segments cover the entire address space precisely. */
/* Each segment can optionally hold an index into the filename table. */

static NSegment nsegments[VG_N_SEGMENTS];
static Int      nsegments_used = 0;

/* NSegment::fnIdx == ON_FREELIST identifies a segment to be on the
   freelist */
#define ON_FREELIST -42

/* Freelist of segments; deleted segments will be added here. Segments
   on this list will be chained together via the 'left' link. */
static NSegment *segment_freelist;

/* For debugging purposes */
static Bool verbose = False;
static Bool expensive_checking = False;

#undef DEBUG
#define DEBUG(fmt, ...) \
   do { \
      if (verbose) VG_(debugLog)(0, "aspacem", fmt, ##__VA_ARGS__); \
   } while (0)

/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Low level access of segment nodes.                        ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static inline NSegment *root_segment( void )
{
   return nsegments + 0;
}

static inline Bool is_leaf( const NSegment *node )
{
   aspacem_assert(node != NULL);

   return node->left == NULL && node->right == NULL;
}

/* In the segment tree rooted at NODE return the leftmost leaf node */
static NSegment *leftmost_leaf( NSegment *node )
{
   aspacem_assert(node != NULL);

   while (! is_leaf(node))
      node = node->left;
   return node;
}

/* In the segment tree rooted at NODE return the rightmost leaf node */
static NSegment *rightmost_leaf( NSegment *node )
{
   aspacem_assert(node != NULL);

   while (! is_leaf(node))
      node = node->right;
   return node;
}

/* From the ordered collection of segments, return the segment
   succeeding SEG, or NULL if there is no such segment. */
NSegment *ML_(am_next_segment)( const NSegment *node )
{
   aspacem_assert(is_leaf(node));

   const NSegment *parent;

   while ((parent = node->up) != NULL) {
      if (node == parent->left)
         /* NODE is a left subtree. Descend into the corresponding right
            subtree and return the leftmost leaf. */
         return leftmost_leaf(parent->right);
      /* NODE is a right subtree. Climb up until we either end up at the
         root node (i.e. there is no next forward segment) or find ourselves
         being the left subtree of some node. */
      node = parent;
   }

   return NULL;   // no succeeding segment
}

/* From the ordered collection of segments, return the segment
   preceding SEG, or NULL if there is no such segment. */
NSegment *ML_(am_prev_segment)( const NSegment *node )
{
   aspacem_assert(is_leaf(node));

   NSegment *parent;

   while ((parent = node->up) != NULL) {
      if (node == parent->right)
         /* NODE is a right subtree. Descend into the corresponding left
            subtree and return the rightmost leaf. */
         return rightmost_leaf(parent->left);
      /* NODE is a left subtree. Climb up until we either end up at the
         root node (i.e. there is no next backward segment) or find ourselves
         being the rightsubtree of some node. */
      node = parent;
   }

   return NULL;   // no preceding segment
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Displaying segments.                                      ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

static const HChar *show_SegKind( SegKind sk )
{
   switch (sk) {
      case SkFree:  return "    ";
      case SkAnonC: return "anon";
      case SkAnonV: return "ANON";
      case SkFileC: return "file";
      case SkFileV: return "FILE";
      case SkShmC:  return "shm ";
      case SkResvn: return "RSVN";
      default:      return "????";
   }
}

static const HChar *show_ShrinkMode( ShrinkMode sm )
{
   switch (sm) {
      case SmLower: return "SmLower";
      case SmUpper: return "SmUpper";
      case SmFixed: return "SmFixed";
      default: return "Sm?????";
   }
}

/* Show a segment in a user-friendly-ish way. */
static void show_segment( Int logLevel, Int segNo, const NSegment *seg )
{
   HChar len_buf[20];
   
   ML_(am_show_len_concisely)(len_buf, seg->start, seg->end);

   switch (seg->kind) {
      case SkFree:
         VG_(debugLog)(logLevel, "aspacem",
                       "%3d: %s %010llx-%010llx %s\n",
                       segNo, show_SegKind(seg->kind),
                       (ULong)seg->start, (ULong)seg->end, len_buf);
         break;

      case SkAnonC: case SkAnonV: case SkShmC:
         VG_(debugLog)(logLevel, "aspacem",
                       "%3d: %s %010llx-%010llx %s %c%c%c%c%c\n",
                       segNo, show_SegKind(seg->kind),
                       (ULong)seg->start, (ULong)seg->end, len_buf,
                       seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
                       seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-',
                       seg->whatsit);
         break;

      case SkFileC: case SkFileV:
         VG_(debugLog)(logLevel, "aspacem",
                       "%3d: %s %010llx-%010llx %s %c%c%c%c%c d=0x%03llx "
                       "i=%-7lld o=%-7lld (%d,%d)\n",
                       segNo, show_SegKind(seg->kind),
                       (ULong)seg->start, (ULong)seg->end, len_buf,
                       seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
                       seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-', 
                       seg->whatsit,
                       seg->dev, seg->ino, seg->offset,
                       ML_(am_segname_get_seqnr)(seg->fnIdx), seg->fnIdx);
         break;

      case SkResvn:
         VG_(debugLog)(logLevel, "aspacem",
                       "%3d: %s %010llx-%010llx %s %c%c%c%c%c %s\n",
                       segNo, show_SegKind(seg->kind),
                       (ULong)seg->start, (ULong)seg->end, len_buf,
                       seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
                       seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-', 
                       seg->whatsit,
                       show_ShrinkMode(seg->smode));
         break;

      default:
         VG_(debugLog)(logLevel, "aspacem",
                       "%3d: ???? UNKNOWN SEGMENT KIND\n", segNo);
         break;
   }
}

static void show_segments( Int logLevel, Int *segNo, const HChar *who,
                           const NSegment *seg )
{
   if (is_leaf(seg)) {
      show_segment( logLevel, (*segNo)++, seg );
   } else {
      // recurse
      show_segments( logLevel, segNo, who, seg->left );
      show_segments( logLevel, segNo, who, seg->right );
   }
}

/* Return the number of segments (leaf nodes) in the tree rooted at NODE. */
static UInt segment_count( const NSegment *node )
{
   if (is_leaf(node)) return 1;
   return segment_count(node->left) + segment_count(node->right);
}

/* Print out the segments (debugging only!). */
void VG_(am_show_nsegments) ( Int logLevel, const HChar *who )
{
   VG_(debugLog)(logLevel, "aspacem",
                 "<<< SHOW_SEGMENTS: %s (%u segments)\n", 
                 who, segment_count(root_segment()));
   ML_(am_show_segnames)( logLevel, who);

   Int segNo = 0;
   show_segments( logLevel, &segNo, who, root_segment() );

   VG_(debugLog)(logLevel, "aspacem",
                 ">>>\n");
}

/* Show full details of a segment. */
void ML_(am_show_segment_full)( Int logLevel, Int segNo, const NSegment *seg )
{
   HChar len_buf[20];
   const HChar *name = ML_(am_get_segname)( seg->fnIdx );

   if (name == NULL)
      name = "(none)";

   ML_(am_show_len_concisely)( len_buf, seg->start, seg->end );

   VG_(debugLog)(logLevel, "aspacem",
                 "%3d: %s %010llx-%010llx %s %c%c%c%c%c %s "
                 "d=0x%03llx i=%-7lld o=%-7lld (%d,%d) %s\n",
                 segNo, show_SegKind(seg->kind),
                 (ULong)seg->start, (ULong)seg->end, len_buf,
                 seg->hasR ? 'r' : '-', seg->hasW ? 'w' : '-', 
                 seg->hasX ? 'x' : '-', seg->hasT ? 'T' : '-',
                 seg->whatsit,
                 show_ShrinkMode(seg->smode),
                 seg->dev, seg->ino, seg->offset,
                 ML_(am_segname_get_seqnr)(seg->fnIdx), seg->fnIdx,
                 name);
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Segment sanity checking and preening.                     ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Check representational invariants for segments. */
static Bool sane_segment ( const NSegment *s )
{
   if (s == NULL) return False;

   /* No zero sized segments and no wraparounds. */
   if (s->start > s->end) return False;

   /* require page alignment */
   if (!VG_IS_PAGE_ALIGNED(s->start)) return False;
   if (!VG_IS_PAGE_ALIGNED(s->end+1)) return False;

   /* Only the root node may have a NULL up link */
   if (s != root_segment())
      if (s->up == NULL) return False;

   /* Either both subtrees are NULL or both are != NULL.
      In the latter case they must point to different segments. */
   if (s->left == NULL) {
      if (s->right != NULL) return False;
   } else {
      if (s->left == NULL) return False;
      /* Both subtrees != NULL */
      if (s->left == s->right) return False;
      if (s->left == s->up) return False;
      if (s->right == s->up) return False;
      /* Check address range containment */
      if (s->start != s->left->start) return False;
      if (s->end   != s->right->end)  return False;
      /* Check that subtree address range abut */
      if (s->right->start != s->left->end + 1) return False;
   }

   switch (s->kind) {
      case SkFree:
         return 
            s->smode == SmFixed
            && s->dev == 0 && s->ino == 0 && s->offset == 0 && s->fnIdx == -1 
            && !s->hasR && !s->hasW && !s->hasX && !s->hasT
            && s->whatsit == WiUnknown;

      case SkAnonC: case SkAnonV: case SkShmC:
         return 
            s->smode == SmFixed 
            && s->dev == 0 && s->ino == 0 && s->offset == 0 && s->fnIdx == -1
            && (s->kind==SkAnonC ? True : s->whatsit == WiUnknown);

      case SkFileC: case SkFileV:
         return 
            s->smode == SmFixed
            && ML_(am_sane_segname)(s->fnIdx)
            && s->whatsit == WiUnknown;

      case SkResvn: 
         return 
            s->dev == 0 && s->ino == 0 && s->offset == 0 && s->fnIdx == -1 
            && !s->hasR && !s->hasW && !s->hasX && !s->hasT
            && (s->whatsit == WiClientBreak || s->whatsit == WiClientStack ||
                s->whatsit == WiUnknown);

      default:
         return False;
   }
}

static void
check_tree( const NSegment *tree )
{
   if (tree == NULL) return;

   if (! sane_segment(tree)) {
      VG_(debugLog)(0, "aspacemgr", "*** node "INTERVAL_FMT" is busted\n",
                    tree->start, tree->end);
      ML_(am_show_segment_full)(0, -1, tree);

      ML_(am_write_dot)(root_segment(), "busted.dot");
      aspacem_assert(0);
   }
   check_tree(tree->left);
   check_tree(tree->right);
}


/* Try merging s2 into s1, if possible.  If successful, s1 is
   modified, and True is returned.  Otherwise s1 is unchanged and
   False is returned. */
static Bool maybe_merge_segments( NSegment *s1, const NSegment *s2 )
{
   if (s1->kind != s2->kind) 
      return False;

   if (s1->end+1 != s2->start)
      return False;

   /* reject cases which would cause wraparound */
   if (s1->start > s2->end)
      return False;

   switch (s1->kind) {
      case SkFree:
         s1->end = s2->end;
         return True;

      case SkAnonC: case SkAnonV:
         if (s1->hasR == s2->hasR && s1->hasW == s2->hasW 
             && s1->hasX == s2->hasX && s1->whatsit == s2->whatsit) {
            s1->end = s2->end;
            s1->hasT |= s2->hasT;
            return True;
         }
         break;

      case SkFileC: case SkFileV:
         if (s1->hasR == s2->hasR 
             && s1->hasW == s2->hasW && s1->hasX == s2->hasX
             && s1->dev == s2->dev && s1->ino == s2->ino
             && s2->offset == s1->offset
                              + ((ULong)s2->start) - ((ULong)s1->start) ) {
            s1->end = s2->end;
            s1->hasT |= s2->hasT;
            ML_(am_dec_refcount)(s1->fnIdx);
            return True;
         }
         break;

      case SkShmC:
         return False;

      case SkResvn:
         if (s1->smode == SmFixed && s2->smode == SmFixed) {
            s1->end = s2->end;
            return True;
         }

      default:
         break;
   
   }
   return False;
}

/* Sanity-check and canonicalise the segment structure (merge mergable
   segments). */
static void preen_segments( void )
{
   Int i, r, w;

   /* Pass 1: check the segment array covers the entire address space
      exactly once, and also that each segment is sane. */
   aspacem_assert(nsegments_used > 0);
   aspacem_assert(nsegments[0].start == Addr_MIN);
   aspacem_assert(nsegments[nsegments_used-1].end == Addr_MAX);

   aspacem_assert(sane_segment(&nsegments[0]));
   for (i = 1; i < nsegments_used; i++) {
      aspacem_assert(sane_segment(&nsegments[i]));
      aspacem_assert(nsegments[i-1].end+1 == nsegments[i].start);
   }

   /* Pass 2: merge as much as possible, using
      maybe_merge_segments. */
   w = 0;
   for (r = 1; r < nsegments_used; r++) {
      if (maybe_merge_segments(&nsegments[w], &nsegments[r])) {
         /* nothing */
      } else {
         w++;
         if (w != r) 
            nsegments[w] = nsegments[r];
      }
   }
   w++;
   aspacem_assert(w > 0 && w <= nsegments_used);
   nsegments_used = w;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Segment query and traversal.                              ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Get the filename corresponding to this segment, if known and if it
   has one. */
const HChar *VG_(am_get_filename)( const NSegment *seg )
{
   aspacem_assert(seg);
   return ML_(am_get_segname)( seg->fnIdx );
}

/* Binary search the segment tree for a given address.  Since the
   tree covers the entire address space the search cannot fail.  The
   _WRK function does the real work.  Its caller (just below) caches
   the results thereof, to save time.  With N_CACHE of 63 we get a hit
   rate exceeding 90% when running OpenOffice.

   Re ">> 12", it doesn't matter that the page size of some targets
   might be different from 12.  Really "(a >> 12) % N_CACHE" is merely
   a hash function, and the actual cache entry is always validated
   correctly against the selected cache entry before use.
*/
/* Don't call find_segment_WRK; use find_segment instead. */
static NSegment *find_segment_WRK( Addr a )
{
   NSegment *seg = root_segment();    // start at the root segment

   while (True) {
      if (is_leaf(seg)) return seg;        // found
      NSegment *left  = seg->left;
      NSegment *right = seg->right;
      if (a < right->start) {
         seg = left;
         continue;
      }
      if (a > left->end) {
         seg = right;
         continue;
      }
      /* Not found.  This can't happen. */
      aspacem_assert(0);
   }
}

/* This function never returns NULL */
NSegment *ML_(am_find_segment)( Addr a )
{
#  define N_CACHE 131 /*prime*/
   static Addr cache_pageno[N_CACHE];
   static NSegment *cache_seg[N_CACHE];
   static Bool cache_inited = False;

   static UWord n_q = 0;
   static UWord n_m = 0;

   UWord ix;

   if (LIKELY(cache_inited)) {
      /* do nothing */
   } else {
      for (ix = 0; ix < N_CACHE; ix++) {
         cache_pageno[ix] = 0;
         cache_seg[ix] = NULL;
      }
      cache_inited = True;
   }

   ix = (a >> 12) % N_CACHE;

   n_q++;
   if (0 && 0 == (n_q & 0xFFFF))
      VG_(debugLog)(0,"xxx","find_segment: %lu %lu\n", n_q, n_m);

   if ((a >> 12) == cache_pageno[ix]
       && cache_seg[ix] != NULL
       && cache_seg[ix]->fnIdx != ON_FREELIST
       && is_leaf(cache_seg[ix])
       && cache_seg[ix]->start <= a
       && a <= cache_seg[ix]->end) {
      /* hit */
      /* aspacem_assert( cache_segidx[ix] == find_segment_idx_WRK(a) ); */
      return cache_seg[ix];
   } else {
      /* miss */
      n_m++;
      cache_seg[ix] = find_segment_WRK(a);
      cache_pageno[ix] = a >> 12;
      return cache_seg[ix];
   }
#  undef N_CACHE
}

/* Finds the segment containing 'a'.  Only returns non-SkFree segments. */
const NSegment *VG_(am_find_nsegment)( Addr a )
{
   const NSegment *seg = ML_(am_find_segment)(a);

   aspacem_assert(seg->start <= a);
   aspacem_assert(a <= seg->end);

   return seg->kind == SkFree ? NULL : seg;
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Constructing segments and low level access.               ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Initialise the common fields of an NSegment */
void ML_(am_init_segment)( /*OUT*/NSegment *seg, SegKind kind, Addr start,
                           Addr end)
{
   seg->kind     = kind;
   seg->start    = start;
   seg->end      = end;
   seg->smode    = SmFixed;
   seg->dev      = 0;
   seg->ino      = 0;
   seg->mode     = 0;
   seg->offset   = 0;
   seg->fnIdx    = -1;
   seg->hasR = seg->hasW = seg->hasX = seg->hasT = False;
   seg->whatsit = WiUnknown;
   seg->left = seg->right = seg->up = NULL;
}

/* Allocate a new segment. */
static NSegment *new_node( void )
{
   /* Check whether there are is a node on the freelist */
   if (segment_freelist) {
      NSegment *new = segment_freelist;
      segment_freelist = segment_freelist->left;
      aspacem_assert(new->fnIdx == ON_FREELIST);
      return new;
   }

   if (nsegments_used >= VG_N_SEGMENTS)
      ML_(am_barf_toolow)("VG_N_SEGMENTS");
   return nsegments + nsegments_used++;
}

static NSegment *
new_leaf( NSegment *parent, SegKind kind, const Addr from, const Addr to )
{
   NSegment *node = new_node();

   DEBUG("adding leaf "INTERVAL_FMT"\n", from, to);

   ML_(am_init_segment)(node, kind, from, to);
   node->left  = node->right = NULL;
   node->up    = parent;

   return node;
}

/* Clone a segment. This creates a new leaf node. */
static NSegment *clone_segment( const NSegment *seg )
{
   aspacem_assert(is_leaf(seg));

   DEBUG("cloning leaf "INTERVAL_FMT"\n", seg->start, seg->end);

   NSegment *new = new_node();

   *new = *seg;
   new->left = new->right = new->up = NULL;

   ML_(am_inc_refcount)(seg->fnIdx);

   return new;
}

static void delete_node( NSegment *node )
{
   DEBUG("removing %s "INTERVAL_FMT"\n",
         is_leaf(node) ? "leaf" : "node", node->start, node->end);

   ML_(am_dec_refcount)(node->fnIdx);

   /* Chain into freelist */
   node->left = node->right = node->up = NULL;
   node->fnIdx = ON_FREELIST;
   node->left = segment_freelist;
   segment_freelist = node;
}

static void delete_subtree( NSegment *tree )
{
   if (tree == NULL) return;   // convenience
   delete_subtree(tree->left);
   delete_subtree(tree->right);
   delete_node(tree);
}

 
/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Modifying the segment structure.                          ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Split SEG with interval [p:q] into two segments [p:a-1] and [a:q] */
static void split_segment_at_WRK ( NSegment *seg, const Addr addr )
{
   aspacem_assert(is_leaf(seg));

   DEBUG("splitting "INTERVAL_FMT" @ "ADDR_FMT"\n", seg->start, seg->end, addr);

   NSegment *left = clone_segment(seg);
   left->start = seg->start;
   left->end   = addr - 1;
   left->up    = seg;

   NSegment *right = clone_segment(seg);
   right->start = addr;
   right->end   = seg->end;
   right->up    = seg;

   if (seg->kind == SkFileV || seg->kind == SkFileC)
      right->offset += ((ULong)right->start) - ((ULong)seg->start);

   ML_(am_dec_refcount)(seg->fnIdx);

   /* SEG is now an internal node whose kind is irrelevant. Give it
      some non-file kind, so we can decrease the reference count on
      its segment name. */
   NSegment *up = seg->up;
   ML_(am_init_segment)(seg, SkResvn, seg->start, seg->end);
   seg->up = up;
   seg->left  = left;
   seg->right = right;
}

/* Split the segment containing 'a' into two, so that 'a' is
   guaranteed to be the start of a new segment.  If 'a' is already the
   start of a segment, do nothing. */
static void split_segment_at( Addr a )
{
   aspacem_assert(a > Addr_MIN);
   aspacem_assert(VG_IS_PAGE_ALIGNED(a));
 
   NSegment *seg = ML_(am_find_segment)(a);

   if (seg->start == a)
      /* 'a' is already the start point of a segment, so nothing to be
         done. */
      return;

   split_segment_at_WRK( seg, a );
}


/* Do the minimum amount of segment splitting necessary to ensure that
   sLo is the first address denoted by some segment and sHi is the
   highest address denoted by some other segment.  Returns the indices
   of the lowest and highest segments in the range. */
static void split_segments_lo_and_hi( Addr sLo, Addr sHi,
                                      /*OUT*/NSegment **segLo,
                                      /*OUT*/NSegment **segHi )
{
   aspacem_assert(sLo < sHi);
   aspacem_assert(VG_IS_PAGE_ALIGNED(sLo));
   aspacem_assert(VG_IS_PAGE_ALIGNED(sHi+1));

   if (sLo > Addr_MIN)
      split_segment_at(sLo);
   if (sHi < Addr_MAX)
      split_segment_at(sHi + 1);

   *segLo = ML_(am_find_segment)(sLo);
   *segHi = ML_(am_find_segment)(sHi);

   aspacem_assert((*segLo)->start == sLo);
   aspacem_assert((*segHi)->end == sHi);
   /* Not that I'm overly paranoid or anything, definitely not :-) */
}


/* Locate the subtree X containing the interval [from:to] such that no
   subtree of X contains [from:to]. This function never returns NULL. */
static NSegment *
locate_subtree_containing(NSegment *tree, const Addr from, const Addr to)
{
   aspacem_assert(tree != NULL);
   aspacem_assert(from >= tree->start);
   aspacem_assert(to   <= tree->end);

   while (True) {
      NSegment *left = tree->left;

      /* If TREE is a leaf node, then the node's interval must contain
         [from:to]. */
      if (left == NULL) {
         aspacem_assert(from >= tree->start && to <= tree->end);
         return tree;
      }

      /* TREE is an internal node. */

      /* Decide which subtree to dive into */
      if (from >= left->start && to <= left->end) {
         tree = left;
         continue;
      }

      NSegment *right = tree->right;
      if (from >= right->start && to <= right->end) {
         tree = right;
         continue;
      }

      aspacem_assert(from >= tree->start && to <= tree->end);
      return tree;
   }
}

static void
do_left_subtree_aux(NSegment *node, NSegment *parent, const Addr from)
{
   DEBUG("processing node "INTERVAL_FMT"\n", node->start, node->end);

   if (is_leaf(node)) {
      aspacem_assert(from > node->start && from <= node->end);
      /* Split node;  Tests:  l1 30:100    big 6:18   l2 25:50 */
      split_segment_at_WRK(node, from);
      return;
   }

   /* No need to check whether the left subtree can be removed. */

   /* Check whether right subtree can be removed */
   NSegment *right = node->right;
   while (right->start >= from) {
      if (right->start == from) {
         /* Remove right subtree      Tests:   l2 31:50 */
         node->right = new_leaf(node, SkResvn, right->start, node->end);
         delete_subtree(right);
         /* We're done. No need to traverse the node->left because its
            interval cannot intersect with [from:to] */
         return;
      }
      
      /* right->start > from */
      delete_subtree(right);

      NSegment *left = node->left;
      if (is_leaf(left))
         split_segment_at_WRK(left, from);
      node->left  = left->left;
      node->right = left->right;
      delete_node(left);
      left  = node->left;
      right = node->right;
      left->up = right->up = node;
      right->end = node->end;

      /* Iterate... */
      DEBUG("... iterating with "INTERVAL_FMT"\n", node->start, node->end);
   }

   /* The right subtree could not be removed. Descent into the right
      subtree. Note, that 'from' cannot be located in the left subtree.
      Because, if it was, then we would have removed the right subtree.
      But when we get here, we haven't. */
   aspacem_assert(from > right->start && from <= right->end);
   // Continue in right subtree
   DEBUG("... continuing RIGHT with "INTERVAL_FMT"\n", right->start,
         right->end);
   right->end = node->end;
   do_left_subtree_aux(right, node, from);
}

static void
do_left_subtree(NSegment *tree, const Addr from)
{
   aspacem_assert(tree != NULL);

   DEBUG("DOING LEFT SUBTREE\n");

   NSegment *node = tree->left;

   /* By construction FROM is covered by some node in the left subtree 
      of TREE. This is ensured by locate_subtree_containing. */

   /* If the left subtree has interval [from:...] it will be replaced
      with a leaf node having the same interval as the tree it replaces. */
   if (node->start == from) {   // Tests: l1 0:60     big 0:27
      tree->left = new_leaf(tree, SkResvn, node->start, node->end);
      delete_subtree(node);
      return;
   }

   do_left_subtree_aux(node, tree, from);

   /* Rearrange nodes such that rightmost leaf is moved up */
#ifdef ASPACEMGR_UNIT_TEST
   if (norotate) return;
#endif

   NSegment *top = tree->left;
   while (1) {
      NSegment *l1 = top->left;
      NSegment *r1 = top->right;
      if (is_leaf(r1)) break;
      /*
              top                         top
             /   \                       /   \
            /     \                     /     \
           l1     r1       ====>       new    r2
                 /  \                 /   \
                /    \               /     \
               l2    r2             l1     l2
      */
      // Do the transformation in place without allocating/freeing nodes. */
      NSegment *l2 = r1->left;
      NSegment *r2 = r1->right;
      NSegment *old_r1 = top->right;        // stash away r1

      // move r2 into place
      top->right = r1->right;
      r2->up = r1->up;

      // arrange for 'new' (previously r1)
      NSegment *new = r1;
      new->start = l1->start;
      new->end = l2->end;

      // move l2 into place
      new->right = r1->left;

      // move l1 into place
      new->left = top->left;
      l1->up = old_r1;

      // move new into place
      top->left = old_r1;
   }
}

static void
do_right_subtree_aux(NSegment *node, NSegment *parent, const Addr to)
{
   DEBUG("processing node "INTERVAL_FMT"\n", node->start, node->end);

   if (is_leaf(node)) {
      aspacem_assert(to >= node->start && to < node->end);
      /* Split node;   Tests: l1 0:60     big 6:27     r2 0:10  */
      split_segment_at_WRK(node, to + 1);
      return;
   }

   /* No need to check whether the right subtree can be removed. */

   /* Check whether left subtree can be removed */
   NSegment *left = node->left;
   while (left->end <= to) {
      if (left->end == to) {
         /* Remove left subtree       Tests:   r2 0:50 */
         node->left = new_leaf(node, SkResvn, node->start, left->end);
         delete_subtree(left);
         /* We're done. No need to traverse the node->right because its
            interval cannot intersect with [from:to] */
         return;
      }

      /* left->end < to */
      /* Remove left subtree */
      delete_subtree(left);

      NSegment *right = node->right;
      if (is_leaf(right))
         split_segment_at_WRK(right, to + 1);
      node->left  = right->left;
      node->right = right->right;
      delete_node(right);
      left  = node->left;
      right = node->right;
      left->up = right->up = node;
      left->start = node->start;

      /* Iterate... */
      DEBUG("... iterating with "INTERVAL_FMT"\n", node->start, node->end);
   }

   /* The left subtree could not be removed. Descent into the left
      subtree. Note, that 'to' cannot be located in the right subtree.
      Because, if it was, then we would have removed the left subtree.
      But when we get here, we haven't. */
   aspacem_assert(to >= left->start && to < left->end);
   // Continue in left subtree
   DEBUG("... continuing LEFT with "INTERVAL_FMT"\n", left->start, left->end);
   left->start = node->start;
   do_right_subtree_aux(left, node, to);
}

static void
do_right_subtree(NSegment *tree, const Addr to)
{
   aspacem_assert(tree != NULL);

   DEBUG("DOING RIGHT SUBTREE\n");

   NSegment *node = tree->right;

   /* By construction TO is covered by some node in the right subtree 
      of TREE. This is ensured by locate_subtree_containing. */

   /* If the right subtree has interval [...:to] it will be replaced
      with a leaf node having the same interval as the tree it replaces. */
   if (node->end == to) {    // Tests: l1 30:100     big: 6:19
      tree->right = new_leaf(tree, SkResvn, node->start, node->end);
      delete_subtree(node);
      return;
   }

   do_right_subtree_aux(node, tree, to);

#ifdef ASPACEMGR_UNIT_TEST
   if (norotate) return;
#endif

   /* Rearrange nodes such that leftmost leaf is moved up */
   NSegment *top = tree->right;
   while (1) {
      NSegment *l1 = top->left;
      NSegment *r1 = top->right;
      if (is_leaf(l1)) break;
      /*
                top                         top
               /   \                       /   \
              /     \                     /     \
             l1     r1       ====>       l2     new
            /  \                               /   \
           /    \                             /     \
          l2    r2                           r2     r1
      */
      // Do the transformation in place without allocating/freeing nodes. */
      NSegment *l2 = l1->left;
      NSegment *r2 = l1->right;
      NSegment *old_l1 = top->left;         // stash away l1

      // move l2 into place
      top->left = l1->left;
      l2->up = l1->up;

      // arrange for 'new' (previously l1)
      NSegment *new = l1;
      new->start = r2->start;
      new->end = r1->end;

      // move r2 into place
      new->left = l1->right;

      // move r1 into place
      new->right = top->right;
      r1->up = old_l1;

      // move new into place
      top->right = old_l1;
   }
}

/* Return pointer to subtree */
static NSegment *
insert_node(Addr from, Addr to)
{
   NSegment *root = root_segment();

   /* Extra check here to make sure, the interval is contained in the
      interval of the root node */
   aspacem_assert(from >= root->start);
   aspacem_assert(to   <= root->end);

   /* Step 1: Locate the subtree containing [from:to] */
   NSegment *subtree = locate_subtree_containing(root, from, to);
   aspacem_assert(subtree != NULL);
   DEBUG("subtree root  "INTERVAL_FMT"\n", subtree->start, subtree->end);

   /* If the interval of the subtree equals [from:to], then remove the
      subtree and return a leaf node with interval [from:to]. */
   if (subtree->start == from && subtree->end == to) {
      /* Tests: big 5:25, big 14:16 */
      /* Replacing the root segment is not allowed and should never occur. */
      aspacem_assert(subtree != root);
      NSegment *parent = subtree->up;
      NSegment *new = new_leaf(parent, SkResvn, from, to);
      if (subtree == parent->left)
         parent->left = new;
      else
         parent->right = new;
      delete_subtree(subtree);
      return new;
   }

   /* Special handling when subtree is a leaf node */
   if (is_leaf(subtree)) {
      if (subtree->start == from) {     // Tests: l1 0:30    r1 51:80
         split_segment_at_WRK(subtree, to + 1);
         return subtree->left;
      } else if (subtree->end == to) {  // Tests: l1 20:50   r2 40:50
         split_segment_at_WRK(subtree, from);
         return subtree->right;
      } else {                          // Tests: l1 30:40
         split_segment_at_WRK(subtree, from);
         split_segment_at_WRK(subtree->right, to + 1);
         return subtree->right->left;
      }
   }

   /* Step 2: Remove all intervals that are being replaced by [from:to] */
   do_left_subtree(subtree, from);
   do_right_subtree(subtree, to);

#ifdef ASPACEMGR_UNIT_TEST
   if (nopost) return subtree;  // return values does not matter
#endif

   DEBUG("POST PROCESSING\n");
   // It cannot be that both children of the subtree are leaves. 
   // Assume both children L and R are leaves with (L) = [p,x] and
   // (R) = [x+1,q]. As L needs to contain a node [from,...] it follows
   // that p = from. Likewise, R needs to contain a node [...,to] and
   // therefore q = to. So (subtree) = [p,q] = [from,to]. But that is
   // a special case that we already considered above.
   aspacem_assert(! (is_leaf(subtree->left) && is_leaf(subtree->right)));
   NSegment *l = subtree->left;
   NSegment *r = subtree->right;
   if (is_leaf(l)) {        // Tests: l1 0:60
      /* Consider inserting [0:60] in the tree on the left. The tree in
         the middle is 'subtree'. The tree on the right is what 'subtree'
         gets transformed into.

               0:100              0:100                0:100
              /     \     -->    /     \     -->      /     \
            0:50  51:100       0:50  51:100         0:60  61:100
                                    /      \
                                 51:60   61:100
      */
      NSegment *rl = r->left;
      NSegment *rr = r->right;
      aspacem_assert(is_leaf(rl));
      aspacem_assert(l->end + 1 == rl->start);
      l->end = rl->end;
      delete_node(rl);
      subtree->right = r->right;
      rr->up = r->up;
      delete_node(r);
      return subtree->left;
   } else if (is_leaf(r)) {   // Tests: l1 30:100
      /* Consider inserting [30:100] in the tree on the left. The tree in
         the middle is 'subtree'. The tree on the right is what 'subtree'
         gets transformed into.

               0:100              0:100                0:100
              /     \     -->    /     \     -->      /     \
            0:50  51:100       0:50  51:100         0:29  30:100
                              /    \
                            0:29 30:50
      */
      NSegment *ll = l->left;
      NSegment *lr = l->right;
      aspacem_assert(is_leaf(lr));
      aspacem_assert(lr->end + 1 == r->start);
      r->start = lr->start;
      delete_node(lr);
      subtree->left = l->left;
      ll->up = l->up;
      delete_node(l);
      return subtree->right;
   } else {                   // Tests: l1 30:80
      /* Consider inserting [30:80] in the tree on the left. The tree in
         the middle is 'subtree'. The tree on the right is what 'subtree'
         gets transformed into.

               0:100              ___0:100___                0:100
              /     \     -->    /           \     -->      /     \
            0:50  51:100       0:50        51:100         0:80  81:100
                              /    \      /     \        /    \
                            0:29 30:50  51:80 81:100   0:29 30:80
      */
      NSegment *lr = l->right;
      NSegment *rl = r->left;
      aspacem_assert(is_leaf(lr));
      aspacem_assert(is_leaf(rl));
      aspacem_assert(lr->end + 1 == rl->start);

      NSegment *new = new_leaf(l, SkResvn, from, to);  // to replace lr
      l->end = new->end;
      l->right = new;
      delete_node(lr);

      NSegment *rr = r->right;
      rr->up = r->up;
      subtree->right = r->right;
      delete_node(rl);
      delete_node(r);
      return subtree->left->right;
   }
}

/* Add SEG to the collection, deleting/truncating any it overlaps.
   This deals with all the tricky cases of splitting up segments as
   needed. */
void ML_(am_add_segment)( const NSegment *seg )
{
   DEBUG("adding segment "INTERVAL_FMT"\n", seg->start, seg->end);

   NSegment *new = insert_node(seg->start, seg->end);

   aspacem_assert(new->start == seg->start && new->end == seg->end);

   /* Initialise the new segment with the template segment, but be careful
      not to overwrite the left, right, and up links. */
   NSegment *left = new->left;
   NSegment *right = new->right;
   NSegment *up = new->up;

   *new = *seg;
   new->up = up;
   new->left = left;
   new->right = right;

   if (expensive_checking) check_tree(root_segment());
   //   ML_(am_write_dot)(root_segment(), "add.dot");
}

/* Add a single interval covering the entire address space. */
void ML_(am_segments_init)( void )
{
   NSegment seg;

   ML_(am_init_segment)(&seg, SkFree, Addr_MIN, Addr_MAX);
   nsegments[0]    = seg;
   nsegments_used  = 1;
}

/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Miscellaneous.                                            ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Helper function for VG_(am_get_anonsize_total) */
static void
sumup_anonsize( const NSegment *seg, ULong *total )
{
   if (is_leaf(seg)) {
      if (seg->kind == SkAnonC || seg->kind == SkAnonV)
         *total += seg->end - seg->start + 1;
      return;
   }
   /* Recurse */
   sumup_anonsize(seg->left, total);
   sumup_anonsize(seg->right, total);
}

/* Return the total amount of space in anonymous mappings,
   both for V and the client.  Is used for printing stats in
   out-of-memory messages. */
ULong VG_(am_get_anonsize_total)( void )
{
   ULong total = 0;
   sumup_anonsize(root_segment(), &total);
   return total;
}

/* Collect up the start addresses of segments whose kind matches one of
   the kinds specified in kind_mask.
   The interface is a bit strange in order to avoid potential
   segment-creation races caused by dynamic allocation of the result
   buffer *starts.

   The function first computes how many entries in the result
   buffer *starts will be needed.  If this number <= nStarts,
   they are placed in starts[0..], and the number is returned.
   If nStarts is not large enough, nothing is written to
   starts[0..], and the negation of the size is returned.

   Correct use of this function may mean calling it multiple times in
   order to establish a suitably-sized buffer. */
Int VG_(am_get_segment_starts)( UInt kind_mask, Addr *starts, Int nStarts )
{
   Int i, j, nSegs;

   /* don't pass dumbass arguments */
   aspacem_assert(nStarts > 0);

   nSegs = 0;
   for (i = 0; i < nsegments_used; i++) {  // FIXME: works; but assumes array
      const NSegment *seg = nsegments + i;
      if (seg->fnIdx == ON_FREELIST || !is_leaf(seg)) continue;
      if ((seg->kind & kind_mask) != 0)
         nSegs++;
   }

   if (nSegs > nStarts) {
      /* The buffer isn't big enough.  Tell the caller how big it needs
         to be. */
      return -nSegs;
   }

   /* There's enough space.  So write into the result buffer. */
   aspacem_assert(nSegs <= nStarts);

   j = 0;
   for (i = 0; i < nsegments_used; i++) {  // FIXME: works; but assumes array
      const NSegment *seg = nsegments + i;
      if (seg->fnIdx == ON_FREELIST || !is_leaf(seg)) continue;
      if ((seg->kind & kind_mask) != 0)
         starts[j++] = seg->start;
   }

   aspacem_assert(j == nSegs); /* this should not fail */
   return nSegs;
}

/* Apply permissions as given by PROT to the address space [addr:addr+len-1] */
void ML_(am_change_permissions)( Addr start, SizeT len, UInt prot )
{
   NSegment *seg, *segLo, *segHi;

   Bool newR = (prot & VKI_PROT_READ)  != 0;
   Bool newW = (prot & VKI_PROT_WRITE) != 0;
   Bool newX = (prot & VKI_PROT_EXEC)  != 0;

   split_segments_lo_and_hi( start, start + len - 1, &segLo, &segHi );

   for (seg = segLo; True; seg = ML_(am_next_segment)(seg)) {
      /* Apply the permissions to all relevant segments. */
      switch (seg->kind) {
         case SkAnonC: case SkAnonV: case SkFileC: case SkFileV: case SkShmC:
            seg->hasR = newR;
            seg->hasW = newW;
            seg->hasX = newX;
            break;
         default:
            break;
      }
      if (seg == segHi) break;
   }

   /* Changing permissions could have made previously un-mergable
      segments mergeable.  Therefore have to re-preen them. */
   //   preen_segments();
   if (expensive_checking) check_tree(root_segment());
}

/* Change ownership of valgrind address space [addr:addr+len-1] to client.
   The address range is supposed to be located in a single segment. If that
   is not so, return False. */
Bool ML_(am_clientise)( Addr start, SizeT len )
{
   const NSegment *seg = ML_(am_find_segment)(start);

   if (seg->kind != SkFileV && seg->kind != SkAnonV)
      return False;
   if (start + len - 1 > seg->end)
      return False;

   /* OK. SEG is a segment of the appropriate kind. Set up a template
      segment with the corresponding client kind and insert that. Function
      ML_(am_add_segment) will take care of everything. */
   NSegment tmp = *seg;

   switch (seg->kind) {
      case SkFileV: tmp.kind = SkFileC; break;
      case SkAnonV: tmp.kind = SkAnonC; break;
      default: aspacem_assert(0); /* can't happen - guarded above */
   }

   ML_(am_add_segment)(&tmp);

   return True;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
