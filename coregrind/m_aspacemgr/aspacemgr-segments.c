/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Address space manager segments                               ---*/
/*---                                         aspacemgr-segments.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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
/* Sorted by .addr field. */
/* I: len may not be zero. */
/* I: overlapping segments are not allowed. */
/* I: the segments cover the entire address space precisely. */
/* Each segment can optionally hold an index into the filename table. */

NSegment nsegments[VG_N_SEGMENTS];
Int      nsegments_used = 0;


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

/* Print out the segments (debugging only!). */
void VG_(am_show_nsegments) ( Int logLevel, const HChar *who )
{
   Int i;
   VG_(debugLog)(logLevel, "aspacem",
                 "<<< SHOW_SEGMENTS: %s (%d segments)\n", 
                 who, nsegments_used);
   ML_(am_show_segnames)( logLevel, who );
   for (i = 0; i < nsegments_used; i++)
      show_segment( logLevel, i, &nsegments[i] );
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

/* Binary search the interval array for a given address.  Since the
   array covers the entire address space the search cannot fail.  The
   _WRK function does the real work.  Its caller (just below) caches
   the results thereof, to save time.  With N_CACHE of 63 we get a hit
   rate exceeding 90% when running OpenOffice.

   Re ">> 12", it doesn't matter that the page size of some targets
   might be different from 12.  Really "(a >> 12) % N_CACHE" is merely
   a hash function, and the actual cache entry is always validated
   correctly against the selected cache entry before use.
*/
/* Don't call find_segment_idx_WRK; use find_segment_idx instead. */
__attribute__((noinline))
static UInt find_segment_idx_WRK( Addr a )
{
   Addr a_mid_lo, a_mid_hi;
   Int  mid,
        lo = 0,
        hi = nsegments_used-1;
   while (True) {
      /* current unsearched space is from lo to hi, inclusive. */
      if (lo > hi) {
         /* Not found.  This can't happen. */
         ML_(am_barf)("find_segment_idx: not found");
      }
      mid      = (lo + hi) / 2;
      a_mid_lo = nsegments[mid].start;
      a_mid_hi = nsegments[mid].end;

      if (a < a_mid_lo) { hi = mid-1; continue; }
      if (a > a_mid_hi) { lo = mid+1; continue; }
      aspacem_assert(a >= a_mid_lo && a <= a_mid_hi);
      aspacem_assert(0 <= mid && mid < nsegments_used);
      return mid;
   }
}

inline static UInt find_segment_idx( Addr a )
{
#  define N_CACHE 131 /*prime*/
   static Addr cache_pageno[N_CACHE];
   static Int  cache_segidx[N_CACHE];
   static Bool cache_inited = False;

   static UWord n_q = 0;
   static UWord n_m = 0;

   UWord ix;

   if (LIKELY(cache_inited)) {
      /* do nothing */
   } else {
      for (ix = 0; ix < N_CACHE; ix++) {
         cache_pageno[ix] = 0;
         cache_segidx[ix] = -1;
      }
      cache_inited = True;
   }

   ix = (a >> 12) % N_CACHE;

   n_q++;
   if (0 && 0 == (n_q & 0xFFFF))
      VG_(debugLog)(0,"xxx","find_segment_idx: %lu %lu\n", n_q, n_m);

   if ((a >> 12) == cache_pageno[ix]
       && cache_segidx[ix] >= 0
       && cache_segidx[ix] < nsegments_used
       && nsegments[cache_segidx[ix]].start <= a
       && a <= nsegments[cache_segidx[ix]].end) {
      /* hit */
      /* aspacem_assert( cache_segidx[ix] == find_segment_idx_WRK(a) ); */
      return cache_segidx[ix];
   } else {
      /* miss */
      n_m++;
      cache_segidx[ix] = find_segment_idx_WRK(a);
      cache_pageno[ix] = a >> 12;
      return cache_segidx[ix];
   }
#  undef N_CACHE
}

NSegment *ML_(am_find_segment)( Addr a )
{
   return nsegments + find_segment_idx(a);
}

/* Finds the segment containing 'a'.  Only returns non-SkFree segments. */
const NSegment *VG_(am_find_nsegment)( Addr a )
{
   const NSegment *seg = ML_(am_find_segment)(a);

   aspacem_assert(seg->start <= a);
   aspacem_assert(a <= seg->end);

   return seg->kind == SkFree ? NULL : seg;
}

/* Find the next segment along from 'here', if it is a non-SkFree segment. */
const NSegment *ML_(am_next_segment)( const NSegment *here, Bool fwds )
{
   const NSegment *next;

   if (fwds) {
      next = here + 1;
      if (next >= nsegments + nsegments_used)
         return NULL;
   } else {
      if (here == nsegments)
         return NULL;
      next = here - 1;
   }
   return (next->kind == SkFree) ? NULL : next;
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
}


/*-----------------------------------------------------------------*/
/*---                                                           ---*/
/*--- Modifying the segment structure.                          ---*/
/*---                                                           ---*/
/*-----------------------------------------------------------------*/

/* Split the segment containing 'a' into two, so that 'a' is
   guaranteed to be the start of a new segment.  If 'a' is already the
   start of a segment, do nothing. */
static void split_segment_at( Addr a )
{
   UInt i, j;

   aspacem_assert(a > Addr_MIN);
   aspacem_assert(VG_IS_PAGE_ALIGNED(a));
   aspacem_assert(nsegments_used > 0);
 
   i = find_segment_idx(a);

   if (nsegments[i].start == a)
      /* 'a' is already the start point of a segment, so nothing to be
         done. */
      return;

   /* else we have to slide the segments upwards to make a hole */
   if (nsegments_used >= VG_N_SEGMENTS)
      ML_(am_barf_toolow)("VG_N_SEGMENTS");
   for (j = nsegments_used-1; j > i; j--)
      nsegments[j+1] = nsegments[j];
   nsegments_used++;

   nsegments[i+1]       = nsegments[i];
   nsegments[i+1].start = a;
   nsegments[i].end     = a-1;

   if (nsegments[i].kind == SkFileV || nsegments[i].kind == SkFileC)
      nsegments[i+1].offset 
         += ((ULong)nsegments[i+1].start) - ((ULong)nsegments[i].start);

   ML_(am_inc_refcount)(nsegments[i].fnIdx);

   aspacem_assert(sane_segment(&nsegments[i]));
   aspacem_assert(sane_segment(&nsegments[i+1]));
}


/* Do the minimum amount of segment splitting necessary to ensure that
   sLo is the first address denoted by some segment and sHi is the
   highest address denoted by some other segment.  Returns the indices
   of the lowest and highest segments in the range. */

static void split_segments_lo_and_hi( Addr sLo, Addr sHi,
                                      /*OUT*/UInt *iLo,
                                      /*OUT*/UInt *iHi )
{
   aspacem_assert(sLo < sHi);
   aspacem_assert(VG_IS_PAGE_ALIGNED(sLo));
   aspacem_assert(VG_IS_PAGE_ALIGNED(sHi+1));

   if (sLo > Addr_MIN)
      split_segment_at(sLo);
   if (sHi < Addr_MAX)
      split_segment_at(sHi + 1);

   *iLo = find_segment_idx(sLo);
   *iHi = find_segment_idx(sHi);

   aspacem_assert(*iLo <= *iHi);
   aspacem_assert(nsegments[*iLo].start == sLo);
   aspacem_assert(nsegments[*iHi].end == sHi);
   /* Not that I'm overly paranoid or anything, definitely not :-) */
}


/* Add SEG to the collection, deleting/truncating any it overlaps.
   This deals with all the tricky cases of splitting up segments as
   needed. */

void ML_(am_add_segment)( const NSegment *seg )
{
   UInt i, iLo, iHi, delta;
   Bool segment_is_sane;

   Addr sStart = seg->start;
   Addr sEnd   = seg->end;

   aspacem_assert(sStart <= sEnd);
   aspacem_assert(VG_IS_PAGE_ALIGNED(sStart));
   aspacem_assert(VG_IS_PAGE_ALIGNED(sEnd+1));

   segment_is_sane = sane_segment(seg);
   if (!segment_is_sane) ML_(am_show_segment_full)(0, -1, seg);
   aspacem_assert(segment_is_sane);

   split_segments_lo_and_hi( sStart, sEnd, &iLo, &iHi );

   /* Now iLo .. iHi inclusive is the range of segment indices which
      seg will replace.  If we're replacing more than one segment,
      slide those above the range down to fill the hole. Before doing
      that decrement the reference counters for the segments names of
      the replaced segments. */
   for (i = iLo; i <= iHi; ++i)
      ML_(am_dec_refcount)(nsegments[i].fnIdx);
   delta = iHi - iLo;

   if (delta > 0) {
      for (i = iLo; i < nsegments_used-delta; i++)
         nsegments[i] = nsegments[i+delta];
      nsegments_used -= delta;
   }

   nsegments[iLo] = *seg;

   preen_segments();
   if (0) VG_(am_show_nsegments)(0,"AFTER preen (add_segment)");
}

void
ML_(am_segments_init)( void )
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

/* Return the total amount of space in anonymous mappings,
   both for V and the client.  Is used for printing stats in
   out-of-memory messages. */
ULong VG_(am_get_anonsize_total)( void )
{
   Int   i;
   ULong total = 0;

   for (i = 0; i < nsegments_used; i++) {
      if (nsegments[i].kind == SkAnonC || nsegments[i].kind == SkAnonV) {
         total += (ULong)nsegments[i].end 
                  - (ULong)nsegments[i].start + 1ULL;
      }
   }
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
   for (i = 0; i < nsegments_used; i++) {
      if ((nsegments[i].kind & kind_mask) != 0)
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
   for (i = 0; i < nsegments_used; i++) {
      if ((nsegments[i].kind & kind_mask) != 0)
         starts[j++] = nsegments[i].start;
   }

   aspacem_assert(j == nSegs); /* this should not fail */
   return nSegs;
}

/* Apply permissions as given by PROT to the address space [addr:addr+len-1] */
void ML_(am_change_permissions)( Addr start, SizeT len, UInt prot )
{
   UInt i, iLo, iHi;

   Bool newR = (prot & VKI_PROT_READ)  != 0;
   Bool newW = (prot & VKI_PROT_WRITE) != 0;
   Bool newX = (prot & VKI_PROT_EXEC)  != 0;

   split_segments_lo_and_hi( start, start + len - 1, &iLo, &iHi );

   for (i = iLo; i <= iHi; i++) {
      /* Apply the permissions to all relevant segments. */
      switch (nsegments[i].kind) {
         case SkAnonC: case SkAnonV: case SkFileC: case SkFileV: case SkShmC:
            nsegments[i].hasR = newR;
            nsegments[i].hasW = newW;
            nsegments[i].hasX = newX;
            break;
         default:
            break;
      }
   }

   /* Changing permissions could have made previously un-mergable
      segments mergeable.  Therefore have to re-preen them. */
   preen_segments();
}

/* Change ownership of valgrind address space [addr:addr+len-1] to client. */
void ML_(am_clientise)( Addr start, SizeT len )
{
   UInt iLo, iHi;

   split_segments_lo_and_hi( start, start + len - 1, &iLo, &iHi );
   aspacem_assert(iLo == iHi);

   switch (nsegments[iLo].kind) {
      case SkFileV: nsegments[iLo].kind = SkFileC; break;
      case SkAnonV: nsegments[iLo].kind = SkAnonC; break;
      default: aspacem_assert(0); /* can't happen - guarded above */
   }

   /* Changing permissions could have made previously un-mergable
      segments mergeable.  Therefore have to re-preen them. */
   preen_segments();
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
