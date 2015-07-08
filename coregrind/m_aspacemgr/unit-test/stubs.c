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
#include "priv_aspacemgr.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/* Stub functions and stuff to get things to link */
Word VG_(clo_valgrind_stacksize) = 0;
Int VG_(clo_sanity_level) = 0;

void VG_(debugLog) ( Int level, const HChar* modul, const HChar *fmt, ... )
{
   va_list vargs;
   va_start(vargs, fmt);
   vprintf(fmt, vargs);
   va_end(vargs);
}

SizeT VG_(strlen)( const HChar *s )
{
   return strlen(s);
}

Int VG_(strcmp)( const HChar *s1, const HChar *s2 )
{
   return strcmp(s1, s2);
}

HChar *VG_(strcpy)( HChar *s1, const HChar *s2 )
{
   return strcpy(s1, s2);
}

void VG_(exit_now)( Int rc )
{
   exit(rc);
}

void *VG_(memset)( void *s, Int c, SizeT n )
{
   return memset(s, c, n);
}

HChar *VG_(strstr)( const HChar *s1, const HChar *s2 )
{
   return strstr(s1, s2);
}

void ML_(am_assert_fail)( const HChar *expr, const HChar *file, Int line, 
                          const HChar *fn )
{
   fprintf(stderr, "\"%s,%d\", ASSERT: %s\n", file, line, expr);
   exit(1);
}

void ML_(am_barf)( const HChar *what )
{
   fprintf(stderr, "BARF: %s\n", what);
   exit(1);
}

void ML_(am_barf_toolow)( const HChar *what )
{
   fprintf(stderr, "BARF_TOO_LOW: %s\n", what);
   exit(1);
}

void ML_(am_show_len_concisely)( /*OUT*/HChar *buf, Addr start, Addr end )
{
   const HChar *fmt;
   ULong len = ((ULong)end) - ((ULong)start) + 1;

   if (len < 10*1000*1000ULL) {
      fmt = "%7llu";
   } 
   else if (len < 999999ULL * (1ULL<<20)) {
      fmt = "%6llum";
      len >>= 20;
   }
   else if (len < 999999ULL * (1ULL<<30)) {
      fmt = "%6llug";
      len >>= 30;
   }
   else if (len < 999999ULL * (1ULL<<40)) {
      fmt = "%6llut";
      len >>= 40;
   }
   else {
      fmt = "%6llue";
      len >>= 50;
   }
   sprintf(buf, fmt, len);
}

/* These should never be called during testing */
SysRes VG_(do_syscall)( UWord sysno, UWord w1, UWord w2, UWord w3, 
                        UWord w4, UWord w5, UWord w6, 
                        UWord w7, UWord z8 )
{
   assert(0);
   return (SysRes){};
}

SysRes VG_(mk_SysRes_Error)( UWord val )
{
   assert(0);
   return (SysRes){};
}

SysRes VG_(mk_SysRes_Success)( UWord val )
{
   assert(0);
   return (SysRes){};
}

SysRes VG_(am_do_mmap_NO_NOTIFY)( Addr start, SizeT length, UInt prot, 
                                  UInt flags, Int fd, Off64T offset )
{
   assert(0);
   return (SysRes){};
}

SysRes ML_(am_do_munmap_NO_NOTIFY)( Addr start, SizeT length )
{
   assert(0);
   return (SysRes){};
}

SysRes ML_(am_do_extend_mapping_NO_NOTIFY)( Addr  old_addr, SizeT old_len,
                                            SizeT new_len )
{
   assert(0);
   return (SysRes){};
}

SysRes ML_(am_do_relocate_nooverlap_mapping_NO_NOTIFY)( Addr old_addr,
                                                        Addr old_len, 
                                                        Addr new_addr,
                                                        Addr new_len )
{
   assert(0);
   return (SysRes){};
}

SysRes ML_(am_open)( const HChar *pathname, Int flags, Int mode )
{
   assert(0);
   return (SysRes){};
}

Int ML_(am_read)( Int fd, void* buf, Int count )
{
   assert(0);
   return 1;
}

void ML_(am_close)( Int fd )
{
   assert(0);
}

void ML_(am_exit)( Int fd )
{
   assert(0);
}

UInt ML_(am_sprintf)( HChar *buf, const HChar *format, ... )
{
   assert(0);
}

Bool ML_(am_resolve_filename)( Int fd, /*OUT*/HChar *buf, Int nbuf )
{
   assert(0);
   return 1;
}

Bool ML_(am_get_fd_d_i_m)( Int fd, 
                           /*OUT*/ULong *dev, 
                           /*OUT*/ULong *ino, /*OUT*/UInt *mode )
{
   assert(0);
   return 1;
}

void ML_(am_write_dot)( const NSegment *seg, const HChar *file )
{
   write_dot(seg, NULL, NULL, file);
}

Bool
ML_(am_sane_segname)(Int ix)
{
   assert(0);
   return True;
}

Int
ML_(am_segname_get_seqnr)(Int fnIdx)
{
   return -1;
}

void
ML_(am_inc_refcount)(Int ix)
{
}

void
ML_(am_dec_refcount)(Int ix)
{
}

const HChar *
ML_(am_get_segname)(Int ix)
{
   return "*unknown*";
}

void
ML_(am_show_segnames)(Int logLevel, const HChar *prefix)
{
   assert(0);
}
