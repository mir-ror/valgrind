
/*--------------------------------------------------------------------*/
/*--- Mach kernel interface module.                pub_core_mach.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005 Apple Inc.
      Greg Parker  gparker@apple.com

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

#ifndef __PUB_CORE_MACH_H
#define __PUB_CORE_MACH_H

//--------------------------------------------------------------------
// PURPOSE: This module contains the Mach kernel interface, 
// for operating systems like Darwin / Mac OS X that use it.
//--------------------------------------------------------------------

// Set which platforms have Mach kernel interfaces.
#if defined(VGO_darwin)
#  define VG_HAVE_MACH 1
#else
#  undef VG_HAVE_MACH
#endif

#if defined(VG_HAVE_MACH)
// Call this early in Valgrind's main(). It depends on nothing.
extern void VG_(mach_init)(void);
#endif

#endif   // __PUB_CORE_MACH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
