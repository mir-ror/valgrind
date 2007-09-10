
/*--------------------------------------------------------------------*/
/*--- pthread intercepts for thread checking.                      ---*/
/*---                                              tc_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Thrcheck, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2007 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* RUNS ON SIMULATED CPU
   Interceptors for pthread_* functions, so that tc_main can see
   significant thread events. 
*/

#include "pub_tool_basics.h"
#include "valgrind.h"
#include "thrcheck.h"

#define TRACE_PTH_FNS 0


/*----------------------------------------------------------------*/
/*---                                                          ---*/
/*----------------------------------------------------------------*/

#define PTH_FUNC(ret_ty, f, args...) \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(libpthreadZdsoZd0,f)(args); \
   ret_ty I_WRAP_SONAME_FNNAME_ZZ(libpthreadZdsoZd0,f)(args)

// Do a client request.  This is a macro rather than a function 
// so as to avoid having an extra function in the stack trace.

#define DO_CREQ_v_W(_creqF, _ty1F,_arg1F)                \
   do {                                                  \
      Word _unused_res, _arg1;                           \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      VALGRIND_DO_CLIENT_REQUEST(_unused_res, 0,         \
                                 (_creqF),               \
                                 _arg1, 0,0,0,0);        \
   } while (0)

#define DO_CREQ_v_WW(_creqF, _ty1F,_arg1F, _ty2F,_arg2F) \
   do {                                                  \
      Word _unused_res, _arg1, _arg2;                    \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      assert(sizeof(_ty2F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      _arg2 = (Word)(_arg2F);                            \
      VALGRIND_DO_CLIENT_REQUEST(_unused_res, 0,         \
                                 (_creqF),               \
                                 _arg1,_arg2,0,0,0);     \
   } while (0)

#define DO_CREQ_v_WWW(_creqF, _ty1F,_arg1F,              \
		      _ty2F,_arg2F, _ty3F, _arg3F)       \
   do {                                                  \
      Word _unused_res, _arg1, _arg2, _arg3;             \
      assert(sizeof(_ty1F) == sizeof(Word));             \
      assert(sizeof(_ty2F) == sizeof(Word));             \
      assert(sizeof(_ty3F) == sizeof(Word));             \
      _arg1 = (Word)(_arg1F);                            \
      _arg2 = (Word)(_arg2F);                            \
      _arg3 = (Word)(_arg3F);                            \
      VALGRIND_DO_CLIENT_REQUEST(_unused_res, 0,         \
                                 (_creqF),               \
                                 _arg1,_arg2,_arg3,0,0); \
   } while (0)


#define DO_PthAPIerror(_fnnameF, _errF)                  \
   do {                                                  \
      char* _fnname = (char*)(_fnnameF);                 \
      long  _err    = (long)(int)(_errF);	         \
      char* _errstr = lame_strerror(_err);               \
      DO_CREQ_v_WWW(_VG_USERREQ__TC_PTH_API_ERROR,       \
                    char*,_fnname,                       \
                    long,_err, char*,_errstr);           \
   } while (0)

#include <stdio.h>
#include <assert.h>
#include <errno.h>

#define __USE_UNIX98 1
#include <pthread.h>


/* A lame version of strerror which doesn't use the real libc
   strerror_r, since using the latter just generates endless more
   threading errors (glibc goes off and does tons of crap w.r.t.
   locales etc) */
static char* lame_strerror ( long err )
{
   switch (err) {
      case EPERM:       return "EPERM: Operation not permitted";
      case ENOENT:      return "ENOENT: No such file or directory";
      case ESRCH:       return "ESRCH: No such process";
      case EINTR:       return "EINTR: Interrupted system call";
      case EBADF:       return "EBADF: Bad file number";
      case EAGAIN:      return "EAGAIN: Try again";
      case ENOMEM:      return "ENOMEM: Out of memory";
      case EACCES:      return "EACCES: Permission denied";
      case EFAULT:      return "EFAULT: Bad address";
      case EEXIST:      return "EEXIST: File exists";
      case EINVAL:      return "EINVAL: Invalid argument";
      case EMFILE:      return "EMFILE: Too many open files";
      case ENOSYS:      return "ENOSYS: Function not implemented";
      case EOVERFLOW:   return "EOVERFLOW: Value too large "
                               "for defined data type";
      case EBUSY:       return "EBUSY: Device or resource busy";
      default:          return "tc_intercepts.c: lame_strerror(): "
                               "unhandled case -- please fix me!";
   }
}


/*----------------------------------------------------------------*/
/*--- pthread_create, pthread_join, pthread_exit               ---*/
/*----------------------------------------------------------------*/

static void* mythread_wrapper ( void* xargsV )
{
   volatile Word volatile* xargs = (volatile Word volatile*) xargsV;
   void*(*fn)(void*) = (void*(*)(void*))xargs[0];
   void* arg         = (void*)xargs[1];
   pthread_t me = pthread_self();
   /* Tell the tool what my pthread_t is. */
   DO_CREQ_v_W(_VG_USERREQ__TC_SET_MY_PTHREAD_T, pthread_t,me);
   /* allow the parent to proceed.  We can't let it proceed until
      we're ready because (1) we need to make sure it doesn't exit and
      hence deallocate xargs[] while we still need it, and (2) we
      don't want either parent nor child to proceed until the tool has
      been notified of the child's pthread_t. */
   xargs[2] = 0;
   /* Now we can no longer safely use xargs[]. */
   return (void*) fn( (void*)arg );
}

// pthread_create
PTH_FUNC(int, pthreadZucreateZAZa, // pthread_create@*
              pthread_t *thread, const pthread_attr_t *attr,
              void *(*start) (void *), void *arg)
{
   int    ret;
   OrigFn fn;
   volatile Word xargs[3];

   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_create wrapper"); fflush(stderr);
   }
   xargs[0] = (Word)start;
   xargs[1] = (Word)arg;
   xargs[2] = 1; /* serves as a spinlock -- sigh */

   CALL_FN_W_WWWW(ret, fn, thread,attr,mythread_wrapper,&xargs[0]);

   if (ret == 0) {
      /* we have to wait for the child to notify the tool of its
         pthread_t before continuing */
      while (xargs[2] != 0) {
         // FIXME: add a yield client request
         /* do nothing */
      }
   } else { 
      DO_PthAPIerror( "pthread_create", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: pth_create -> %d >>\n", ret);
   }
   return ret;
}

// pthread_join
PTH_FUNC(int, pthreadZujoin, // pthread_join
              pthread_t thread, void** value_pointer)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_join wrapper"); fflush(stderr);
   }

   CALL_FN_W_WW(ret, fn, thread,value_pointer);

   /* At least with NPTL as the thread library, this is safe because
      it is guaranteed (by NPTL) that the joiner will completely gone
      before pthread_join (the original) returns.  See email below.*/
   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_JOIN_POST, pthread_t,thread);
   } else { 
      DO_PthAPIerror( "pthread_join", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: pth_join -> %d >>\n", ret);
   }
   return ret;
}

/* Behaviour of pthread_join on NPTL:

Me:
I have a question re the NPTL pthread_join implementation.

  Suppose I am the thread 'stayer'.  

  If I call pthread_join(quitter), is it guaranteed that the
  thread 'quitter' has really exited before pthread_join returns?

  IOW, is it guaranteed that 'quitter' will not execute any further
  instructions after pthread_join returns?

I believe this is true based on the following analysis of
glibc-2.5 sources.  However am not 100% sure and would appreciate
confirmation.

  'quitter' will be running start_thread() in nptl/pthread_create.c

  The last action of start_thread() is to exit via
  __exit_thread_inline(0), which simply does sys_exit 
  (nptl/pthread_create.c:403)

  'stayer' meanwhile is waiting for lll_wait_tid (pd->tid) 
  (call at nptl/pthread_join.c:89)

  As per comment at nptl/sysdeps/unix/sysv/linux/i386/lowlevellock.h:536,
  lll_wait_tid will not return until kernel notifies via futex
  wakeup that 'quitter' has terminated.

  Hence pthread_join cannot return until 'quitter' really has
  completely disappeared.

Drepper:
>   As per comment at nptl/sysdeps/unix/sysv/linux/i386/lowlevellock.h:536,
>   lll_wait_tid will not return until kernel notifies via futex
>   wakeup that 'quitter' has terminated.
That's the key.  The kernel resets the TID field after the thread is
done.  No way the joiner can return before the thread is gone.
*/


/*----------------------------------------------------------------*/
/*--- pthread_mutex_lock, pthread_mutex_unlock, et al          ---*/
/*----------------------------------------------------------------*/

// pthread_mutex_init
PTH_FUNC(int, pthreadZumutexZuinit, // pthread_mutex_init
              pthread_mutex_t *mutex,
              pthread_mutexattr_t* attr)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxinit %p", mutex); fflush(stderr);
   }
#if 0
if (attr) {
int ty, zzz;
zzz = pthread_mutexattr_gettype(attr, &ty);
if (zzz == 0 && ty == PTHREAD_MUTEX_RECURSIVE)
   fprintf(stderr, "ZZZZZ recursive!\n");
}
#endif
   CALL_FN_W_WW(ret, fn, mutex,attr);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__tc_PTHREAD_MUTEX_INIT_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      DO_PthAPIerror( "pthread_mutex_init", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxinit -> %d >>\n", ret);
   }
   return ret;
}


// pthread_mutex_destroy
PTH_FUNC(int, pthreadZumutexZudestroy, // pthread_mutex_destroy
              pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxdestroy %p", mutex); fflush(stderr);
   }

   CALL_FN_W_W(ret, fn, mutex);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_DESTROY_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      DO_PthAPIerror( "pthread_mutex_destroy", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxdestroy -> %d >>\n", ret);
   }
   return ret;
}


// pthread_mutex_lock
PTH_FUNC(int, pthreadZumutexZulock, // pthread_mutex_lock
              pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxlock %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__tc_PTHREAD_MUTEX_LOCK_PRE,
               pthread_mutex_t*,mutex);

   CALL_FN_W_W(ret, fn, mutex);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_LOCK_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      DO_PthAPIerror( "pthread_mutex_lock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxlock -> %d >>\n", ret);
   }
   return ret;
}


// pthread_mutex_trylock.  AFAICS the handling needed here is identical
// to that for pthread_mutex_lock.
PTH_FUNC(int, pthreadZumutexZutrylock, // pthread_mutex_trylock
              pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxtrylock %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__tc_PTHREAD_MUTEX_LOCK_PRE,
               pthread_mutex_t*,mutex);

   CALL_FN_W_W(ret, fn, mutex);

   /* There's a hole here: libpthread now knows the lock is locked,
      but the tool doesn't, so some other thread could run and detect
      that the lock has been acquired by someone (this thread).  Does
      this matter?  Not sure, but I don't think so. */

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_LOCK_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      if (ret != EBUSY)
         DO_PthAPIerror( "pthread_mutex_trylock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " :: mxtrylock -> %d >>\n", ret);
   }
   return ret;
}


// pthread_mutex_unlock
PTH_FUNC(int, pthreadZumutexZuunlock, // pthread_mutex_unlock
              pthread_mutex_t *mutex)
{
   int    ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_mxunlk %p", mutex); fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_UNLOCK_PRE,
               pthread_mutex_t*,mutex);

   CALL_FN_W_W(ret, fn, mutex);

   if (ret == 0 /*success*/) {
      DO_CREQ_v_W(_VG_USERREQ__tc_PTHREAD_MUTEX_UNLOCK_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      DO_PthAPIerror( "pthread_mutex_unlock", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " mxunlk -> %d >>\n", ret);
   }
   return ret;
}


/*----------------------------------------------------------------*/
/*--- pthread_cond_wait, pthread_cond_signal, et al            ---*/
/*----------------------------------------------------------------*/

// pthread_cond_wait
PTH_FUNC(int, pthreadZucondZuwaitZAZa, // pthread_cond_wait@*
              pthread_cond_t* cond, pthread_mutex_t* mutex)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_wait %p %p", cond, mutex);
      fflush(stderr);
   }

   /* Tell the tool we're about to drop the mutex.  This reflects the
      fact that in a cond_wait, we show up holding the mutex, and the
      call atomically drops the mutex and waits for the cv to be
      signalled. */
   DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_UNLOCK_PRE,
               pthread_mutex_t*,mutex);

   CALL_FN_W_WW(ret, fn, cond,mutex);

   if (ret == 0) {
      DO_CREQ_v_WW(_VG_USERREQ__TC_PTHREAD_COND_WAIT_POST,
                   pthread_cond_t*,cond, pthread_mutex_t*,mutex);

      /* And now we have the mutex again. */
      DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_LOCK_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      DO_PthAPIerror( "pthread_cond_wait", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cowait -> %d >>\n", ret);
   }

   return ret;
}


// pthread_cond_timedwait
PTH_FUNC(int, pthreadZucondZutimedwaitZAZa, // pthread_cond_timedwait@*
         pthread_cond_t* cond, pthread_mutex_t* mutex, 
         struct timespec* abstime)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_timedwait %p %p %p", 
                      cond, mutex, abstime);
      fflush(stderr);
   }

   /* Tell the tool we're about to drop the mutex.  This reflects the
      fact that in a cond_wait, we show up holding the mutex, and the
      call atomically drops the mutex and waits for the cv to be
      signalled. */
   DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_UNLOCK_PRE,
               pthread_mutex_t*,mutex);

   CALL_FN_W_WWW(ret, fn, cond,mutex,abstime);

   if (ret == 0) {
      DO_CREQ_v_WW(_VG_USERREQ__TC_PTHREAD_COND_WAIT_POST,
                   pthread_cond_t*,cond, pthread_mutex_t*,mutex);

      /* And now we have the mutex again. */
      DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_MUTEX_LOCK_POST,
                  pthread_mutex_t*,mutex);
   } else { 
      DO_PthAPIerror( "pthread_cond_timedwait", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cotimedwait -> %d >>\n", ret);
   }

   return ret;
}


// pthread_cond_signal
PTH_FUNC(int, pthreadZucondZusignalZAZa, // pthread_cond_signal@*
              pthread_cond_t* cond)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_cond_signal %p", cond);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_COND_SIGNAL_PRE,
               pthread_cond_t*,cond);

   CALL_FN_W_W(ret, fn, cond);

   if (ret != 0) {
      DO_PthAPIerror( "pthread_cond_signal", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cosig -> %d >>\n", ret);
   }

   return ret;
}


// pthread_cond_broadcast
// Note, this is pretty much identical, from a dependency-graph
// point of view, with cond_signal, so the code is duplicated.
// Maybe it should be commoned up.
PTH_FUNC(int, pthreadZucondZubroadcastZAZa, // pthread_cond_broadcast@*
              pthread_cond_t* cond)
{
   int ret;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);

   if (TRACE_PTH_FNS) {
      fprintf(stderr, "<< pthread_broadcast_signal %p", cond);
      fflush(stderr);
   }

   DO_CREQ_v_W(_VG_USERREQ__TC_PTHREAD_COND_BROADCAST_PRE,
               pthread_cond_t*,cond);

   CALL_FN_W_W(ret, fn, cond);

   if (ret != 0) { 
      DO_PthAPIerror( "pthread_cond_broadcast", ret );
   }

   if (TRACE_PTH_FNS) {
      fprintf(stderr, " cobro -> %d >>\n", ret);
   }

   return ret;
}


/*--------------------------------------------------------------------*/
/*--- end                                          tc_intercepts.c ---*/
/*--------------------------------------------------------------------*/
