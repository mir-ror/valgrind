#include <assert.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

// kernel brk() and libc brk() act quite differently...

int main(void)
{
   int i;
   void* orig_ds = sbrk(0);
   void* ds = orig_ds;
   struct {
     void *brkval;
     const char *what;
   } vals[] = {
     { (void *)0,  "like sbrk(0)" },
     { (void *)1,  "shrink to 0x1 --> expect underflow" },
     { ds - 0x1,   "shrink just below current brk value --> expect underflow" },
     { ds,         "brk to current brk value" },
     { ds + 0x1000,"grow by 0x1000" },
     { ds + 0x40000000, "excessive growth --> expect overflow" },
     { ds + 0x200, "shrink by 0x800" },
   };

   fprintf(stderr, "KERNEL __NR_brk\n");
   for (i = 0; i < sizeof vals / sizeof vals[0]; i++) {
      fprintf(stderr, "...%s\n", vals[i].what);
      void *res = (void*)syscall(__NR_brk, vals[i].brkval);
      fprintf(stderr, "res = %p\n", res);
   }

   fprintf(stderr, "LIBC brk\n");
   assert( 0 == brk(orig_ds) );  // libc brk()

   for (i = 0; i < sizeof vals / sizeof vals[0]; i++) {
      fprintf(stderr, "...%s\n", vals[i].what);
      int rc = brk(vals[i].brkval);
      fprintf(stderr, "rc = %d\n", rc);
   }

   return 0;
}
