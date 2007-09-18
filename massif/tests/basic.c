#include <stdlib.h>

// Allocate some memory and then deallocate it, to get a nice up-then-down
// graph.

int main(void)
{
   // N=36 gives us 72 sample points, which fills the text graph nicely.
   #define N   36
   int i;
   int* a[N];

   for (i = 0; i < N; i++) {
      a[i] = malloc(100);
   }
   for (i = 0; i < N; i++) {
      free(a[i]);
   }
   
   return 0;
}
