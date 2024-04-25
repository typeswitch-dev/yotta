#include <stdio.h>
#include <sys/mman.h>

#define MK(n) printf("-D%s=%lld ", #n, (long long)n)
int main(void) {
    MK(PROT_EXEC);
    MK(PROT_READ);
    MK(PROT_WRITE);
    MK(MAP_ANON);
    #ifdef MAP_JIT
        MK(MAP_JIT);
    #endif
    MK(MAP_PRIVATE);
    MK(MAP_FAILED);
}
