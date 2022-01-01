#include <stdlib.h>
#include <time.h>

__lambda__ int random_seed() {
    srand((unsigned int)(time(NULL)) + clock());

    return rand() % 64;
}

