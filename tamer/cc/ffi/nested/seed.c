#include <time.h>

int random_seed() {
    srand(time(NULL) + clock());

    return rand() % 64;
}

