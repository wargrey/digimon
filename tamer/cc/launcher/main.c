#include <stdio.h>

#include "../ffi/nested/seed.h"

/**************************************************************************************************/
int main(int argc, char* argv[]) {
    printf("the seed is %d\n", random_seed());

    return 0;
}

