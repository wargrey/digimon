#include <stdio.h>

#include "../ffi/nested/version.h"

///////////////////////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[]) {
    printf("the standard C version is %ldL\n", stdc_version());

    return 0;
}
