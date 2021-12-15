#include <iostream>

export "C" {
    #include "../ffi/nested/seed.h"
}

/**************************************************************************************************/
int main(int argc, char** argv[]) {
    cout << "the seed is " << random_seed() << endl;
    return 0;
}

