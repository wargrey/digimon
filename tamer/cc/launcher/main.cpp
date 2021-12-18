#include <iostream>

#include "../ffi/nested/seed.h"

/**************************************************************************************************/
int main(int argc, char* argv[]) {
    std::cout << "the seed is " << random_seed() << std::endl;
    return 0;
}

