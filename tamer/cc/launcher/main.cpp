#include <iostream>

#include "../ffi/nested/version.h"

/**************************************************************************************************/
int main(int argc, char* argv[]) {
    std::cout << "the standard C++ version is " << stdc_version() << "L" << std::endl;
    
    return 0;
}
