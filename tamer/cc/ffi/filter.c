#include "nested/version.h"

/////////////////////////////////////////////////////////////////////////////////////////////////// 
__ffi__ double stdc_version_filter() {
    return stdc_version() * 0.01;
}

__ffi__ int conflict_datum() {
    return 1;
}
