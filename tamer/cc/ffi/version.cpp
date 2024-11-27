
extern "C" {
    __ffi__ long stdc_version_plus() {
        return __cplusplus;
    }

    __ffi__ int conflict_datum() {
        return 2;
    }
}
