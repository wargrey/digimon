///////////////////////////////////////////////////////////////////////////////////////////////////
__ffi__ long stdc_version() {
#if defined(__STDC_VERSION__)
    return __STDC_VERSION__;
#elif defined(__cplusplus)
    return __cplusplus;
#else
    return 0L;
#endif
}
