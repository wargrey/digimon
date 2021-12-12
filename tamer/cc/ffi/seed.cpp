#include <random>

extern "C" {
    int random_seed_plus() {
        std::random_device r;
        std::default_random_engine el(r());
        std::uniform_int_distribution<int> uniform_dist(64, 128);
    
        return uniform_dist(el);
    }
}

