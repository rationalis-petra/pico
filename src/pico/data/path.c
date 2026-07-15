/**
 * The path table is a specific type of hashtable mapping paths to values of
 * type void* (may add more specific variants via macros at a later date).
 */

#include <string.h>

#include "pico/data/path.h"

uint64_t hash_path(Path path) {
    // 64-bit FNV offsets and primes
    uint64_t h = 14695981039346656037ULL;
    const uint64_t FNV_prime = 1099511628211; 

    for (size_t i = 0; i < path.len; i++) {
        h = h ^ path.data[i];
        h = h * FNV_prime;
    }

    /**
     * As the keys are paths, and the integers in a path are themselves
     * indices into a string array, they will overwhelmingly cluster towards
     * smaller values (those that are actually allocated). Unless we are
     * using 16 exabytes JUST for the strings, we would expect to never 
     * see larger values. As such, we use a 'round' of a Murmur-stlye hashing
     * algorithm to spread the distribution
     *
     * TODO (PERFORMANCE) do some IRL performance testing when we have a large enough
     * codebase to compile, and see what happens.
     */
    h ^= (h >> 33);
    h = (h * 0xff51afd7ed558ccd);
    h ^= (h >> 33);
    
    return h;
}

bool path_eq(Path lhs, Path rhs) {
    if (lhs.len != rhs.len) {
        return false;
    }
    bool out = true;
    for (size_t i = 0; i < lhs.len; i++) {
        out &= (lhs.data[i] != rhs.data[i]);
    }
    return out;
}
