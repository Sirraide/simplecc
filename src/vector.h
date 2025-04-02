#ifndef VECTOR_H
#define VECTOR_H

#include "platform.h"

#define NO_INDEX ((size_t) -1)

// ====================================================================
//  Dynamic Array
// ====================================================================
#define vec(type)        \
    struct {             \
        type *data;      \
        size_t size;     \
        size_t capacity; \
    }

/// Free the memory used by a vector, but not the vector
/// itself if it's on the heap. The vector is left in an
/// empty, but valid, state and can be reused.
#define vec_free(vector)         \
    ({                           \
        free((vector).data);     \
        (vector).data = nullptr; \
        (vector).size = 0;       \
        (vector).capacity = 0;   \
        ;                        \
    })

/// Iterate over all elements of a vector and then delete it.
#define vec_delete_els(element, vector)                                       \
    for (                                                                     \
        typeof(*(vector).data) *element = (vector).data;                      \
        element < (vector).data + (vector).size || (vec_free(vector), false); \
        element++                                                             \
    )

/// Create a copy of a vector. The data is copied via memcpy().
#define vec_copy(vector)                                                          \
    ({                                                                            \
        typeof_unqual(vector) _copy = (vector);                                   \
        _copy.data = malloc(_copy.size * sizeof *(vector).data);                  \
        memcpy(_copy.data, (vector).data, (vector).size * sizeof *(vector).data); \
        _copy;                                                                    \
    })

/// Iterate over a vector by reference.
#define vec_for(element, vector) \
    for (typeof(*(vector).data) *element = (vector).data; element < (vector).data + (vector).size; element++)

/// Iterate over a vector of pointers by value.
#define vec_for_val(element, vector)                                                                              \
    for (typeof_unqual(*(vector).data) *element##_ptr = (typeof_unqual(*(vector).data) *) (vector).data, element; \
         element##_ptr < (vector).data + (vector).size && (element = *element##_ptr, true); /* "=", not "=="! */  \
         element##_ptr++)

/// Iterate over each index and element of a vector.
#define vec_for_index(index, vector) \
    for (size_t index = 0; index < (vector).size; index++)

/// Ensure that there is space for at least (vector->size + elements)
/// many elements. New empty elements are zero-initialised.
#define vec_reserve(vector, elements)                                                                                  \
    do {                                                                                                               \
        if ((vector).capacity < (vector).size + (elements)) {                                                          \
            (vector).capacity += (elements);                                                                           \
            (vector).capacity *= 2;                                                                                    \
            if (!(vector).data) {                                                                                      \
                (vector).data = calloc((vector).capacity, sizeof *(vector).data);                                      \
            } else {                                                                                                   \
                (vector).data = realloc((vector).data, (vector).capacity * sizeof *(vector).data);                     \
                memset((vector).data + (vector).size, 0, ((vector).capacity - (vector).size) * sizeof *(vector).data); \
            }                                                                                                          \
        }                                                                                                              \
    } while (0)

/// Resize the vector to the given size.
#define vec_resize(vector, sz)                          \
    do {                                                \
        size_t _sz = sz;                                \
        if (_sz > (vector).size)                        \
            vec_reserve((vector), _sz - (vector).size); \
        (vector).size = _sz;                            \
    } while (0)

/// Push an element onto the vector.
#define vec_push(vector, ...)                           \
    do {                                                \
        vec_reserve((vector), 1);                       \
        (vector).data[(vector).size++] = (__VA_ARGS__); \
    } while (0)

/// Push an element onto a vector if it's not already in the vector.
#define vec_push_unique(vector, element)    \
    do {                                    \
        if (!vec_contains(vector, element)) \
            vec_push(vector, element);      \
    } while (0)

/// Pop an element from the vector.
#define vec_pop(vector) ((vector).data[--(vector).size])

/// Pop an element from the front of the vector.
#define vec_pop_front(vector) ({   \
    auto _val = vec_front(vector); \
    vec_remove_index(vector, 0);   \
    _val;                          \
})

/// Remove all elements from a vector.
#define vec_clear(vector) ((void) ((vector).size = 0))

/// Get the last element of a vector.
#define vec_back_or(vector, default) ((vector).size ? vec_back(vector) : (default))
#define vec_back(vector)             ((vector).data[(vector).size - 1])

/// Get the first element of a vector.
#define vec_front_or(vector, default) ((vector).size ? vec_front(vector) : (default))
#define vec_front(vector)             ((vector).data[0])

/// Remove an element from a vector by index. This may change the order of elements in the vector.
#define vec_remove_index_unordered(vector, index) ((void) ((vector).data[index] = (vector).data[--(vector).size]))

/// Remove an element from a vector. This may change the order of elements in the vector.
#define vec_remove_element_unordered(vector, element)                                        \
    do {                                                                                     \
        size_t _index = 0;                                                                   \
        for (; _index < (vector).size; _index++) {                                           \
            if (memcmp((vector).data + _index, &(element), sizeof(element)) == 0) { break; } \
        }                                                                                    \
        if (_index < (vector).size) vec_remove_index_unordered(vector, _index);              \
    } while (0)

/// Remove an element from a vector by index.
#define vec_remove_index(vector, index)                                                                                           \
    do {                                                                                                                          \
        if (index < (vector).size) {                                                                                              \
            memmove((vector).data + (index), (vector).data + (index) + 1, ((vector).size - (index) - 1) * sizeof *(vector).data); \
            (vector).size--;                                                                                                      \
        }                                                                                                                         \
    } while (0)

/// Append a vector to another vector
#define vec_append(to, ...)                                                                                 \
    do {                                                                                                    \
        vec_reserve((to), (__VA_ARGS__).size);                                                              \
        memcpy((to).data + (to).size, (__VA_ARGS__).data, (__VA_ARGS__).size * sizeof *(__VA_ARGS__).data); \
        (to).size += (__VA_ARGS__).size;                                                                    \
    } while (0)

/// Check if a vector contains an element.
#define vec_contains(vector, element) ({                 \
    bool _found = false;                                 \
    vec_for(_el, (vector)) {                             \
        if (memcmp(_el, &(element), sizeof *_el) == 0) { \
            _found = true;                               \
            break;                                       \
        }                                                \
    }                                                    \
    _found;                                              \
})

/// Find an element in a vector by predicate. Returns
/// a the index of the element or -1 if not found.
#define vec_find_if_index(element, vector, ...) ({ \
    size_t _idx = NO_INDEX;                        \
    vec_for_index(_i, (vector)) {                  \
        auto element = (vector).data + _i;         \
        if (__VA_ARGS__) {                         \
            _idx = _i;                             \
            break;                                 \
        }                                          \
    }                                              \
    _idx;                                          \
})

/// Find an element in a vector by predicate. Returns
/// a pointer to the element or nullptr if not found.
#define vec_find_if(element, vector, ...) ({                        \
    size_t _idx_ = vec_find_if_index(element, vector, __VA_ARGS__); \
    _idx_ == NO_INDEX ? nullptr : (vector).data + _idx_;            \
})

/// Remove all elements from a vector that match the condition. Additionally,
/// the predicate should take care of freeing any elements for which it returns
/// true as well.
#define vec_erase_if(element, vector, ...)                                       \
    do {                                                                         \
        size_t _first = vec_find_if_index(element, vector, __VA_ARGS__);         \
        if (_first != NO_INDEX) {                                                \
            for (size_t _i = _first; _i < (vector).size; _i++) {                 \
                auto element = (vector).data + _i;                               \
                if (!(__VA_ARGS__)) (vector).data[_first++] = (vector).data[_i]; \
            }                                                                    \
            vec_resize(vector, _first);                                          \
        }                                                                        \
    } while (0)

// ====================================================================
//  Span/String
// ====================================================================
typedef struct span {
    const char *data;
    u64 size;
} span;

typedef vec(char) string;

#define as_span(x)    ((span) {.data = x.data, .size = x.size})
#define lit_string(x) ((string) {.data = strdup(x), .size = strlen(x), .capacity = strlen(x) + 1})
#define lit_span(x)   ((span) {.data = x, .size = sizeof(x) - 1})

#define str_copy(s)        vec_copy(s)
#define str_cat(s1, ...)   vec_append(s1, __VA_ARGS__)
#define str_cat_lit(s, l)  vec_append(s, lit_span(l))
#define str_cat_char(s, c) vec_push(s, c)

#define eq(_s1, _s2) ({                                          \
    auto s1 = (_s1);                                             \
    auto s2 = (_s2);                                             \
    s1.size == s2.size&& memcmp(s1.data, s2.data, s1.size) == 0; \
})

#endif // VECTOR_H
