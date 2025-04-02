// ====================================================================
//  Constants
// ====================================================================
#define ONE_MEGABYTE (1'024 * 1'024 * 1'024)
#define NO_INDEX    ((size_t) -1)
#define noreturn     __attribute__((__noreturn__)) void
#define nodiscard    __attribute__((__warn_unused_result__))

#define ALL_KEYWORDS(kw) \
    kw(__attribute__)      \
    kw(bool)               \
    kw(break)              \
    kw(case)               \
    kw(char)               \
    kw(const)              \
    kw(continue)           \
    kw(default)            \
    kw(do)                 \
    kw(else)               \
    kw(enum)               \
    kw(false)              \
    kw(for)                \
    kw(if)                 \
    kw(int)                \
    kw(long)               \
    kw(nullptr)            \
    kw(restrict)           \
    kw(return)             \
    kw(short)              \
    kw(signed)             \
    kw(sizeof)             \
    kw(static)             \
    kw(struct)             \
    kw(switch)             \
    kw(true)               \
    kw(typedef)            \
    kw(typeof)             \
    kw(unsigned)           \
    kw(void)               \
    kw(while)

#define assert(x)                                \
    do {                                         \
        if (!(x)) die("assertion failed : " #x); \
    } while (false)

// ====================================================================
//  Dynamic Arrays
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
        typeof(vector) _copy = (vector);                                          \
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
#define vec_append(to, from)                                                           \
    do {                                                                               \
        vec_reserve((to), (from).size);                                                \
        memcpy((to).data + (to).size, (from).data, (from).size * sizeof *(from).data); \
        (to).size += (from).size;                                                      \
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
    size_t _idx = NO_INDEX;                       \
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
    _idx_ == NO_INDEX ? nullptr : (vector).data + _idx_;           \
})

/// Remove all elements from a vector that match the condition. Additionally,
/// the predicate should take care of freeing any elements for which it returns
/// true as well.
#define vec_erase_if(element, vector, ...)                                       \
    do {                                                                         \
        size_t _first = vec_find_if_index(element, vector, __VA_ARGS__);         \
        if (_first != NO_INDEX) {                                               \
            for (size_t _i = _first; _i < (vector).size; _i++) {                 \
                auto element = (vector).data + _i;                               \
                if (!(__VA_ARGS__)) (vector).data[_first++] = (vector).data[_i]; \
            }                                                                    \
            vec_resize(vector, _first);                                          \
        }                                                                        \
    } while (0)

// ====================================================================
//  Types
// ====================================================================
typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned long uintptr_t;
typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;
typedef long i64;
typedef unsigned long u64;

// ====================================================================
//  Stdlib
// ====================================================================
void *malloc(size_t size);
void *calloc(size_t count, size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);
char *strdup(const char *str);
int strncmp(const char *s1, const char *s2, size_t n);
size_t strlen(const char *s);
int memcmp(const void *s1, const void *s2, size_t n);
void *memset(void *s, int c, size_t n);
void *memcpy(void *restrict dest, const void *restrict src, size_t n);
void *memmove(void *dest, const void *src, size_t n);

void perror(const char *msg);
int puts(const char *msg);
int putchar(int c);

int open(const char *path, int flags, ...);
ssize_t read(int fd, void *buf, size_t count);

noreturn exit(int code);

__attribute__((format(printf, 1, 2))) int printf(const char *format, ...);

// ====================================================================
//  Helpers
// ====================================================================
typedef struct span {
    const char *data;
    u64 size;
} span;

typedef vec(char) string;

typedef struct loc {
    span file;
    size_t line;
} loc;

noreturn die(const char *msg) {
    printf("fatal error: %s\n", msg);
    exit(1);
}

void print_loc(loc l) {
    printf("%.*s:%zu: ", (int) l.file.size, l.file.data, l.line);
}

#define as_span(x)    ((span) {.data = x.data, .size = x.size})
#define lit_string(x) ((string) {.data = strdup(x), .size = strlen(x), .capacity = strlen(x) + 1})
#define lit_span(x)   ((span) {.data = x, .size = sizeof(x) - 1})

#define str_copy(s)        vec_copy(s)
#define str_cat(s1, s2)    vec_append(s1, s2)
#define str_cat_lit(s, l)  vec_append(s, lit_span(l))
#define str_cat_char(s, c) vec_push(s, c)

#define eq(_s1, _s2) ({                                          \
    auto s1 = (_s1);                                             \
    auto s2 = (_s2);                                             \
    s1.size == s2.size&& memcmp(s1.data, s2.data, s1.size) == 0; \
})

// ====================================================================
//  Lexer
// ====================================================================
typedef enum {
    tt_invalid,
    tt_eof,
    tt_pp_name,
    tt_pp_param, // Parameter in the replacement text of a macro.
    tt_pp_va_opt,
    tt_pp_va_args,
    tt_int_lit,
    tt_char,
    tt_string,

    tt_lparen,
    tt_rparen,
    tt_lbrace,
    tt_rbrace,
    tt_lbrack,
    tt_rbrack,
    tt_gt,
    tt_gt_eq,
    tt_lt,
    tt_lt_eq,
    tt_eq_eq,
    tt_bang_eq,
    tt_bang,
    tt_assign,
    tt_caret,
    tt_amp,
    tt_pipe,
    tt_tilde,
    tt_amp_amp,
    tt_pipe_pipe,
    tt_lt_lt,

    tt_gt_gt,
    tt_slash,
    tt_per_cent,
    tt_star,
    tt_plus,
    tt_minus,

    tt_plus_eq,
    tt_minus_eq,
    tt_star_eq,
    tt_slash_eq,
    tt_per_cent_eq,
    tt_caret_eq,
    tt_amp_eq,
    tt_pipe_eq,
    tt_lt_lt_eq,
    tt_gt_gt_eq,
    tt_plus_plus,
    tt_minus_minus,

    tt_hash,
    tt_hash_hash,
    tt_semi,
    tt_comma,
    tt_colon,
    tt_dot,
    tt_ellipsis,
    tt_question,
    tt_arrow,

#define kw(x) tt_kw_##x,
    ALL_KEYWORDS(kw)
#undef kw
} tt;

typedef struct tok {
    loc loc;
    tt type;
    bool start_of_line;
    bool whitespace_before;

    // The contents of a string, character literal, or identifier.
    string name;

    // The value of a numeric literal; for a __VA_OPT__ token, the
    // index of the matching closing parenthesis.
    u64 val;
} tok;

typedef vec(tok) tokens;
typedef vec(string) strings;
typedef vec(tokens) tokens_vec;

typedef struct lexer {
    loc loc;
    char *data;
    const char *char_ptr;
    const char *end_ptr;
    char c;
    bool start_of_line;
    bool whitespace_before;
} *lexer;

typedef struct macro_expansion {
    struct macro *m;
    tokens toks;
    size_t cursor;
} macro_expansion;

typedef struct token_buffer {
    tokens toks;
    size_t cursor;
} token_buffer;

typedef struct token_stream {
    enum token_stream_kind {
        ts_lexer,
        ts_macro_expansion,
        ts_toks,
    } kind;

    union {
        struct lexer lex;
        macro_expansion expansion;
        token_buffer toks;
    };
} *token_stream;

bool lex_eof(lexer l) {
    return l->c == 0;
}

void lex_char_raw(lexer l) {
    if (l->char_ptr >= l->end_ptr) {
        l->c = 0;
        return;
    }

    l->c = *l->char_ptr++;
}

void lex_char(lexer l) {
    lex_char_raw(l);
    switch (l->c) {
        case '\n':
            l->loc.line++;
            break;
        case '\\':
            if (l->char_ptr < l->end_ptr && *l->char_ptr == '\n') {
                l->char_ptr++;
                l->loc.line++;
                lex_char(l);
            }
    }
}

bool lex_eat(lexer l, char c) {
    if (l->c != c)
        return false;

    lex_char(l);
    return true;
}

void lex_skip_ws(lexer l) {
    l->start_of_line = false;
    l->whitespace_before = false;
    for (;;) {
        switch (l->c) {
            default: return;
            case ' ':
            case '\t':
            case '\f':
            case '\v':
            case '\r':
                lex_char(l);
                l->whitespace_before = true;
                break;

            case '\n':
                lex_char(l);
                l->whitespace_before = l->start_of_line = true;
                break;
        }
    }
}

void lex_skip_line(lexer l) {
    // don't eat the \n here so we can set the start of line flag
    while (!lex_eof(l) && l->c != '\n')
        lex_char(l);
}

bool lex_is_continue(char c) {
    switch (c) {
        default: return false;
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '_':
        case '0' ... '9':
        case '$':
            return true;
    }
}

noreturn lex_error(lexer l, const char *msg) {
    print_loc(l->loc);
    printf("fatal error: %s\n", msg);
    exit(1);
}

bool is_octal_or_sep(char c) { return c == '\'' || (c >= '0' && c <= '7'); }
bool is_hex_or_sep(char c) { return c == '\'' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }
bool is_bin_or_sep(char c) { return c == '\'' || c == '0' || c == '1'; }
bool is_decimal_or_sep(char c) { return c == '\'' || (c >= '0' && c <= '9'); }

void lex_chars(lexer l, tok *t, bool(char_p)(char)) {
    while (char_p(l->c)) {
        vec_push(t->name, l->c);
        lex_char(l);
    }

    if (is_hex_or_sep(l->c))
        lex_error(l, "unexpected character in integer literal");
}

void lex_number(lexer l, tok *t, bool(digit_p)(char), u8 base) {
    lex_chars(l, t, digit_p);
    string name = t->name;
    t->val = 0;
    vec_for_val(c, name) {
        u64 prev = t->val;
        if (c == '\'') continue;
        if (c >= '0' && c <= '9') t->val = t->val * base + (u8) (c - '0');
        else if (c >= 'a' && c <= 'f') t->val = t->val * base + (u8) (c - 'a' + 10);
        else if (c >= 'A' && c <= 'F') t->val = t->val * base + (u8) (c - 'A' + 10);
        else die("unreachable");
        if (t->val < prev) lex_error(l, "integer literal overflow");
    }
}

tt lex_string(lexer l, tok *t, char delim) {
    vec_clear(t->name);
    while (!lex_eof(l) && l->c != delim) {
        switch (l->c) {
            default:
                vec_push(t->name, l->c);
                lex_char(l);
                break;
            case '\n': lex_error(l, "unclosed string literal");
            case '\\': {
                lex_char(l);
                char c = l->c;
                lex_char(l);
                switch (c) {
                    default: lex_error(l, "unsupported escape character");
                    case 'n': vec_push(t->name, '\n'); break;
                    case 'r': vec_push(t->name, '\r'); break;
                    case 't': vec_push(t->name, '\t'); break;
                    case 'b': vec_push(t->name, '\b'); break;
                    case 'f': vec_push(t->name, '\f'); break;
                    case 'a': vec_push(t->name, '\a'); break;
                    case 'v': vec_push(t->name, '\v'); break;
                    case '0': vec_push(t->name, '\0'); break;
                    case '\'': vec_push(t->name, '\''); break;
                    case '\"': vec_push(t->name, '\"'); break;
                    case '\\': vec_push(t->name, '\\'); break;
                }
            }
        }
    }

    if (!lex_eat(l, delim))
        lex_error(l, "string or character literal terminated by eof");

    if (delim == '\'' && t->name.size != 1)
        lex_error(l, "character literals must be exactly 1 character");

    return delim == '\'' ? tt_char : tt_string;
}

tt lex(lexer l, tok *t) {
    lex_skip_ws(l);
    t->start_of_line = l->start_of_line;
    t->whitespace_before = l->whitespace_before;

    if (lex_eof(l))
        return tt_eof;

    char c = l->c;
    lex_char(l);
    switch (c) {
        case '/': {
            if (lex_eat(l, '/')) {
                lex_skip_line(l);
                return lex(l, t);
            }
            return lex_eat(l, '=') ? tt_slash_eq : tt_slash;
        }

        case '#':
            return lex_eat(l, '#') ? tt_hash_hash : tt_hash;

        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '_':
        case '$': {
            vec_clear(t->name);
            vec_push(t->name, c);
            lex_chars(l, t, lex_is_continue);
            return tt_pp_name;
        }

        case '0' ... '9': {
            vec_clear(t->name);
            if (c == '0') lex_number(l, t, is_octal_or_sep, 8);
            else if (lex_eat(l, 'x') || lex_eat(l, 'X')) lex_number(l, t, is_hex_or_sep, 16);
            else if (lex_eat(l, 'b') || lex_eat(l, 'B')) lex_number(l, t, is_bin_or_sep, 2);
            else {
                vec_push(t->name, c);
                lex_number(l, t, is_decimal_or_sep, 10);
            }
            return tt_int_lit;
        }

        case '.': {
            if (!lex_eat(l, '.')) return tt_dot;
            if (!lex_eat(l, '.')) lex_error(l, "expected '.' after '..'");
            return tt_ellipsis;
        }

        case '\'':
        case '\"':
            return lex_string(l, t, c);

        case '(': return tt_lparen;
        case ')': return tt_rparen;
        case '{': return tt_lbrace;
        case '}': return tt_rbrace;
        case '[': return tt_lbrack;
        case ']': return tt_rbrack;
        case '?': return tt_question;
        case ';': return tt_semi;
        case ',': return tt_comma;
        case ':': return tt_colon;
        case '~': return tt_tilde;

        case '*': return lex_eat(l, '=') ? tt_star_eq : tt_star;
        case '=': return lex_eat(l, '=') ? tt_eq_eq : tt_assign;
        case '!': return lex_eat(l, '=') ? tt_bang_eq : tt_bang;
        case '^': return lex_eat(l, '=') ? tt_caret_eq : tt_caret;

        case '+': return lex_eat(l, '=') ? tt_plus_eq : lex_eat(l, '+') ? tt_plus_plus
                                                                        : tt_plus;
        case '&': return lex_eat(l, '&') ? tt_amp_amp : lex_eat(l, '=') ? tt_amp_eq
                                                                        : tt_amp;
        case '|': return lex_eat(l, '|') ? tt_pipe_pipe : lex_eat(l, '=') ? tt_pipe_eq
                                                                          : tt_pipe;

        case '-':
            if (lex_eat(l, '>')) return tt_arrow;
            return lex_eat(l, '=') ? tt_minus_eq : lex_eat(l, '-') ? tt_minus_minus
                                                                   : tt_minus;

        case '<':
            if (lex_eat(l, '<')) return lex_eat(l, '=') ? tt_lt_lt_eq : tt_lt_lt;
            return lex_eat(l, '=') ? tt_lt_eq : tt_lt;

        case '>':
            if (lex_eat(l, '>')) return lex_eat(l, '=') ? tt_gt_gt_eq : tt_gt_gt;
            return lex_eat(l, '=') ? tt_gt_eq : tt_gt;

        default:
            lex_error(l, "unexpected character");
    }
}

bool tok_has_str(const tok *t) {
    switch (t->type) {
        default: return false;
        case tt_string:
        case tt_char:
        case tt_pp_name:
        case tt_pp_param:
        case tt_int_lit: // Needed for stringising e.g. `1'024'`.
            return true;
    }
}

tok tok_copy(const tok *t) {
    tok copy = *t;
    if (tok_has_str(t)) copy.name = str_copy(t->name);
    else copy.name = (string) {};
    return copy;
}

void tok_free(tok *t) {
    if (tok_has_str(t)) vec_free(t->name);
}

tok tok_move(tok *t) {
    tok moved = *t;
    *t = (tok) {};
    return moved;
}

void tok_move_into(tok *a, tok *b) {
    tok_free(a);
    *a = tok_move(b);
}

// ====================================================================
//  Preprocessor
// ====================================================================
typedef struct macro {
    string name;
    tokens tokens;
    strings params;
    bool is_variadic;
    bool is_function_like;
    bool expanding;
} *macro;

// State for __VA_OPT__ processing.
typedef struct pp_va_opt {
    size_t start_of_expansion;
    size_t index;
    size_t rparen_index;
    loc stringise_loc;
    bool stringise;
    bool stringise_whitespace_before;
    bool paste_tokens;
    bool ends_with_placemarker;
} pp_va_opt;

typedef struct pp_expansion {
    struct pp* pp;
    pp_va_opt va_opt;
    size_t cursor;            ///< The index of the token in the replacement list we’re processing.
    tokens_vec *args;         ///< The arguments bound to each parameter + one more for __VA_ARGS__.
    tokens_vec expanded_args; ///< The fully macro-replaced arguments. One for each argument.
    tokens expansion;         ///< The resulting expansion of this macro.
    tokens aux;               ///< Auxiliary vector used as temporary storage.
    macro m;                  ///< The macro being expanded.
} *pp_expansion;

typedef struct pp {
    tokens look_ahead_tokens;
    vec(struct token_stream) token_streams;
    vec(struct macro) defs;
    strings filenames;
    bool retain_empty_streams;
} *pp;

void pp_conv(pp pp, tok *t);
noreturn pp_error(pp pp, const char *msg);
void pp_free_macro(macro m);
macro pp_get_expandable_macro(pp pp, span name);
macro pp_get_macro_and_args(pp pp, tokens_vec *args, span name);
tok *pp_look_ahead(pp pp, size_t n);
void pp_enter_token_stream(tokens *toks);
void pp_materialise_look_ahead_toks(pp pp);
void pp_preprocess(pp pp, tok *t);
void pp_read_token(pp pp, tok *t);
void pp_read_and_expand_token(pp pp, tok *t);
void pp_read_token_raw(pp pp, tok *t);
void pp_read_token_raw_impl(pp pp, tok *t, bool include_look_ahead);
void pp_stringise_token(string *s, const tok *t, bool escape);
tok pp_stringise_tokens(const tokens *toks, loc l, bool whitespace_before);
bool pp_ts_done(token_stream t);
void pp_ts_pop(pp pp);
bool pp_undefine(pp pp, span name);
void pp_va_opt_reset(pp_expansion exp);

void pp_add_lexer(pp pp, const char *filename) {
    int fd = open(filename, 0);
    if (fd < 0) {
        perror(filename);
        die("failed to open file");
    }

    char *buffer = malloc(ONE_MEGABYTE);
    long n_read = read(fd, buffer, ONE_MEGABYTE);
    if (n_read < 0) {
        perror(filename);
        die("failed to read file");
    }

    if (n_read == ONE_MEGABYTE) {
        printf("while reading: %s\n", filename);
        die("this file is too big for us");
    }

    vec_push(pp->filenames, lit_string(filename));

    struct lexer l = {};
    l.loc.file = as_span(vec_back(pp->filenames));
    l.loc.line = 1;
    l.data = buffer;
    l.char_ptr = buffer;
    l.end_ptr = buffer + (size_t) n_read;
    l.c = '\n'; // to set the start of line flag

    pp_materialise_look_ahead_toks(pp);
    vec_push(pp->token_streams, (struct token_stream) {
                                    .kind = ts_lexer,
                                    .lex = l,
                                });
}

void pp_conv(pp pp, tok *t) {
    if (t->type == tt_pp_name) {
        if (false) {}
#define kw(x) \
    else if (eq(t->name, lit_span(#x))) t->type = tt_kw_##x;
        ALL_KEYWORDS(kw)
#undef kw
    }
}

bool pp_is_param(const tok *t) {
    return t->type == tt_pp_param || t->type == tt_pp_va_args;
}

bool pp_dir_define_check_hash_impl(macro m, size_t cursor) {
    if (m->tokens.size == cursor) return false;
    auto t = &m->tokens.data[cursor];
    return pp_is_param(t) || t->type == tt_pp_va_opt;
}

void pp_dir_define_check_hash(pp pp, macro m, size_t cursor) {
    if (!pp_dir_define_check_hash_impl(m, cursor))
        pp_error(pp, "expected parameter name or '__VA_OPT__' after '#'");
}

void pp_dir_define_check_hash_hash(pp pp, macro m, size_t cursor) {
    if (cursor == 1 || m->tokens.size == cursor)
        pp_error(pp, "'##' must not occur at the start or end of a macro definition");
}

size_t pp_find_va_opt_rparen(pp pp, macro m, size_t *cursor) {
    if (
        m->tokens.size != *cursor ||
        m->tokens.data[*cursor].type != tt_lparen
    ) pp_error(pp, "expected '(' after '__VA_OPT__'");
    (*cursor)++;

    size_t parens = 1;
    while (*cursor < m->tokens.size) {
        auto t = &m->tokens.data[(*cursor)++];
        switch (t->type) {
            default: break;
            case tt_hash: pp_dir_define_check_hash(pp, m, *cursor); break;
            case tt_hash_hash: pp_dir_define_check_hash_hash(pp, m, *cursor); break;
            case tt_lparen: parens++; break;
            case tt_rparen:
                if (--parens == 0) return *cursor - 1;
                break;
            case tt_pp_va_opt:
                pp_error(pp, "'__VA_OPT__' cannot be nested");
                break;
        }
    }

    pp_error(pp, "missing ')' after '__VA_OPT__'");
}

void pp_dir_define(pp pp) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_pp_name || pp->tok.start_of_line)
        pp_error(pp, "expected macro name");

    struct macro m = {};
    m.name = str_copy(pp->tok.name);
    pp_read_token_raw(pp);

    m.is_function_like = pp->tok.type == tt_lparen && !pp->tok.whitespace_before;
    if (m.is_function_like) {
        pp_read_token_raw(pp);

        while (pp->tok.type != tt_eof && pp->tok.type != tt_rparen && !pp->tok.start_of_line) {
            if (pp->tok.type == tt_ellipsis) {
                m.is_variadic = true;
                pp_read_token_raw(pp);
                break;
            }

            if (pp->tok.type == tt_pp_name) {
                if (vec_find_if(arg, m.params, eq(*arg, pp->tok.name)))
                    pp_error(pp, "duplicate macro argument");

                vec_push(m.params, str_copy(pp->tok.name));
                pp_read_token_raw(pp);

                if (pp->tok.type == tt_comma && !pp->tok.start_of_line) {
                    pp_read_token_raw(pp);
                    continue;
                }

                break;
            }

            pp_error(pp, "unexpected token in macro argument list");
        }

        if (pp->tok.type != tt_rparen) pp_error(pp, "expected ')'");
        if (pp->tok.start_of_line) pp_error(pp, "terminating ')' of macro argument list must be on the same line as the definition");
        pp_read_token_raw(pp);
    }

    while (!pp->tok.start_of_line) {
        if (pp->tok.type == tt_pp_name) {
            if (eq(pp->tok.name, lit_span("__VA_ARGS__"))) pp->tok.type = tt_pp_va_args;
            else if (eq(pp->tok.name, lit_span("__VA_OPT__"))) pp->tok.type = tt_pp_va_opt;
            else if (vec_find_if(p, m.params, eq(*p, pp->tok.name))) pp->tok.type = tt_pp_param;
        }

        if (!m.is_variadic && (pp->tok.type == tt_pp_va_args || pp->tok.type == tt_pp_va_opt))
            pp_error(pp, "'__VA_OPT__'/'__VA_ARGS__' cannot be used in non-variadic macros");

        vec_push(m.tokens, tok_move(&pp->tok));
        pp_read_token_raw(pp);
    }

    if (m.is_function_like) {
        for (size_t cursor = 0; cursor < m.tokens.size;) {
            auto t = &m.tokens.data[cursor++];
            if (t->type == tt_hash) pp_dir_define_check_hash(pp, &m, cursor);
            else if (t->type == tt_hash_hash) pp_dir_define_check_hash_hash(pp, &m, cursor);
            else if (t->type == tt_pp_va_opt) t->val = pp_find_va_opt_rparen(pp, &m, &cursor);
        }
    }

    pp_undefine(pp, as_span(m.name));
    vec_push(pp->defs, m);
}

void pp_dir_undef(pp pp) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_pp_name)
        pp_error(pp, "expected macro name");

    if (!pp_undefine(pp, as_span(pp->tok.name))) {
        print_loc(pp->tok.loc);
        printf("warning: macro '%.*s' is not defined\n", (int) pp->tok.name.size, pp->tok.name.data);
    }
}

noreturn pp_error(pp pp, const char *msg) {
    print_loc(pp->tok.loc);
    printf("fatal error: %s\n", msg);
    exit(1);
}

const tok* pp_cur(pp_expansion exp) {
    assert(exp->cursor < exp->m->tokens.size && "cursor out of bounds");
    return &exp->m->tokens.data[exp->cursor];
}

size_t pp_get_param_index(pp_expansion exp, const tok *t) {
    assert(t->type != tt_pp_va_opt && "__VA_OPT__ cannot be handled here");
    if (t->type == tt_pp_va_args) return exp->m->params.size;
    auto idx = vec_find_if_index(p, exp->m->params, eq(*p, t->name));
    assert(idx != NO_INDEX);
    return idx;
}

tokens *pp_get_param_tokens(pp_expansion exp, const tok *t) {
    assert(t->type == tt_pp_param || t->type == tt_pp_va_args);
    return &exp->args->data[pp_get_param_index(exp, t)];
}

/// This implements C23 6.10.5.1 'Argument substitution'.
///
/// This only handles the case of there not being any preceding #
/// or ## tokens or any following ##. This *does* handle __VA_ARGS__,
/// as that is just another argument.
nodiscard tokens *pp_substitute(pp_expansion exp, size_t param_index) {
    auto pp = exp->pp;

    // p4: 'the replacement preprocessing tokens are the preprocessing tokens
    // of the corresponding argument after all macros contained therein have been
    // expanded.'
    auto arg_toks = exp->args->data + param_index;

    // Check if we need to expand this, i.e. if there are any macro names in
    // the argument list. If not, we can append the tokens as is.
    if (!vec_find_if(a, *arg_toks, a->type == tt_pp_name && pp_get_expandable_macro(pp, as_span(a->name))))
        return arg_toks;

    // Check if we’ve already computed the expansion of this argument.
    if (exp->expanded_args.size != exp->m->params.size)
        vec_resize(exp->expanded_args, exp->m->params.size);

    auto expanded_arg = &exp->expanded_args.data[param_index];
    if (expanded_arg->size)
        return expanded_arg;

    // p4: 'The argument’s preprocessing tokens are completely macro replaced before
    // being substituted as if they formed the rest of the preprocessing file with no
    // other preprocessing tokens being available.'
    pp_materialise_look_ahead_toks(pp);
    pp_enter_token_stream(arg_toks);
    pp->retain_empty_streams = true;
    while (!pp_ts_done(&vec_front(pp->token_streams))) {
        tok t = {};
        pp_read_and_expand_token(pp, &t);
        vec_push(*expanded_arg, t);
    }
    pp->retain_empty_streams = false;
    pp_ts_pop(pp);
    return expanded_arg;
}

void pp_paste(pp_expansion exp, const tok* t) {
    tok* before = &vec_back(exp->expansion);

    string concat = {};
    pp_stringise_token(&concat, before, false);
    pp_stringise_token(&concat, t, false);

    struct lexer l = {};
    l.loc = before->loc;
    l.char_ptr = concat.data;
    l.end_ptr = concat.data + concat.size;
    l.c = ' ';
    tok tmp = {};
    tmp.type = lex(&l, &tmp);
    if (!lex_eof(&l)) pp_error(exp->pp, "token pasting did not produce a valid pp-token");
    tmp.loc = before->loc;
    tmp.whitespace_before = before->whitespace_before;
    tmp.start_of_line = before->start_of_line;
    tok_move_into(before, &tmp);
}

nodiscard tok pp_stringise(pp_expansion exp, const tok *t) {
    tokens *param = pp_get_param_tokens(exp, t);
    assert(param && "'#' must be followed by parameter");
    return pp_stringise_tokens(param, t->loc, t->whitespace_before);
}

void pp_enter_token_stream(pp pp, tokens *toks) { // clang-format off
    pp_materialise_look_ahead_toks(pp);
    tokens copy = {};
    vec_for(t, *toks) vec_push(copy, tok_copy(t));
    vec_push(pp->token_streams, (struct token_stream) {
        .kind = ts_toks,
        .toks = {
            .toks = copy,
            .cursor = 0,
        },
    });
} // clang-format on

void pp_defer_stringise_va_opt(pp_expansion exp, const tok* hash) {
    assert(!exp->va_opt.stringise && "va_opt not reset");
    exp->va_opt.stringise = true;
    exp->va_opt.stringise_loc = hash->loc;
    exp->va_opt.stringise_whitespace_before = hash->whitespace_before;
}

void pp_enter_va_opt(pp_expansion exp, const tok* va_opt_token) {
    exp->va_opt.index = exp->cursor;
    exp->va_opt.rparen_index = va_opt_token->val;
    exp->va_opt.start_of_expansion = exp->expansion.size;
    exp->cursor++; // Yeet '__VA_OPT__'.
    exp->cursor++; // Yeet '('.
}

bool pp_in_va_opt(pp_expansion exp) {
    return exp->va_opt.index != NO_INDEX;
}

bool pp_has_variadic_args(pp_expansion exp) {
    // We treat __VA_ARGS__ as just another parameter, so its expansion can be
    // retrieved in much the same manner.
    auto param = pp_substitute(exp, exp->m->params.size);
    return param->size != 0;
}

void pp_expand_impl(pp_expansion exp) {
    pp_va_opt_reset(exp);

    // Process the replacement token list, performing argument substitution,
    // evaluation of '#' and '##', and handling of __VA_OPT__.
    //
    // The wording for this in the C standard, as ever, is rather obscure, and
    // doesn’t exactly lend itself well to direct implementation, for which reason
    // this is largely adapted from Clang’s lexer instead. Roughly, this is how
    // each of these features is implemented.
    while (exp->cursor < exp->m->tokens.size) {
        auto t = pp_cur(exp);

        // __VA_OPT__
        //
        // Skip '__VA_OPT__(' and mark that we're inside of __VA_OPT__.
        if (t->type == tt_pp_va_opt) {
            pp_enter_va_opt(exp, t);
            continue;
        }

        // We’re inside of __VA_OPT__.
        if (pp_in_va_opt(exp)) {
            // This token is not the closing rparen.
            if (exp->cursor != exp->va_opt.rparen_index) {
                // Discard it if __VA_ARGS__ expands to nothing.
                if (!pp_has_variadic_args(exp)) {
                    exp->cursor++;
                    continue;
                }

                // If we *do* have variadic arguments, then let the rest of this loop
                // process this token.
            }

            // This *is* the closing rparen.
            else {
                exp->cursor++; // Yeet it.
                assert(
                    !exp->va_opt.paste_tokens || exp->va_opt.stringise &&
                    "only paste tokens here if we’re also stringising"
                );

                // Perform stringising now if we need to. This needs to be done before
                // token pasting should that also be required. Note that this involves
                // stringising all the tokens produced by the __VA_OPT__.
                if (exp->va_opt.stringise) {
                    tokens toks = {};
                    toks.data = exp->expansion.data + exp->va_opt.start_of_expansion;
                    toks.size = exp->expansion.size - exp->va_opt.start_of_expansion;
                    tok s = pp_stringise_tokens(&toks, exp->va_opt.stringise_loc, exp->va_opt.stringise_whitespace_before);

                    // Paste the string literal if need be.
                    if (exp->va_opt.paste_tokens) {
                        pp_paste(exp, &s);
                        tok_free(&s);
                    } else {
                        vec_push(exp->expansion, s);
                    }
                }

                // If the last token in __VA_OPT__ is a placemarker, discard a single
                // paste operation to consume the placemarker.
                if (
                    exp->va_opt.ends_with_placemarker &&
                    exp->cursor < exp->m->tokens.size &&
                    pp_cur(exp)->type == tt_hash_hash
                ) exp->cursor++;

                pp_va_opt_reset(exp);
                continue;
            }
        }

        // Stringising operator.
        if (t->type == tt_hash) {
            exp->cursor++; // Yeet '#'.
            auto hash = t;
            t = pp_cur(exp);

            // If the next token is __VA_OPT__, it’s easier to handle this
            // after we’re done processing it.
            if (t->type == tt_pp_va_opt) {
                pp_defer_stringise_va_opt(exp, hash);
                continue;
            }

            vec_push(exp->expansion, pp_stringise(exp, t));
            continue;
        }

        // Token pasting operator.
        //
        // The standard specifies this in a rather cumbersome manner; there are several
        // possible cases we need to handle here; some of them are handled when we encounter
        // the lhs of '##', some when we encounter the '##' itself. Doing so allows us to
        // circumvent the need for ‘placemarker tokens’, which the standard uses to deal
        // with empty pastes.
        //
        // Note that '##' always has a lhs and rhs; otherwise, the macro definition would
        // be invalid, and we would already have errored about this.
        //
        // The lhs can be either:
        //
        //    1. a parameter (including __VA_ARGS__) whose argument is empty;
        //    2. a parameter whose argument is *not* empty;
        //    3. a __VA_OPT__(...) that produces no tokens;
        //    4. a __VA_OPT__(...) that produces at least one token;
        //    5. any other token, with no special meaning.
        //
        // Cases 1/3 are handled when the parameter/__VA_OPT__(...) is encountered; they are
        // no-ops, so we simply dropping the '##'. In the standard’s language, this is 'A##'
        // where 'A' is a placemarker, and we discard both the placemarker and '##'.
        //
        // Cases 2/4 can be reduced to 5 by just appending all the tokens involved to the expansion.
        // We append the singular token in 5 as well, which means that the lhs of the paste operation
        // is always the last token of the expansion.
        //
        // Note that the lhs can’t be '#' or '##':
        //
        //   - It can’t be '#' because '###' is lexed as '## #', not '# ##', due to the maximal munch
        //     rule, and moreover, '#' must be followed by a parameter, which '##' isn’t.
        //
        //   - It can’t be '##' because we treat '##' as left-associative and because the last paragraph
        //     implies that a pasting operation can never result in '##'.
        //
        // As for the rhs, it can either be:
        //
        //  1. a parameter (including __VA_ARGS__) whose argument is empty;
        //  2. a parameter whose argument is *not* empty;
        //  3. __VA_OPT__(...) that produces no tokens;
        //  4. __VA_OPT__(...) that produces at least one token;
        //  5. another '##';
        //  6. a stringising operation on __VA_OPT__, i.e. '#__VA_OPT__(...)';
        //  7. any other stringising operation, i.e. '#';
        //  8. any other token, with no special meaning.
        //
        // Case 1 means the paste is a no-op, so we drop the rhs and the '##' and do nothing. Case 5 is
        // handled by treating any sequence of '##' as a single '##' everywhere, which means that
        // this case is actually impossible.
        //
        // FIXME: The following isn’t true. Re-examine all places where the standard produces a placemarker
        //    and all places where we skip '##'.
        // We can do this since e.g. 'A####B' is always equivalent
        // to 'A##B' iff 'A' is not a placemarker (in which.
        //
        // Case 3 (which is a no-op) and case 4 can be handled by pasting with the first token after
        // '__VA_OPT__(' (if there is one), or by doing nothing if we have '__VA_OPT__()'. This reduces
        // cases 3/4 to the other cases.
        //
        // Case 6 can only be handled once we’re done processing the __VA_OPT__(...), so we set a
        // flag to indicate that we need to perform pasting later and simply drop the '##'. The
        // actual pasting is then handled when we encounter the right paren of the __VA_OPT__(...).
        //
        // Case 7 is implemented by stringising first and then performing a paste with the
        // resulting token; this reduces it to case 8 since the rhs is always a string literal
        // in that case.
        //
        // Finally, case 2 is handled by pasting with the first token of the parameter, and case 8
        // by pasting with that token.
        if (t->type == tt_hash_hash) {
            // Skip all '##'s (rhs case 5).
            pp_skip_hash_hash_seq(exp);

            // The next token is __VA_OPT__ (rhs cases 3/4).
            t = pp_cur(exp);
            if (t->type == tt_pp_va_opt) {
                pp_enter_va_opt(exp, t);

                // If the next token is ')', we have '__VA_OPT__()', so do nothing. The __VA_OPT__
                // code will deal with the ')', and the paste is a no-op anyway. Also do this if
                // the __VA_OPT__ contents are discarded.
                t = pp_cur(exp);
                if (t->type == tt_rparen || !pp_has_variadic_args(exp)) continue;

                // The next token can’t be '##' since '__VA_OPT__(##' is invalid; it also can’t
                // be another __VA_OPT__ since they can’t be nested. This means the first token
                // of __VA_OPT__ must fall into one of the other cases, so simply fall through
                // here and handle it below.
            }

            // The next token is '#' (rhs case 6/7).
            if (t->type == tt_hash) {
                exp->cursor++; // Yeet '#'.
                auto hash = t;
                t = pp_cur(exp);

                // Case 6.
                if (t->type == tt_pp_va_opt) {
                    exp->va_opt.paste_tokens = true;
                    pp_defer_stringise_va_opt(exp, hash);
                    continue;
                }

                // Case 7.
                exp->cursor++; // Yeet the parameter after '#'.
                tok str = pp_stringise(exp, t);
                pp_paste(exp, &str);
                tok_free(&str);
                continue;
            }

            // The next token is a regular (= non-parameter) token (case 8).
            if (!pp_is_param(t)) {
                exp->cursor++; // Yeet the token.
                pp_paste(exp, t);
                continue;
            }

            // The next token is a parameter (cases 1/2).
            exp->cursor++; // Yeet the parameter.
            auto toks = pp_get_param_tokens(exp, t);

            // Case 1.
            //
            // Don’t do anything else here. We only get here if the lhs was
            // non-empty, i.e. we have 'A##B' w/ 'A' non-empty, 'B' empty,
            // which results in 'A' with no placemarkers according to the
            // standard. This means the lhs can still be pasted with something
            // else if we have e.g. 'A##B##C', so leave any following '##'s
            // in the input.
            if (toks->size == 0) continue;

            // Case 2.
            pp_paste(exp, &vec_front(*toks));
            vec_for_index(i, *toks)
                if (i >= 1)
                    vec_push(exp->expansion, tok_copy(&toks->data[i]));
            continue;
        }

        // If the next token is not a parameter, it must be a regular, non-special
        // token; such tokens can never be ‘empty’, so we don’t need to worry about
        // empty pasting just yet.
        if (!pp_is_param(t)) {
            exp->cursor++;
            vec_push(exp->expansion, tok_copy(t));
            continue;
        }

        // Otherwise, we’re looking at a parameter. If the next token is '##', handle
        // the lhs cases 1/2 for '##' (see above).
        exp->cursor++; // Yeet the parameter.
        if (exp->cursor < exp->m->tokens.size && exp->m->tokens.data[exp->cursor].type == tt_hash_hash) {
            auto param = pp_get_param_tokens(exp, t);

            // The parameter expands to nothing; yeet any subsequent paste operations,
            // since 'A##B' yields 'B' if 'A' is empty, and append its tokens otherwise.
            if (param->size == 0) {
                pp_skip_hash_hash_seq(exp);
            } else {
                vec_for(p, *param) vec_push(exp->expansion, tok_copy(p));
            }
            continue;
        }

        // Finally, if we get here, we have a parameter and there is no paste in sight.
        // in this case, the standard requires that we fully expand it before appending
        // it to the expansion (this does not happen for operands of '#' or '##').
        auto toks = pp_substitute(exp, pp_get_param_index(exp, t));
        vec_for(p, *toks) vec_push(exp->expansion, tok_copy(p));

        // There is an edge case here: if we have '__VA_OPT__(a b)##c' where 'b' is empty,
        // then we get 'a c' rather than 'ac', i.e. the 'b' acts as a placemarker in that
        // position. This is only relevant if the '##' is *after* the ')' since a '##' that
        // comes *before* a __VA_OPT__ is handled differently.
        if (toks->size == 0 && pp_in_va_opt(exp) && exp->cursor == exp->va_opt.rparen_index)
            exp->va_opt.ends_with_placemarker = true;
    }
}

void pp_expand(pp pp, tok *t, macro m, tokens_vec *args) {
#define DIAG(...)                                                                               \
    do {                                                                                        \
        print_loc(t->loc);                                                                      \
        printf("fatal error in expansion of macro '%.*s': ", (int) m->name.size, m->name.data); \
        printf("  " __VA_ARGS__);                                                               \
        exit(1);                                                                                \
    } while (false)

    assert(args);

    // Replacement list is empty. Don't bother doing anything.
    if (m->tokens.size == 0) return;

    // If this is a function-like macro, check that we have enough args.
    if (m->is_function_like) {
        if (!m->is_variadic) {
            if (args->size != m->params.size) DIAG(
                "mismatched macro argument count: expected %zu, got %zu\n",
                m->params.size,
                args->size
            );
        } else if (args->size < m->params.size) DIAG(
            "not enough arguments for macro: expected at least %zu, got %zu\n",
            m->params.size,
            args->size
        );
    }

    // Otherwise, we should have no args at all.
    else { assert(!args->size && "passing arguments to object-like macro?"); }

    // If there are no variadic arguments passed in by the user, we should have
    // synthesised an empty argument.
    assert(!m->is_variadic || args->size == m->params.size + 1);

    // Perform the expansion proper.
    struct pp_expansion exp = {};
    exp.pp = pp;
    exp.m = m;
    exp.args = args;
    pp_expand_impl(&exp);

    // Push the resulting tokens.
    if (!exp.expansion.size) return;

    // Propagate start of line flags for pretty printing only.
    auto first = &vec_front(exp.expansion);
    first->start_of_line = t->start_of_line;
    first->whitespace_before = t->whitespace_before;

    // TODO
    pp_enter_token_stream(pp, &exp.expansion);
    vec_push(pp->token_streams, (struct token_stream) {
        .kind = ts_macro_expansion,
        .expansion = (macro_expansion) {
            .m = m,
            .toks = expansion,
            .cursor = 0,
        },
    });

    /*// Next, remove all placemarkers.
    vec_erase_if(t, expansion, t->type == tt_pp_placemarker);

    string s = {};
    pp_stringise_tokens(&s, &expansion);
    /*printf("expanded %.*s to ", (int) m->name.size, m->name.data);
    printf("%.*s\n", (int) s.size, s.data);#1#
    vec_free(s);*/

    /*// At this point, we’re supposed to do a rescan, but we’ll do that as part of
    // processing this token sequence.
    return expansion;*/
#undef DIAG
}

void pp_fini(pp pp) {
    vec_delete_els(def, pp->defs)
        pp_free_macro(def);

    vec_delete_els(f, pp->filenames)
        vec_free(*f);
}

void pp_free_macro(macro m) {
    vec_free(m->name);
    vec_delete_els(t, m->tokens)
        vec_free(t->name);
}

void pp_get_macro_args(pp pp, macro m, tokens_vec *args) {
    // C23 6.10.5p11: 'The sequence of preprocessing tokens bounded by the outside-most
    // matching parentheses forms the list of arguments for the function-like macro.'
    size_t parens = 1;
    vec_push(*args, (tokens) {});
    while (pp->tok.type != tt_eof && parens) {
        switch (pp->tok.type) {
            default: break;
            case tt_lparen: parens++; break;
            case tt_rparen:
                parens--;
                if (parens == 0) continue;
                break;

            // ... 'The individual arguments within the list are separated by comma preprocessing
            // tokens, but comma preprocessing tokens between matching inner parentheses do not
            // separate arguments.
            //
            // ... 'If there is a ... in the identifier-list in the macro definition, then the trailing
            // arguments (if any), including any separating comma preprocessing tokens, are merged to
            // form a single item:'
            case tt_comma:
                if (parens == 1 && (!m->is_variadic || args->size <= m->params.size)) {
                    vec_push(*args, (tokens) {});
                    continue;
                }
                break;
        }

        vec_push(vec_back(*args), tok_move(&pp->tok));
        pp_read_token_raw(pp);
    }

    if (parens != 0 || pp->tok.type != tt_rparen)
        pp_error(pp, "unmatched ')' in macro argument list");

    // ... 'if there are as many arguments as named parameters, the macro invocation behaves as if
    // a comma token has been appended to the argument list such that variable arguments are formed
    // that contain no pp-tokens.'
    if (m->is_variadic && args->size == m->params.size)
        vec_push(*args, (tokens) {});
}

macro pp_get_expandable_macro(pp pp, span name) {
    auto macro = vec_find_if(m, pp->defs, eq(m->name, name));
    if (!macro || macro->expanding) return nullptr;
    return macro;
}

macro pp_get_macro_and_args(pp pp, tokens_vec *args, span name) {
    auto m = pp_get_expandable_macro(pp, name);
    if (!m->is_function_like) return m;
    if (pp_look_ahead(pp, 1)->type != tt_lparen) return nullptr;
    pp_read_token_raw(pp);
    pp_read_token_raw(pp);
    if (pp->tok.type != tt_rparen) pp_get_macro_args(pp, m, args);
    pp_read_token_raw(pp);
    return m;
}

tok *pp_look_ahead(pp pp, size_t n) {
    if (n == 0) return &pp->tok;
    if (n <= pp->look_ahead_tokens.size)
        return &pp->look_ahead_tokens.data[n - 1];

    tok saved = tok_move(&pp->tok);
    for (size_t i = pp->look_ahead_tokens.size; i < n; i++) {
        pp_read_token_raw_impl(pp, false);
        vec_push(pp->look_ahead_tokens, tok_move(&pp->tok));
    }

    pp->tok = tok_move(&saved);
    return pp_look_ahead(pp, n);
}

void pp_materialise_look_ahead_toks(pp pp) {
    vec_erase_if(t, pp->look_ahead_tokens, t->type == tt_eof);
    if (!pp->look_ahead_tokens.size) return;
    vec_push(pp->token_streams, (struct token_stream) {
                                    .kind = ts_toks,
                                    .toks = (token_buffer) {
                                        .toks = pp->look_ahead_tokens,
                                        .cursor = 0,
                                    },
                                });
    pp->look_ahead_tokens = (tokens) {};
}

void pp_preprocess(pp pp, tok *t) {
    if (t->type == tt_hash) {
        if (!t->start_of_line) pp_error(pp, "unexpected #");
        pp_read_token_raw(pp, t);

        // TODO: handle the GNU '#' directive.
        if (t->type != tt_pp_name)
            pp_error(pp, "expected preprocessor directive");

        if (eq(t->name, lit_span("define"))) pp_dir_define(pp);
        else if (eq(t->name, lit_span("undef"))) pp_dir_undef(pp);
        else pp_error(pp, "unknown preprocessor directive");
        return pp_preprocess(pp, t);
    }

    if (t->type == tt_pp_name) {
        tokens_vec args = {};
        macro m = pp_get_macro_and_args(pp, &args, as_span(t->name));
        if (m && !m->expanding) {
            pp_expand(pp, m, &args);
            pp_read_token_raw(pp);
            return pp_preprocess(pp);
        }
    }

    // TODO: Concatenate adjacent string literal tokens here.
}

void pp_read_token(pp pp, tok *t) {
    pp_read_and_expand_token(pp, t);
    pp_conv(pp, t);
}

void pp_read_and_expand_token(pp pp, tok *t) {
    pp_read_token_raw(pp, t);
    pp_preprocess(pp, t);
}

void pp_read_token_raw(pp pp, tok *t) {
    pp_read_token_raw_impl(pp, t, true);
}

void pp_read_token_raw_impl(pp pp, tok *t, bool include_look_ahead) {
    if (include_look_ahead && pp->look_ahead_tokens.size) {
        tok_move_into(t, &vec_front(pp->look_ahead_tokens));
        vec_pop_front(pp->look_ahead_tokens);
        return;
    }

    while (pp->token_streams.size && pp_ts_done(&vec_back(pp->token_streams))) {
        if (pp->retain_empty_streams) {
            t->type = tt_eof;
            return;
        }

        pp_ts_pop(pp);
    }

    if (!pp->token_streams.size) {
        t->type = tt_eof;
        return;
    }

    auto ts = &vec_back(pp->token_streams);
    switch (ts->kind) {
        case ts_lexer: t->type = lex(&ts->lex, t); break;
        case ts_macro_expansion:
            tok_move_into(t, &ts->expansion.toks.data[ts->expansion.cursor++]);
            break;
        case ts_toks:
            tok_move_into(t, &ts->toks.toks.data[ts->toks.cursor++]);
            break;
    }

    if (t->type == tt_eof)
        pp_read_token_raw_impl(pp, t, include_look_ahead);
}

void pp_stringise_str(string *s, span str, char delim) {
    str_cat_lit(*s, "\"");
    if (delim == '"') str_cat_lit(*s, "\\\"");
    else str_cat_lit(*s, "'");

    vec_for_val(c, str) {
        if (c == '"' || c == '\\') str_cat_char(*s, '\\');
        str_cat_char(*s, c);
    }

    if (delim == '"') str_cat_lit(*s, "\\\"");
    else str_cat_lit(*s, "'");
    str_cat_lit(*s, "\"");
}

void pp_stringise_token(string *s, const tok *t, bool escape) {
    switch (t->type) {
        case tt_invalid: str_cat_lit(*s, "<INVALID>"); break;
        case tt_eof: str_cat_lit(*s, "<EOF>"); break;
        case tt_pp_va_args: str_cat_lit(*s, "__VA_ARGS__"); break;
        case tt_pp_va_opt: str_cat_lit(*s, "__VA_OPT__"); break;
        case tt_pp_param: assert(false && "should have been replaced already"); break;
        case tt_pp_name: str_cat(*s, t->name); break;
        case tt_int_lit: str_cat(*s, t->name); break;
        case tt_lparen: str_cat_lit(*s, "("); break;
        case tt_rparen: str_cat_lit(*s, ")"); break;
        case tt_lbrace: str_cat_lit(*s, "{"); break;
        case tt_rbrace: str_cat_lit(*s, "}"); break;
        case tt_lbrack: str_cat_lit(*s, "["); break;
        case tt_rbrack: str_cat_lit(*s, "]"); break;
        case tt_gt: str_cat_lit(*s, ">"); break;
        case tt_gt_eq: str_cat_lit(*s, ">="); break;
        case tt_lt: str_cat_lit(*s, "<"); break;
        case tt_lt_eq: str_cat_lit(*s, "<="); break;
        case tt_eq_eq: str_cat_lit(*s, "=="); break;
        case tt_bang_eq: str_cat_lit(*s, "!="); break;
        case tt_bang: str_cat_lit(*s, "!"); break;
        case tt_assign: str_cat_lit(*s, "="); break;
        case tt_caret: str_cat_lit(*s, "^"); break;
        case tt_amp: str_cat_lit(*s, "&"); break;
        case tt_pipe: str_cat_lit(*s, "|"); break;
        case tt_tilde: str_cat_lit(*s, "~"); break;
        case tt_amp_amp: str_cat_lit(*s, "&&"); break;
        case tt_pipe_pipe: str_cat_lit(*s, "||"); break;
        case tt_lt_lt: str_cat_lit(*s, "<<"); break;
        case tt_gt_gt: str_cat_lit(*s, ">>"); break;
        case tt_slash: str_cat_lit(*s, "/"); break;
        case tt_per_cent: str_cat_lit(*s, "%%"); break;
        case tt_star: str_cat_lit(*s, "*"); break;
        case tt_plus: str_cat_lit(*s, "+"); break;
        case tt_minus: str_cat_lit(*s, "-"); break;
        case tt_plus_eq: str_cat_lit(*s, "+="); break;
        case tt_minus_eq: str_cat_lit(*s, "-="); break;
        case tt_star_eq: str_cat_lit(*s, "*="); break;
        case tt_slash_eq: str_cat_lit(*s, "/="); break;
        case tt_per_cent_eq: str_cat_lit(*s, "%%="); break;
        case tt_caret_eq: str_cat_lit(*s, "^="); break;
        case tt_amp_eq: str_cat_lit(*s, "&="); break;
        case tt_pipe_eq: str_cat_lit(*s, "|="); break;
        case tt_lt_lt_eq: str_cat_lit(*s, "<<="); break;
        case tt_gt_gt_eq: str_cat_lit(*s, ">>="); break;
        case tt_plus_plus: str_cat_lit(*s, "++"); break;
        case tt_minus_minus: str_cat_lit(*s, "--"); break;
        case tt_hash: str_cat_lit(*s, "#"); break;
        case tt_hash_hash: str_cat_lit(*s, "##"); break;
        case tt_semi: str_cat_lit(*s, ";"); break;
        case tt_comma: str_cat_lit(*s, ","); break;
        case tt_colon: str_cat_lit(*s, ":"); break;
        case tt_dot: str_cat_lit(*s, "."); break;
        case tt_ellipsis: str_cat_lit(*s, "..."); break;
        case tt_question: str_cat_lit(*s, "?"); break;
        case tt_arrow: str_cat_lit(*s, "->"); break;
        case tt_char:
            if (escape) pp_stringise_str(s, as_span(t->name), '\'');
            else {
                str_cat_char(*s, '\'');
                str_cat(*s, t->name);
                str_cat_char(*s, '\'');
            }
            break;

        case tt_string:
            if (escape) pp_stringise_str(s, as_span(t->name), '\"');
            else {
                str_cat_char(*s, '"');
                str_cat(*s, t->name);
                str_cat_char(*s, '"');
            }
            break;

#define kw(x) \
    case tt_kw_##x: str_cat_lit(*s, #x); break;
            ALL_KEYWORDS(kw)
#undef kw
    }
}

tok pp_stringise_tokens(const tokens *toks, loc l, bool whitespace_before) {
    tok str = {};
    str.loc = l;
    str.type = tt_string;
    str.whitespace_before = whitespace_before;
    bool first = true;
    vec_for(p, *toks) {
        if (first) first = false;
        else if (p->whitespace_before) str_cat_char(str.name, ' ');
        pp_stringise_token(&str.name, p, true);
    }
    return str;
}

bool pp_ts_done(token_stream s) {
    switch (s->kind) {
        case ts_lexer: return lex_eof(&s->lex);
        case ts_macro_expansion: return s->expansion.cursor == s->expansion.toks.size;
        case ts_toks: return s->toks.cursor == s->toks.toks.size;
    }

    die("unreachable");
}

void pp_ts_pop(pp pp) {
    auto ts = vec_pop(pp->token_streams);
    switch (ts.kind) {
        case ts_lexer: free(ts.lex.data); return;
        case ts_macro_expansion: ts.expansion.m->expanding = false; return;
        case ts_toks: vec_free(ts.toks.toks); return;
    }

    die("unreachable");
}

bool pp_undefine(pp pp, span macro_name) {
    size_t idx = vec_find_if_index(v, pp->defs, eq(v->name, macro_name));
    if (idx == NO_INDEX) return false;
    pp_free_macro(pp->defs.data + idx);
    vec_remove_index_unordered(pp->defs, idx);
    return true;
}

void pp_va_opt_reset(pp_expansion exp) {
    memset(&exp->va_opt, 0, sizeof(exp->va_opt));
    exp->va_opt.index = NO_INDEX;
}

// ====================================================================
//  Parser
// ====================================================================
typedef struct parser {
    pp pp;
} *parser;

// void parser_parse(parser p) {
// }

// ====================================================================
//  Entry
// ====================================================================
void print_escaped_name(tok *t) {
    putchar(t->type == tt_char ? '\'' : '\"');
    vec_for_val(c, t->name) {
        switch (c) {
            case '\n': printf("\\n"); break;
            case '\r': printf("\\r"); break;
            case '\t': printf("\\t"); break;
            case '\b': printf("\\b"); break;
            case '\f': printf("\\f"); break;
            case '\a': printf("\\a"); break;
            case '\v': printf("\\v"); break;
            case '\0': printf("\\0"); break;
            case '\'': printf("\\'"); break;
            case '\"': printf("\\\""); break;
            case '\\': printf("\\\\"); break;
            default: putchar(c); break;
        }
    }
    putchar(t->type == tt_char ? '\'' : '\"');
}

void dump_tokens(pp pp) {
    bool first = true;
    tok t = {};
    for (;;) {
        pp_read_token(pp, &t);
        if (t.type != tt_eof) break;
        if (first) first = false;
        else if (t.start_of_line) putchar('\n');
        else if (t.whitespace_before) putchar(' ');
        if (t.type == tt_char || t.type == tt_string) print_escaped_name(&t);
        else {
            string s = {};
            pp_stringise_token(&s, &t, false);
            printf("%.*s", (int) s.size, s.data);
        }
    }
    putchar('\n');
}

int main(int argc, char **argv) {
    if (argc < 2) die("usage: ./simplecc <file>");
    struct pp pp = {};
    pp_add_lexer(&pp, argv[1]);
    dump_tokens(&pp);

    // struct parser parser = {.pp = &pp};
    // parser_parse(&parser);

    pp_fini(&pp);
}
