// ====================================================================
//  Constants
// ====================================================================
#define ONE_MEGABYTE (1'024 * 1'024 * 1'024)
#define NOT_FOUND    ((size_t) -1)
#define noreturn     __attribute__((__noreturn__)) void

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

/// Ensure that there is space for at least (vector->size + elements) many elements.
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
    size_t _idx = NOT_FOUND;                       \
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
    _idx_ == NOT_FOUND ? nullptr : (vector).data + _idx_;           \
})

/// Remove all elements from a vector that match the condition. Additionally,
/// the predicate should take care of freeing any elements for which it returns
/// true as well.
#define vec_erase_if(element, vector, ...)                                       \
    do {                                                                         \
        size_t _first = vec_find_if_index(element, vector, __VA_ARGS__);         \
        if (_first != NOT_FOUND) {                                               \
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
    tt_eof,
    tt_pp_name,
    tt_pp_param, // Parameter in the replacement text of a macro.
    tt_pp_va_opt,
    tt_pp_va_args,
    tt_pp_placemarker,
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
    struct macro* m;
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

bool tok_has_str(tok *t) {
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

tok tok_copy(tok *t) {
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

typedef struct pp {
    tok tok;
    tokens look_ahead_tokens;
    vec(struct token_stream) token_streams;
    vec(struct macro) defs;
    strings filenames;
} *pp;

void pp_conv(pp pp);
noreturn pp_error(pp pp, const char *msg);
void pp_free_macro(macro m);
macro pp_get_macro_and_args(pp pp, tokens_vec *args, span name);
tok *pp_look_ahead(pp pp, size_t n);
void pp_materialise_look_ahead_toks(pp pp);
void pp_preprocess(pp pp);
void pp_read_token(pp pp);
void pp_read_token_raw(pp pp);
void pp_read_token_raw_impl(pp pp, bool include_look_ahead);
void pp_stringise(string *s, tok *t, bool escape);
void pp_stringise_tokens(string* s, tokens* toks);
bool pp_ts_done(token_stream t);
void pp_ts_pop(pp pp);
bool pp_undefine(pp pp, span name);

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

void pp_conv(pp pp) {
    if (pp->tok.type == tt_pp_name) {
        if (false) {}
#define kw(x) \
    else if (eq(pp->tok.name, lit_span(#x))) pp->tok.type = tt_kw_##x;
        ALL_KEYWORDS(kw)
#undef kw
    }
}

bool pp_is_param(tok *t) {
    return t->type == tt_pp_param || t->type == tt_pp_va_args || t->type == tt_pp_va_opt;
}

bool pp_dir_define_check_hash_impl(macro m, size_t cursor) {
    if (m->tokens.size == cursor) return false;
    return pp_is_param(&m->tokens.data[cursor]);
}

void pp_dir_define_check_hash(pp pp, macro m, size_t cursor) {
    if (!pp_dir_define_check_hash_impl(m, cursor))
        pp_error(pp, "expected parameter name after '#'");
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

tokens *pp_get_param_tokens(tokens *aux, macro m, tokens_vec *args, tok *t, size_t *cursor) {
    static tokens empty = {};
    bool have_variadic_args = args->size != m->params.size;
    switch (t->type) {
        default: return nullptr;
        case tt_pp_param: {
            (*cursor)++;
            auto idx = vec_find_if_index(p, m->params, eq(*p, t->name));
            assert(idx != NOT_FOUND);
            return &args->data[idx];
        }

        case tt_pp_va_args:
            (*cursor)++;
            return !have_variadic_args ? &empty : &vec_back(*args);

        case tt_pp_va_opt:
            vec_clear(*aux);
            (*cursor)++; // Skip '__VA_OPT__'.
            (*cursor)++; // Skip '('.

            if (have_variadic_args) {
                while (*cursor < t->val) vec_push(*aux, tok_copy(&m->tokens.data[(*cursor)++]));
            } else {
                *cursor = t->val;
            }

            (*cursor)++; // Skip ')'.
            return aux;
    }
}

tokens pp_expand(pp pp, macro m, tokens_vec *args) {
#define DIAG(...)                                                                               \
    do {                                                                                        \
        print_loc(pp->tok.loc);                                                                 \
        printf("fatal error in expansion of macro '%.*s': ", (int) m->name.size, m->name.data); \
        printf("  " __VA_ARGS__);                                                               \
        exit(1);                                                                                \
    } while (false)

    assert(args);
    if (m->tokens.size == 0) return (tokens){};
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
    } else {
        assert(!args->size && "passing arguments to object-like macro?");
    }

    m->expanding = true;
    tokens expansion = {};
    tokens aux = {};
    bool have_variadic_args = args->size != m->params.size;
    for (size_t cursor = 0; cursor < m->tokens.size;) {
        auto t = &m->tokens.data[cursor];

        // Stringising operator.
        if (t->type == tt_hash) {
            cursor++; // Yeet '#'.
            t = &m->tokens.data[cursor];
            tokens *param = pp_get_param_tokens(&aux, m, args, t, &cursor);
            assert(param && "'#' must be followed by parameter");

            // Stringise it.
            tok str = {};
            str.loc = t->loc;
            str.type = tt_string;
            str.whitespace_before = t->whitespace_before;
            pp_stringise_tokens(&str.name, param);
            vec_push(expansion, str);
        }

        // Token pasting operator.
        else if (t->type == tt_hash_hash) {
            tok placemarker = {};
            placemarker.loc = t->loc;
            placemarker.type = tt_pp_placemarker;
            placemarker.whitespace_before = t->whitespace_before;

            cursor++; // Yeet '##'.
            auto before = &expansion.data[expansion.size - 1];
            tok *after = nullptr;

            // Get the next token from the parameter if there is one.
            auto after_param = pp_get_param_tokens(&aux, m, args, &m->tokens.data[cursor], &cursor);
            if (after_param) before = after_param->size ? &vec_front(*after_param) : &placemarker;
            else after = &m->tokens.data[cursor];

            // Concatenate the tokens.
            //
            // Yes, the sane way of doing this is to actually just use the lexer; that’s
            // also what Clang does.
            if (before->type == tt_pp_placemarker) vec_push(expansion, tok_copy(after));
            else if (after->type == tt_pp_placemarker) vec_push(expansion, tok_copy(before));
            else {
                string concat = {};
                pp_stringise(&concat, before, false);
                pp_stringise(&concat, after, false);
                struct lexer l = {};
                l.loc = t->loc;
                l.char_ptr = concat.data;
                l.end_ptr = concat.data + concat.size;
                l.c = ' ';
                tok tmp = {};
                t->type = lex(&l, &tmp);
                if (!lex_eof(&l)) pp_error(pp, "token pasting did not produce a valid pp-token");
                tmp.loc = t->loc;
                tmp.whitespace_before = t->whitespace_before;
                tmp.start_of_line = t->start_of_line;
                vec_push(expansion, tmp);
            }
        }

        // Parameter.
        else if (pp_is_param(t)) {
            auto param = pp_get_param_tokens(&aux, m, args, t, &cursor);

            // We need to concatenate this with other tokens; push them into
            // the expansion and handle this in the next iteration.
            if (cursor < m->tokens.size && m->tokens.data[cursor].type == tt_hash_hash) {
                vec_for(p, *param) vec_push(expansion, tok_copy(p));
                continue;
            }

            // For __VA_OPT__, we’re supposed to insert a placemarker if there
            // are no variadic arguments.
            if (t->type == tt_pp_va_opt && !have_variadic_args) {
                tok placemarker = {};
                placemarker.loc = t->loc;
                placemarker.type = tt_pp_placemarker;
                placemarker.whitespace_before = t->whitespace_before;
                vec_push(expansion, placemarker);
                continue;
            }

            // If we get here, we can expand the tokens.
            tokens param_subst = {};
            vec_for(p, *param) {
                if (p->type != tt_pp_name) {
                    vec_push(param_subst, tok_copy(p));
                    continue;
                }

                tokens_vec nested_args = {};
                auto nested = pp_get_macro_and_args(pp, &nested_args, as_span(p->name));
                if (!nested || nested->expanding) vec_push(param_subst, tok_copy(p));
                else {
                    auto expanded = pp_expand(pp, nested, &nested_args);
                    vec_append(param_subst, expanded);
                    vec_free(expanded);
                }
            }

            vec_append(expansion, param_subst);
            vec_free(param_subst);
        }

        // Other random tokens are appended as-is. Macros in the replacement list are
        // not expanded here, but rather during the rescan phase after this loop.
        else {
            cursor++;
            vec_push(expansion, tok_copy(t));
        }
    }

    // Next, remove all placemarkers.
    vec_erase_if(t, expansion, t->type == tt_pp_placemarker);

    string s = {};
    pp_stringise_tokens(&s, &expansion);
    /*printf("expanded %.*s to ", (int) m->name.size, m->name.data);
    printf("%.*s\n", (int) s.size, s.data);*/
    vec_free(s);

    // At this point, we’re supposed to do a rescan, but we’ll do that as part of
    // processing this token sequence.
    return expansion;
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

            case tt_comma:
                // If we already have enough arguments, and the macro is variadic, then all
                // remaining arguments are grouped into a single argument that is bound to
                // __VA_ARGS__.
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
}

macro pp_get_macro_and_args(pp pp, tokens_vec *args, span name) {
    auto macro = vec_find_if(m, pp->defs, eq(m->name, name));
    if (!macro || macro->expanding) return nullptr;
    if (!macro->is_function_like) return macro;
    if (pp_look_ahead(pp, 1)->type != tt_lparen) return nullptr;
    pp_read_token_raw(pp);
    pp_read_token_raw(pp);
    if (pp->tok.type != tt_rparen) pp_get_macro_args(pp, macro, args);
    pp_read_token_raw(pp);
    return macro;
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
    vec_push(pp->token_streams, (struct token_stream){
        .kind = ts_toks,
        .toks = (token_buffer) {
            .toks = pp->look_ahead_tokens,
            .cursor = 0,
        },
    });
    pp->look_ahead_tokens = (tokens){};
}

void pp_preprocess(pp pp) {
    while (pp->tok.type == tt_hash) {
        if (!pp->tok.start_of_line) pp_error(pp, "unexpected #");
        pp_read_token_raw(pp);

        // TODO: handle the GNU '#' directive.
        if (pp->tok.type != tt_pp_name)
            pp_error(pp, "expected preprocessor directive");

        if (eq(pp->tok.name, lit_span("define"))) pp_dir_define(pp);
        else if (eq(pp->tok.name, lit_span("undef"))) pp_dir_undef(pp);
        else pp_error(pp, "unknown preprocessor directive");
    }

    if (pp->tok.type == tt_pp_name) {
        tokens_vec args = {};
        macro m = pp_get_macro_and_args(pp, &args, as_span(pp->tok.name));
        if (m && !m->expanding) {
            auto expansion = pp_expand(pp, m, &args);
            if (expansion.size) {
                auto first = &vec_front(expansion);
                first->start_of_line = pp->tok.start_of_line;
                first->whitespace_before = pp->tok.whitespace_before;

                pp_materialise_look_ahead_toks(pp);
                vec_push(pp->token_streams, (struct token_stream){
                    .kind = ts_macro_expansion,
                    .expansion = (macro_expansion){
                        .m = m,
                        .toks = expansion,
                        .cursor = 0,
                    },
                });
            }

            pp_read_token_raw(pp);
            return pp_preprocess(pp);
        }
    }

    // TODO: Concatenate adjacent string literal tokens here.
}

void pp_read_token(pp pp) {
    pp_read_token_raw(pp);
    pp_preprocess(pp);
    pp_conv(pp);
}

void pp_read_token_raw(pp pp) {
    pp_read_token_raw_impl(pp, true);
}

void pp_read_token_raw_impl(pp pp, bool include_look_ahead) {
    if (include_look_ahead && pp->look_ahead_tokens.size) {
        tok_move_into(&pp->tok, &vec_front(pp->look_ahead_tokens));
        vec_pop_front(pp->look_ahead_tokens);
        return;
    }

    while (pp->token_streams.size && pp_ts_done(&vec_back(pp->token_streams)))
        pp_ts_pop(pp);

    if (!pp->token_streams.size) {
        pp->tok.type = tt_eof;
        return;
    }

    auto ts = &vec_back(pp->token_streams);
    switch (ts->kind) {
        case ts_lexer: pp->tok.type = lex(&ts->lex, &pp->tok); break;
        case ts_macro_expansion:
            tok_move_into(&pp->tok, &ts->expansion.toks.data[ts->expansion.cursor++]);
            break;
        case ts_toks:
            tok_move_into(&pp->tok, &ts->toks.toks.data[ts->toks.cursor++]);
            break;
    }

    if (pp->tok.type == tt_eof)
        pp_read_token_raw_impl(pp, include_look_ahead);
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

void pp_stringise(string *s, tok *t, bool escape) {
    switch (t->type) {
        case tt_eof: str_cat_lit(*s, "<EOF>"); break;
        case tt_pp_va_args: str_cat_lit(*s, "__VA_ARGS__"); break;
        case tt_pp_va_opt: str_cat_lit(*s, "__VA_OPT__"); break;
        case tt_pp_param: assert(false && "should have been replaced already"); break;
        case tt_pp_placemarker: break;
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

void pp_stringise_tokens(string* s, tokens* toks) {
    bool first = true;
    vec_for(p, *toks) {
        if (first) first = false;
        else if (p->whitespace_before) str_cat_char(*s, ' ');
        pp_stringise(s, p, true);
    }
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
    if (idx == NOT_FOUND) return false;
    pp_free_macro(pp->defs.data + idx);
    vec_remove_index_unordered(pp->defs, idx);
    return true;
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
    while (pp->tok.type != tt_eof) {
        if (first) first = false;
        else if (pp->tok.start_of_line) putchar('\n');
        else if (pp->tok.whitespace_before) putchar(' ');
        if (pp->tok.type == tt_char || pp->tok.type == tt_string) print_escaped_name(&pp->tok);
        else {
            string s = {};
            pp_stringise(&s, &pp->tok, false);
            printf("%.*s", (int) s.size, s.data);
        }
        pp_read_token(pp);
    }
    putchar('\n');
}

int main(int argc, char **argv) {
    if (argc < 2) die("usage: ./simplecc <file>");
    struct pp pp = {};
    pp_add_lexer(&pp, argv[1]);
    pp_read_token(&pp);
    dump_tokens(&pp);

    // struct parser parser = {.pp = &pp};
    // parser_parse(&parser);

    pp_fini(&pp);
}
