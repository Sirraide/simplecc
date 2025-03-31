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
        typeof(vector) _copy = (vector);                                          \
        _copy.data = malloc(_copy.size * sizeof *(vector).data);                  \
        memcpy(_copy.data, (vector).data, (vector).size * sizeof *(vector).data); \
        _copy;                                                                    \
    })

/// Iterate over a vector by reference.
#define vec_for(element, vector) \
    for (typeof(*(vector).data) *element = (vector).data; element < (vector).data + (vector).size; element++)

/// Iterate over a vector of pointers by value.
#define vec_for_val(element, vector)                                                                             \
    for (typeof(*(vector).data) *element##_ptr = (vector).data, element;                                         \
         element##_ptr < (vector).data + (vector).size && (element = *element##_ptr, true); /* "=", not "=="! */ \
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
        usz _sz = sz;                                   \
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
        if (_index < (vector).size) vec_remove_unordered(vector, _index);                    \
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
    size_t _idx = (size_t) -1;                     \
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
    _idx_ == (size_t) -1 ? nullptr : (vector).data + _idx_;         \
})

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

#define str_copy(s) vec_copy(s)

#define eq(_s1, _s2) ({                                          \
    auto s1 = (_s1);                                             \
    auto s2 = (_s2);                                             \
    s1.size == s2.size&& memcmp(s1.data, s2.data, s1.size) == 0; \
})

// ====================================================================
//  Lexer
// ====================================================================
typedef struct lexer {
    loc loc;
    char *data;
    const char *char_ptr;
    const char *end_ptr;
    char c;
    struct lexer *prev;
    bool start_of_line;
    bool whitespace_before;
} *lexer;

typedef enum {
    tt_eof,
    tt_pp_name,
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
    string name;
    u64 val;
    bool start_of_line;
    bool whitespace_before;
} tok;

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
    t->val = 0;
    vec_for_val(c, t->name) {
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

tok tok_copy(tok *t) {
    tok copy = *t;
    if (
        t->type == tt_string ||
        t->type == tt_char ||
        t->type == tt_pp_name
    ) {
        copy.name = str_copy(t->name);
    } else {
        copy.name = (string) {};
    }
    return copy;
}

// ====================================================================
//  Preprocessor
// ====================================================================
typedef vec(tok) tokens;
typedef vec(string) strings;

typedef struct macro {
    string name;
    tokens tokens;
    strings args;
    bool is_variadic;
    bool is_function_like;
} *macro;

typedef struct pp {
    lexer lex;
    tok tok;
    vec(struct macro) defs;
    strings filenames;
} *pp;

void pp_conv(pp pp);
noreturn pp_error(pp pp, const char *msg);
void pp_free_macro(macro m);
void pp_read_token(pp pp);
void pp_read_token_raw(pp pp);
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

    lexer l = calloc(sizeof *l, 1);
    l->loc.file = as_span(vec_back(pp->filenames));
    l->loc.line = 1;
    l->data = buffer;
    l->char_ptr = buffer;
    l->end_ptr = buffer + (size_t) n_read;
    l->prev = pp->lex;
    l->c = '\n'; // to set the start of line flag
    pp->lex = l;
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
                if (vec_find_if(arg, m.args, eq(*arg, pp->tok.name)))
                    pp_error(pp, "duplicate macro argument");

                vec_push(m.args, str_copy(pp->tok.name));
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
        vec_push(m.tokens, tok_copy(&pp->tok));
        pp_read_token_raw(pp);
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

void pp_lex_fini(pp pp) {
    lexer l = pp->lex;
    pp->lex = l->prev;
    free(l->data);
    free(l);
}

void pp_read_token(pp pp) {
    pp_read_token_raw(pp);

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

    pp_conv(pp);
}

void pp_read_token_raw(pp pp) {
    while (pp->lex && lex_eof(pp->lex))
        pp_lex_fini(pp);

    if (!pp->lex) {
        pp->tok.type = tt_eof;
        return;
    }

    pp->tok.type = lex(pp->lex, &pp->tok);
    if (pp->tok.type == tt_eof)
        pp_read_token_raw(pp);
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
        switch (pp->tok.type) {
            case tt_eof: die("unreachable");
            case tt_pp_name: printf("%.*s", (int) pp->tok.name.size, pp->tok.name.data); break;
            case tt_int_lit: printf("%zu", pp->tok.val); break;
            case tt_char: print_escaped_name(&pp->tok); break;
            case tt_string: print_escaped_name(&pp->tok); break;
            case tt_lparen: printf("("); break;
            case tt_rparen: printf(")"); break;
            case tt_lbrace: printf("{"); break;
            case tt_rbrace: printf("}"); break;
            case tt_lbrack: printf("["); break;
            case tt_rbrack: printf("]"); break;
            case tt_gt: printf(">"); break;
            case tt_gt_eq: printf(">="); break;
            case tt_lt: printf("<"); break;
            case tt_lt_eq: printf("<="); break;
            case tt_eq_eq: printf("=="); break;
            case tt_bang_eq: printf("!="); break;
            case tt_bang: printf("!"); break;
            case tt_assign: printf("="); break;
            case tt_caret: printf("^"); break;
            case tt_amp: printf("&"); break;
            case tt_pipe: printf("|"); break;
            case tt_tilde: printf("~"); break;
            case tt_amp_amp: printf("&&"); break;
            case tt_pipe_pipe: printf("||"); break;
            case tt_lt_lt: printf("<<"); break;
            case tt_gt_gt: printf(">>"); break;
            case tt_slash: printf("/"); break;
            case tt_per_cent: printf("%%"); break;
            case tt_star: printf("*"); break;
            case tt_plus: printf("+"); break;
            case tt_minus: printf("-"); break;
            case tt_plus_eq: printf("+="); break;
            case tt_minus_eq: printf("-="); break;
            case tt_star_eq: printf("*="); break;
            case tt_slash_eq: printf("/="); break;
            case tt_per_cent_eq: printf("%%="); break;
            case tt_caret_eq: printf("^="); break;
            case tt_amp_eq: printf("&="); break;
            case tt_pipe_eq: printf("|="); break;
            case tt_lt_lt_eq: printf("<<="); break;
            case tt_gt_gt_eq: printf(">>="); break;
            case tt_plus_plus: printf("++"); break;
            case tt_minus_minus: printf("--"); break;
            case tt_hash: printf("#"); break;
            case tt_hash_hash: printf("##"); break;
            case tt_semi: printf(";"); break;
            case tt_comma: printf(","); break;
            case tt_colon: printf(":"); break;
            case tt_dot: printf("."); break;
            case tt_ellipsis: printf("..."); break;
            case tt_question: printf("?"); break;
            case tt_arrow: printf("->"); break;
#define kw(x) \
    case tt_kw_##x: printf(#x); break;
                ALL_KEYWORDS(kw)
#undef kw
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
