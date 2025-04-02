#include "platform.h"
#include "vector.h"

// ====================================================================
//  Constants
// ====================================================================
#define ONE_MEGABYTE (1'024 * 1'024 * 1'024)
#define NO_INDEX     ((size_t) -1)

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
#define str_cat(s1, ...)   vec_append(s1, __VA_ARGS__)
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
    u32 pp_if_depth;
    loc loc;
    char *data;
    const char *char_ptr;
    const char *end_ptr;
    char c;
    bool start_of_line;
    bool whitespace_before;
} *lexer;

typedef struct token_buffer {
    struct macro *m; ///< May be null.
    tokens toks;
    size_t cursor;
    bool keep_when_empty;
} token_buffer;

typedef struct token_stream {
    enum token_stream_kind {
        ts_lexer,
        ts_toks,
    } kind;

    union {
        struct lexer lex;
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
    printf("error: %s\n", msg);
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
    t->loc = l->loc;

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
            vec_push(t->name, c);
            if (c == '0') lex_number(l, t, is_octal_or_sep, 8);
            else if (lex_eat(l, 'x') || lex_eat(l, 'X')) lex_number(l, t, is_hex_or_sep, 16);
            else if (lex_eat(l, 'b') || lex_eat(l, 'B')) lex_number(l, t, is_bin_or_sep, 2);
            else lex_number(l, t, is_decimal_or_sep, 10);
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
    struct pp *pp;
    pp_va_opt va_opt;
    size_t cursor;            ///< The index of the token in the replacement list we’re processing.
    tokens_vec *args;         ///< The arguments bound to each parameter + one more for __VA_ARGS__.
    tokens_vec expanded_args; ///< The fully macro-replaced arguments. One for each argument.
    tokens expansion;         ///< The resulting expansion of this macro.
    macro m;                  ///< The macro being expanded.
    loc l;                    ///< Expansion location.
} *pp_expansion;

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
macro pp_get_expandable_macro(pp pp, span name);
macro pp_get_macro_and_args(pp pp, tokens_vec *args, span name);
tok *pp_look_ahead(pp pp, size_t n);
void pp_enter_token_stream(pp pp, tokens *toks, macro m);
void pp_materialise_look_ahead_toks(pp pp);
void pp_preprocess(pp pp);
void pp_read_token(pp pp);
void pp_read_and_expand_token(pp pp);
void pp_read_token_raw(pp pp);
void pp_read_token_raw_impl(pp pp, bool include_look_ahead);
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
    vec_push(
        pp->token_streams,
        (struct token_stream) {
            .kind = ts_lexer,
            .lex = l,
        }
    );
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

bool pp_is_param(const tok *t) {
    return t->type == tt_pp_param || t->type == tt_pp_va_args;
}

bool pp_dir_define_check_hash_impl(macro m, size_t cursor) {
    if (m->tokens.size == cursor) return false;
    auto t = &m->tokens.data[cursor];
    return pp_is_param(t) || t->type == tt_pp_va_opt;
}

void pp_dir_define_check_hash(pp pp, macro m, size_t cursor) {
    if (m->is_function_like && !pp_dir_define_check_hash_impl(m, cursor))
        pp_error(pp, "expected parameter name or '__VA_OPT__' after '#'");
}

void pp_dir_define_check_hash_hash(pp pp, macro m, size_t cursor) {
    if (cursor == 1 || m->tokens.size == cursor)
        pp_error(pp, "'##' must not occur at the start or end of a macro definition");
}

size_t pp_find_va_opt_rparen(pp pp, macro m, size_t *cursor) {
    if (
        *cursor == m->tokens.size ||
        m->tokens.data[*cursor].type != tt_lparen
    ) pp_error(pp, "expected '(' after '__VA_OPT__'");
    (*cursor)++; // Yeet '('.

    if (
        *cursor != m->tokens.size &&
        m->tokens.data[*cursor].type == tt_hash_hash
    ) pp_error(pp, "'##' cannot be the first token inside '__VA_OPT__'");

    size_t parens = 1;
    while (*cursor < m->tokens.size) {
        auto t = &m->tokens.data[(*cursor)++];
        switch (t->type) {
            default: break;
            case tt_hash:
                pp_dir_define_check_hash(pp, m, *cursor);
                break;

            case tt_hash_hash:
                pp_dir_define_check_hash_hash(pp, m, *cursor);
                if (
                    parens == 1 &&
                    *cursor != m->tokens.size &&
                    m->tokens.data[*cursor].type == tt_rparen
                ) pp_error(pp, "'##' cannot be the last token inside '__VA_OPT__'");
                break;

            case tt_lparen:
                parens++;
                break;

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

        // Prohibit '####' since the standard isn’t clear what it’s supposed to mean, and vendors
        // can’t seem to agree on how it should actually be handled.
        if (pp->tok.type == tt_hash_hash && m.tokens.size != 0 && vec_back(m.tokens).type == tt_hash_hash)
            pp_error(pp, "'##' cannot be followed by another '##'");

        vec_push(m.tokens, tok_move(&pp->tok));
        pp_read_token_raw(pp);
    }

    for (size_t cursor = 0; cursor < m.tokens.size;) {
        auto t = &m.tokens.data[cursor++];
        if (t->type == tt_hash) pp_dir_define_check_hash(pp, &m, cursor);
        else if (t->type == tt_hash_hash) pp_dir_define_check_hash_hash(pp, &m, cursor);
        else if (t->type == tt_pp_va_opt) t->val = pp_find_va_opt_rparen(pp, &m, &cursor);
    }

    pp_undefine(pp, as_span(m.name));
    vec_push(pp->defs, m);
}

void pp_skip_line_raw(pp pp) {
    do pp_read_token_raw(pp);
    while (pp->tok.type != tt_eof && !pp->tok.start_of_line);
}

void pp_dir_undef(pp pp) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_pp_name || pp->tok.start_of_line)
        pp_error(pp, "expected macro name");

    if (!pp_undefine(pp, as_span(pp->tok.name))) {
        print_loc(pp->tok.loc);
        printf("warning: macro '%.*s' is not defined\n", (int) pp->tok.name.size, pp->tok.name.data);
    }

    pp_skip_line_raw(pp);
}

bool pp_lexing_file(pp pp) {
    return pp->token_streams.size && vec_back(pp->token_streams).kind == ts_lexer;
}

lexer pp_lexer(pp pp) {
    assert(pp_lexing_file(pp));
    return &vec_back(pp->token_streams).lex;
}

void pp_dir_ifdef(pp pp, bool ifdef) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_pp_name || pp->tok.start_of_line)
        pp_error(pp, "expected macro name");

    bool defined = vec_find_if(p, pp->defs, eq(p->name, pp->tok.name)) != nullptr;
    if (defined == ifdef) {
        pp_lexer(pp)->pp_if_depth++;
        pp_skip_line_raw(pp);
        return;
    }

    for (;;) {
        pp_skip_line_raw(pp);
        if (pp->tok.type == tt_eof) pp_error(pp, "unterminated #if(n)def");
        if (pp->tok.type == tt_hash && pp_lexing_file(pp)) {
            pp_read_token_raw(pp);
            if (pp->tok.type == tt_pp_name && eq(pp->tok.name, lit_span("endif"))) {
                pp_skip_line_raw(pp);
                return;
            }
        }
    }
}

void pp_dir_endif(pp pp) {
    if (!pp_lexing_file(pp) || !pp_lexer(pp)->pp_if_depth)
        pp_error(pp, "unexpected #endif");

    pp_lexer(pp)->pp_if_depth--;
    pp_skip_line_raw(pp);
}

void pp_dir_include(pp pp) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_string || pp->tok.start_of_line)
        pp_error(pp, "expected include path; only string literals are supported for now");

    string s = {};
    str_cat(s, pp->tok.loc.file);
    str_cat_char(s, 0);
    auto resolved = realpath(s.data, nullptr);
    if (!resolved) {
        print_loc(pp->tok.loc);
        perror("failed to resolve path");
        exit(1);
    }

    auto len = strlen(resolved);
    if (len != 1) {
        auto parent = strrchr(resolved, '/');
        if (parent) len = (size_t)(parent - resolved);
    }

    vec_clear(s);
    str_cat(s, (span){.data = resolved, .size = len});
    str_cat_char(s, '/');
    str_cat(s, pp->tok.name);
    str_cat_char(s, 0);
    pp_add_lexer(pp, s.data);
    pp_skip_line_raw(pp);
    vec_free(s);
    free(resolved);
}

noreturn pp_error(pp pp, const char *msg) {
    print_loc(pp->tok.loc);
    printf("error: %s\n", msg);
    exit(1);
}

const tok *pp_cur(pp_expansion exp) {
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

/// This implements C2y 6.10.5.2 'Argument substitution'.
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
    pp_enter_token_stream(pp, arg_toks, nullptr);
    vec_back(pp->token_streams).toks.keep_when_empty = true;
    for (;;) {
        pp_read_and_expand_token(pp);
        if (pp->tok.type == tt_eof) break;
        vec_push(*expanded_arg, tok_move(&pp->tok));
    }
    pp_ts_pop(pp);

    // Insert a space before the first token.
    if (expanded_arg->size) vec_front(*expanded_arg).whitespace_before = true;
    return expanded_arg;
}

void pp_error_at(loc l, const char *err) {
    print_loc(l);
    printf("error: %s\n", err);
    exit(1);
}

void pp_paste(pp_expansion exp, const tok *t) {
    tok *before = &vec_back(exp->expansion);

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

    if (!lex_eof(&l)) pp_error_at(exp->l, "token pasting did not produce a valid pp-token");
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

void pp_enter_token_stream(pp pp, tokens *toks, macro m) { // clang-format off
    if (m) m->expanding = true;
    pp_materialise_look_ahead_toks(pp);
    tokens copy = {};
    vec_for(t, *toks) vec_push(copy, tok_copy(t));
    vec_push(pp->token_streams, (struct token_stream) {
        .kind = ts_toks,
        .toks = {
            .m = m,
            .toks = copy,
            .cursor = 0,
            .keep_when_empty = false,
        },
    });
} // clang-format on

void pp_defer_stringise_va_opt(pp_expansion exp, const tok *hash) {
    assert(!exp->va_opt.stringise && "va_opt not reset");
    exp->va_opt.stringise = true;
    exp->va_opt.stringise_loc = hash->loc;
    exp->va_opt.stringise_whitespace_before = hash->whitespace_before;
}

bool pp_has_variadic_args(pp_expansion exp) {
    // We treat __VA_ARGS__ as just another parameter, so its expansion can be
    // retrieved in much the same manner.
    auto param = pp_substitute(exp, exp->m->params.size);
    return param->size != 0;
}

void pp_enter_va_opt(pp_expansion exp, const tok *va_opt_token) {
    exp->va_opt.index = exp->cursor;
    exp->va_opt.rparen_index = va_opt_token->val;
    exp->va_opt.start_of_expansion = exp->expansion.size;
    exp->cursor++; // Yeet '__VA_OPT__'.
    exp->cursor++; // Yeet '('.

    // If __VA_ARGS__ expands to nothing, discard everything up
    // to, but NOT including, the closing rparen.
    if (!pp_has_variadic_args(exp)) {
        while (exp->cursor != exp->va_opt.rparen_index)
            exp->cursor++;

        // Per C2y 6.10.5.2p7, the __VA_OPT__ parameter expands to
        // a single placemarker in this case.
        exp->va_opt.ends_with_placemarker = true;
    }
}

bool pp_in_va_opt(pp_expansion exp) {
    return exp->va_opt.index != NO_INDEX;
}

void pp_placemarker(pp_expansion exp) {
    // ‘Insert’ a ‘placemarker token’, i.e. consume any '##' that
    // follows or record that we need to consume a '##' if we’re at
    // the end of a __VA_OPT__ replacement.
    //
    // This assumes that we’re looking *at* the '##' or ')'.
    if (exp->cursor < exp->m->tokens.size && pp_cur(exp)->type == tt_hash_hash)
        exp->cursor++;
    else if (pp_in_va_opt(exp) && exp->cursor == exp->va_opt.rparen_index)
        exp->va_opt.ends_with_placemarker = true;
}

void pp_expand_function_like_impl(pp_expansion exp) {
    pp_va_opt_reset(exp);

    // Process the replacement token list, performing argument substitution,
    // evaluation of '#' and '##', and handling of __VA_OPT__.
    //
    // The wording for this in the C standard, as ever, is rather obscure, and
    // doesn’t exactly lend itself well to direct implementation, for which reason
    // this is largely adapted from Clang’s lexer instead. Roughly, this is how
    // each of these features is implemented.
    //
    // The actual hard part here is token pasting, and the implementation of this
    // feature is spread all throughout the loop, so we explain it here:
    //
    // According to the standard, if either argument of '##' is a parameter (which
    // includes __VA_ARGS__ and '__VA_OPT__(...)') whose corresponding argument is
    // ‘empty’—i.e. the user provided no tokens for it, or, in the case of __VA_OPT__,
    // either '__VA_ARGS__' expands to nothing, or we have '__VA_OPT__()' with no
    // tokens inside the '()'—that parameter is replaced with a ‘placemarker token’.
    //
    // Given the sequence 'A##B':
    //
    //   - If neither is a placemarker, we perform token pasting.
    //   - If 'A' is a placemarker, the result is 'B'.
    //   - If 'B' is a placemarker, the result is 'A'.
    //   - If both are placemarkers, the result is a placemarker.
    //
    // Note that an equivalent, definition without placemarker tokens is:
    //
    //   - If neither is empty, we perform token pasting.
    //   - If 'A' is empty, we discard 'A##'.
    //   - If 'B' is empty, we discard '##B'.
    //   - If both are empty, we discard 'A##B'. (*)
    //
    // (*) There is one more part to this last point: consider 'A##B##C'. First, it
    // should be noted that the order of the two is unspecified. We choose to evaluate
    // them left to right. If 'A' and 'B' are empty, the result is '##C', but more
    // accurately, it is '<placemarker>##C', i.e. the '##' after 'B' must be discarded
    // as well.
    //
    // Thus, it suffices to correctly discard '##'s whenever the standard would output
    // a placemarker token.
    //
    // Note that we currently ban '####' for reasons described in the '#define' parsing
    // code, which means that consecutive placemarker tokens are equivalent to a single
    // placemarker.
    while (exp->cursor < exp->m->tokens.size) {
        auto t = pp_cur(exp);

        // __VA_OPT__
        //
        // Skip '__VA_OPT__(' and mark that we're inside of __VA_OPT__.
        if (t->type == tt_pp_va_opt) {
            pp_enter_va_opt(exp, t);
            t = pp_cur(exp);
        }

        // We’re inside of __VA_OPT__ and just encountered the closing parenthesis.
        if (pp_in_va_opt(exp) && exp->cursor == exp->va_opt.rparen_index) {
            exp->cursor++; // Yeet it.
            assert(
                (!exp->va_opt.paste_tokens || exp->va_opt.stringise) &&
                "only paste tokens here if we’re also stringising"
            );

            // Perform stringising now if we need to. This needs to be done before
            // token pasting should that also be required. Note that this involves
            // stringising all the tokens produced by the __VA_OPT__.
            if (exp->va_opt.stringise) {
                tokens toks = {};
                toks.data = exp->expansion.data + exp->va_opt.start_of_expansion;
                toks.size = exp->expansion.size - exp->va_opt.start_of_expansion;
                tok s = pp_stringise_tokens(
                    &toks,
                    exp->va_opt.stringise_loc,
                    exp->va_opt.stringise_whitespace_before
                );

                // This is 'A###__VA_OPT__(...)'
                //
                // Paste the string literal if need be.
                if (exp->va_opt.paste_tokens) {
                    pp_paste(exp, &s);
                    tok_free(&s);
                } else {
                    vec_push(exp->expansion, s);
                }
            }

            // We have a placemarker here if the last token of '...' in '__VA_OPT__(...)' is a
            // placemarker, if '...' contains no tokens, or if we have no variadic arguments.
            bool expands_to_nothing = exp->expansion.size == exp->va_opt.start_of_expansion;
            if (exp->va_opt.ends_with_placemarker || expands_to_nothing) pp_placemarker(exp);
            pp_va_opt_reset(exp);
            continue;
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

            exp->cursor++; // Yeet parameter.
            vec_push(exp->expansion, pp_stringise(exp, t));
            continue;
        }

        // Token pasting operator. If 'A' is a placemarker, we will already have discarded
        // any '##' that follows, so if we get here, 'A' is not a placemarker, and we only
        // need to check 'B'.
        if (t->type == tt_hash_hash) {
            exp->cursor++; // Yeet '##'.
            t = pp_cur(exp);

            // We disallow this in the definition.
            assert(t->type != tt_hash_hash && "'####' should have been diagnosed");

            // The next token is __VA_OPT__. If it is __VA_OPT__ is non-empty, we paste with
            // the first token in its token list.
            t = pp_cur(exp);
            if (t->type == tt_pp_va_opt) {
                pp_enter_va_opt(exp, t);

                // If the '__VA_OPT__(...)' consists of exactly one placemarker, discard it; exit
                // the __VA_OPT__ context too in that case since there is no other action to take,
                // and doing so avoids inserting two placemarkers here.
                if (exp->cursor == exp->va_opt.rparen_index) {
                    exp->cursor++; // Yeet the closing parenthesis.
                    pp_va_opt_reset(exp);
                    continue;
                }

                // The next token can’t be '##' since '__VA_OPT__(##' is invalid; it also can’t
                // be another __VA_OPT__ since they can’t be nested. This means the first token
                // of __VA_OPT__ must fall into one of the other cases, so simply fall through
                // here and handle it below.
                t = pp_cur(exp);
            }

            // The next token is '#'.
            if (t->type == tt_hash) {
                exp->cursor++; // Yeet '#'.
                auto hash = t;
                t = pp_cur(exp);

                // If the parameter of '#' is a __VA_OPT__ replacement, then we’ll defer the
                // pasting (and stringising) until we've processed the __VA_OPT__.
                if (t->type == tt_pp_va_opt) {
                    exp->va_opt.paste_tokens = true;
                    pp_defer_stringise_va_opt(exp, hash);
                    continue;
                }

                // Otherwise, stringise, which never produces a placemarker, then paste.
                exp->cursor++; // Yeet the parameter after '#'.
                tok str = pp_stringise(exp, t);
                pp_paste(exp, &str);
                tok_free(&str);
                continue;
            }

            // The next token is a regular (= non-parameter) token that is not a preprocessor
            // operator. This is never a placemarker.
            if (!pp_is_param(t)) {
                exp->cursor++; // Yeet the token.
                pp_paste(exp, t);
                continue;
            }

            // The next token is a parameter (other than '__VA_OPT__(...)').
            exp->cursor++; // Yeet the parameter.
            auto toks = pp_get_param_tokens(exp, t);

            // The parameter expands to a placemarker.
            if (toks->size == 0) {
                pp_placemarker(exp);
                continue;
            }

            // Paste with the first token of the parameter and append any other tokens as-is.
            pp_paste(exp, &vec_front(*toks));
            vec_for_index(i, *toks) if (i >= 1)
                vec_push(exp->expansion, tok_copy(&toks->data[i]));
            continue;
        }

        // We’re done with the pasting cases. At this point, if the next token is not a
        // parameter, it must be a regular, non-special token.
        if (!pp_is_param(t)) {
            exp->cursor++;
            vec_push(exp->expansion, tok_copy(t));
            continue;
        }

        // Otherwise, we’re looking at a parameter (other than '__VA_OPT__(...)'). Yeet it.
        exp->cursor++;

        // If the next token is '##', we must check for placemarkers here, and in any
        // case, we need to append the argument tokens as-is.
        if (exp->cursor < exp->m->tokens.size && pp_cur(exp)->type == tt_hash_hash) {
            auto param = pp_get_param_tokens(exp, t);
            if (param->size == 0) pp_placemarker(exp);
            else { vec_for(p, *param) vec_push(exp->expansion, tok_copy(p)); }
            continue;
        }

        // Finally, if we get here, we have a parameter and there is no paste in sight.
        // in this case, the standard requires that we fully expand it before appending
        // it to the expansion (this does not happen for operands of '#' or '##').
        auto toks = pp_substitute(exp, pp_get_param_index(exp, t));
        vec_for(p, *toks) {
            vec_push(exp->expansion, tok_copy(p));
            if (t->whitespace_before && p == &vec_front(*toks))
                vec_back(exp->expansion).whitespace_before = true;
        }

        // Insert a placemarker here too (this is actually only relevant in an edge case,
        // namely if this parameter is the A in '__VA_OPT__(...A)##B').
        if (toks->size == 0) pp_placemarker(exp);
    }
}

void pp_fini_expansion(pp_expansion exp, bool start_of_line) {
    if (exp->expansion.size) {
        auto first = &vec_front(exp->expansion);
        first->start_of_line = start_of_line;
        first->whitespace_before = true;
        pp_enter_token_stream(exp->pp, &exp->expansion, exp->m);
    }

    vec_delete_els(t, exp->expansion) tok_free(t);
    vec_delete_els(arg, exp->expanded_args)
        vec_delete_els(t, *arg)
            tok_free(t);
}

void pp_expand_function_like(pp pp, macro m, tokens_vec *args, loc l, bool start_of_line) {
#define DIAG(...)                                                                         \
    do {                                                                                  \
        print_loc(l);                                                                     \
        printf("error in expansion of macro '%.*s': ", (int) m->name.size, m->name.data); \
        printf("  " __VA_ARGS__);                                                         \
        exit(1);                                                                          \
    } while (false)

    assert(args);

    // Replacement list is empty. Don't bother doing anything.
    if (m->tokens.size == 0) return;

    // Check that we have enough args.
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

    // If there are no variadic arguments passed in by the user, we should have
    // synthesised an empty argument.
    assert(!m->is_variadic || args->size == m->params.size + 1);

    // Perform the expansion proper.
    struct pp_expansion exp = {};
    exp.pp = pp;
    exp.m = m;
    exp.args = args;
    exp.l = l;
    pp_expand_function_like_impl(&exp);
    pp_fini_expansion(&exp, start_of_line);
#undef DIAG
}

void pp_expand_object_like(pp pp, macro m, loc l, bool start_of_line) {
    if (m->tokens.size == 0) return;
    struct pp_expansion exp = {};
    exp.pp = pp;
    exp.m = m;
    exp.l = l;
    for (; exp.cursor < m->tokens.size; exp.cursor++) {
        auto t = pp_cur(&exp);
        if (t->type == tt_hash_hash) {
            exp.cursor++;
            pp_paste(&exp, pp_cur(&exp));
        } else {
            vec_push(exp.expansion, tok_copy(t));
        }
    }
    pp_fini_expansion(&exp, start_of_line);
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
    if (pp->tok.type != tt_rparen) {
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
                        pp_read_token_raw(pp);
                        continue;
                    }
                    break;
            }

            vec_push(vec_back(*args), tok_move(&pp->tok));
            pp_read_token_raw(pp);
        }

        // Do NOT discard the right parenthesis just yet. We can only do that
        // *after* expanding the macro so we start reading from its expansion
        // instead of the rest of the file.
        if (parens != 0 || pp->tok.type != tt_rparen)
            pp_error(pp, "unmatched ')' in macro argument list");
    }

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
    if (!m || !m->is_function_like) return m;
    if (pp_look_ahead(pp, 1)->type != tt_lparen) return nullptr;
    pp_read_token_raw(pp);
    pp_read_token_raw(pp);
    pp_get_macro_args(pp, m, args);
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
    vec_push(
        pp->token_streams,
        (struct token_stream) {
            .kind = ts_toks,
            .toks = (token_buffer) {
                .toks = pp->look_ahead_tokens,
                .cursor = 0,
            },
        }
    );
    pp->look_ahead_tokens = (tokens) {};
}

void pp_preprocess(pp pp) {
    if (pp->tok.type == tt_hash) {
        if (!pp->tok.start_of_line || !pp_lexing_file(pp)) pp_error(pp, "unexpected #");
        pp_read_token_raw(pp);

        // TODO: handle the GNU '#' directive.
        if (pp->tok.type != tt_pp_name)
            pp_error(pp, "expected preprocessor directive");

        if (eq(pp->tok.name, lit_span("define"))) pp_dir_define(pp);
        else if (eq(pp->tok.name, lit_span("undef"))) pp_dir_undef(pp);
        else if (eq(pp->tok.name, lit_span("ifdef"))) pp_dir_ifdef(pp, true);
        else if (eq(pp->tok.name, lit_span("ifndef"))) pp_dir_ifdef(pp, false);
        else if (eq(pp->tok.name, lit_span("endif"))) pp_dir_endif(pp);
        else if (eq(pp->tok.name, lit_span("include"))) pp_dir_include(pp);
        else pp_error(pp, "unknown preprocessor directive");
        return pp_preprocess(pp);
    }

    if (pp->tok.type == tt_pp_name) {
        tokens_vec args = {};
        loc l = pp->tok.loc;
        bool sol = pp->tok.start_of_line;
        macro m = pp_get_macro_and_args(pp, &args, as_span(pp->tok.name));
        if (m && !m->expanding) {
            if (m->is_function_like) pp_expand_function_like(pp, m, &args, l, sol);
            else pp_expand_object_like(pp, m, l, sol);
            pp_read_token_raw(pp);
            return pp_preprocess(pp);
        }
    }

    // TODO: Concatenate adjacent string literal tokens here.
}

void pp_read_token(pp pp) {
    pp_read_and_expand_token(pp);
    pp_conv(pp);
}

void pp_read_and_expand_token(pp pp) {
    pp_read_token_raw(pp);
    pp_preprocess(pp);
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

    while (pp->token_streams.size && pp_ts_done(&vec_back(pp->token_streams))) {
        // This is used for macro arg substitution.
        auto s = &vec_back(pp->token_streams);
        if (s->kind == ts_toks && s->toks.keep_when_empty) {
            pp->tok.type = tt_eof;
            return;
        }

        pp_ts_pop(pp);
    }

    if (!pp->token_streams.size) {
        pp->tok.type = tt_eof;
        return;
    }

    auto ts = &vec_back(pp->token_streams);
    switch (ts->kind) {
        case ts_lexer: pp->tok.type = lex(&ts->lex, &pp->tok); break;
        case ts_toks: tok_move_into(&pp->tok, &ts->toks.toks.data[ts->toks.cursor++]); break;
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
        case ts_toks: return s->toks.cursor == s->toks.toks.size;
    }

    die("unreachable");
}

void pp_ts_pop(pp pp) {
    auto ts = vec_pop(pp->token_streams);
    switch (ts.kind) {
        case ts_lexer:
            if (ts.lex.pp_if_depth) pp_error_at(ts.lex.loc, "missing #endif at end of file");
            free(ts.lex.data);
            return;
        case ts_toks:
            vec_free(ts.toks.toks);
            if (ts.toks.m) ts.toks.m->expanding = false;
            return;
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
    for (;;) {
        pp_read_token(pp);
        if (pp->tok.type == tt_eof) break;
        if (first) first = false;
        else if (pp->tok.start_of_line) putchar('\n');
        else if (pp->tok.whitespace_before) putchar(' ');
        if (pp->tok.type == tt_char || pp->tok.type == tt_string) print_escaped_name(&pp->tok);
        else {
            string s = {};
            pp_stringise_token(&s, &pp->tok, false);
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
