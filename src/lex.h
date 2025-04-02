#ifndef LEX_H
#define LEX_H

#include "platform.h"
#include "vector.h"

// ====================================================================
//  Lexer
// ====================================================================

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

typedef enum : u8 {
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

typedef struct loc {
    span file;
    size_t line;
} loc;

typedef struct tok {
    loc loc;
    tt type;

    /// Whether this token is at the start of a line; this is relevant
    /// for preprocessor directives.
    bool start_of_line;

    /// Whether macro expansion is disabled for this token.
    bool disable_expansion;

    /// Whether there is whitespace before this token.
    bool whitespace_before;

    /// The contents of a string, character literal, or identifier.
    string name;

    /// The value of a numeric literal; for a __VA_OPT__ token, the
    /// index of the matching closing parenthesis.
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

tt lex(lexer l, tok *t);
bool lex_eof(lexer l);

void print_loc(loc l);

tok tok_copy(const tok *t);
void tok_free(tok *t);
tok tok_move(tok *t);
void tok_move_into(tok *a, tok *b);
void tok_reset(tok* t);
void tokens_free(tokens* toks);

#endif // LEX_H
