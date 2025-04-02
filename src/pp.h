#ifndef PP_H
#define PP_H

#include "vector.h"
#include "lex.h"

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

noreturn pp_error(pp pp, const char *msg);
noreturn pp_error_at(loc l, const char *err);

void pp_add_lexer(pp pp, const char *filename);
void pp_dir(pp pp);
void pp_do_define(pp pp);
void pp_enter_token_stream(pp pp, tokens toks, macro m);
void pp_free(pp pp);
bool pp_lexing_file(pp pp);
tok *pp_look_ahead(pp pp, size_t n);
bool pp_maybe_expand_macro(pp pp);
void pp_read_and_expand_token(pp pp);
void pp_read_token_raw(pp pp);
void pp_read_token(pp pp);
void pp_stringise_token(string *s, const tok *t, bool escape);
void pp_ts_pop(pp pp);
bool pp_undefine(pp pp, span m);

#endif //PP_H
