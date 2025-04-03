#ifndef PP_H
#define PP_H

#include "lex.h"
#include "vector.h"

typedef struct pp_token_stream {
    enum pp_token_stream_kind {
        pp_token_stream_lexer,
        pp_token_stream_toks,
    } kind;

    union {
        lexer lex;
        token_buffer toks;
    };
} pp_token_stream;

typedef struct pp_macro {
    span name;             ///< The name of the macro.
    tokens tokens;         ///< The replacement list of the macro.
    span_vec params;       ///< The names of the macro parameters.
    bool is_variadic;      ///< Whether this function-like macro is variadic.
    bool is_function_like; ///< Whether this even is a function-like macro.
    bool requires_pasting; ///< Whether this object-like macro contains any '##' characters.
    bool expanding;        ///< Whether this macro is currently being expanded.
} pp_macro;

typedef struct pp_memory_map {
    const void *ptr;
    size_t size;
} pp_memory_map;

typedef struct pp {
    tok tok;
    tokens look_ahead_tokens;
    vec(pp_token_stream) token_streams;
    vec(pp_macro) defs;
    vec(pp_memory_map) memory_maps;
    struct obstack string_alloc;
} pp;

noreturn pp_error(pp *pp, const char *msg);
noreturn pp_error_at(loc l, const char *err);

void pp_add_lexer_for_file(pp *pp, const char *filename);
void pp_dir(pp *pp);
void pp_do_define(pp *pp);
void pp_enter_token_stream(pp *pp, tokens toks, pp_macro *m);
void pp_free(pp *pp);
void pp_init(pp *pp);
bool pp_lexing_file(pp *pp);
tok *pp_look_ahead(pp *pp, size_t n);
bool pp_maybe_expand_macro(pp *pp);
void pp_read_and_expand_token(pp *pp);
void pp_read_token_raw(pp *pp);
void pp_read_token(pp *pp);
void pp_stringise_token(string *s, const tok *t, bool escape);
void pp_ts_pop(pp *pp);
bool pp_undefine(pp *pp, span m);

#endif // PP_H
