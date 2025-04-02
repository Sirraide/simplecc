#include "pp.h"

static void pp_skip_line_raw(pp pp) {
    do pp_read_token_raw(pp);
    while (pp->tok.type != tt_eof && !pp->tok.start_of_line);
}

static lexer pp_lexer(pp pp) {
    assert(pp_lexing_file(pp));
    return &vec_back(pp->token_streams).lex;
}

// #define
static void pp_dir_define(pp pp) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_pp_name || pp->tok.start_of_line)
        pp_error(pp, "expected macro name");

    pp_do_define(pp);
}

// #endif
static void pp_dir_endif(pp pp) {
    if (!pp_lexing_file(pp) || !pp_lexer(pp)->pp_if_depth)
        pp_error(pp, "unexpected #endif");

    pp_lexer(pp)->pp_if_depth--;
    pp_skip_line_raw(pp);
}

// #if(n)def
static void pp_dir_ifdef(pp pp, bool ifdef) {
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

// #include
static void pp_dir_include(pp pp) {
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
        if (parent) len = (size_t) (parent - resolved);
    }

    vec_clear(s);
    str_cat(s, (span) {.data = resolved, .size = len});
    str_cat_char(s, '/');
    str_cat(s, pp->tok.name);
    str_cat_char(s, 0);
    pp_add_lexer(pp, s.data);
    pp_skip_line_raw(pp);
    vec_free(s);
    free(resolved);
}

// #undef
static void pp_dir_undef(pp pp) {
    pp_read_token_raw(pp);

    if (pp->tok.type != tt_pp_name || pp->tok.start_of_line)
        pp_error(pp, "expected macro name");

    if (!pp_undefine(pp, as_span(pp->tok.name))) {
        print_loc(pp->tok.loc);
        printf("warning: macro '%.*s' is not defined\n", (int) pp->tok.name.size, pp->tok.name.data);
    }

    pp_skip_line_raw(pp);
}

void pp_dir(pp pp) {
    if (eq(pp->tok.name, lit_span("define"))) pp_dir_define(pp);
    else if (eq(pp->tok.name, lit_span("undef"))) pp_dir_undef(pp);
    else if (eq(pp->tok.name, lit_span("ifdef"))) pp_dir_ifdef(pp, true);
    else if (eq(pp->tok.name, lit_span("ifndef"))) pp_dir_ifdef(pp, false);
    else if (eq(pp->tok.name, lit_span("endif"))) pp_dir_endif(pp);
    else if (eq(pp->tok.name, lit_span("include"))) pp_dir_include(pp);
    else pp_error(pp, "unknown preprocessor directive");
}
