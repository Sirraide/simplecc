#include "pp.h"

#define ONE_MEGABYTE (1'024 * 1'024 * 1'024)

void pp_read_token_raw_impl(pp pp, bool include_look_ahead);

bool pp_lexing_file(pp pp) {
    return pp->token_streams.size && vec_back(pp->token_streams).kind == ts_lexer;
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

noreturn pp_error(pp pp, const char *msg) {
    print_loc(pp->tok.loc);
    printf("error: %s\n", msg);
    exit(1);
}

void pp_error_at(loc l, const char *err) {
    print_loc(l);
    printf("error: %s\n", err);
    exit(1);
}

void pp_enter_token_stream(pp pp, tokens toks, macro m) { // clang-format off
    if (m) m->expanding = true;
    pp_materialise_look_ahead_toks(pp);
    vec_push(pp->token_streams, (struct token_stream) {
        .kind = ts_toks,
        .toks = {
            .m = m,
            .toks = toks,
            .cursor = 0,
            .keep_when_empty = false,
        },
    });
} // clang-format on

void pp_free_macro(macro m) {
    vec_free(m->name);
    vec_delete_els(p, m->params) vec_free(*p);
    tokens_free(&m->tokens);
}

void pp_free(pp pp) {
    vec_delete_els(def, pp->defs)
        pp_free_macro(def);

    vec_delete_els(f, pp->filenames)
        vec_free(*f);

    tok_free(&pp->tok);
    tokens_free(&pp->look_ahead_tokens);
    vec_free(pp->token_streams);
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
            tokens_free(&ts.toks.toks);
            if (ts.toks.m) ts.toks.m->expanding = false;
            return;
    }

    die("unreachable");
}

void pp_preprocess(pp pp) {
    if (pp->tok.type == tt_hash) {
        if (!pp->tok.start_of_line || !pp_lexing_file(pp)) pp_error(pp, "unexpected #");
        pp_read_token_raw(pp);

        // TODO: handle the GNU '#' directive.
        if (pp->tok.type != tt_pp_name)
            pp_error(pp, "expected preprocessor directive");

        pp_dir(pp);
        return pp_preprocess(pp);
    }

    if (pp->tok.type == tt_pp_name && pp_maybe_expand_macro(pp))
        return pp_preprocess(pp);

    // TODO: Concatenate adjacent string literal tokens here.
}

void pp_read_token_raw_impl(pp pp, bool include_look_ahead) {
    tok_reset(&pp->tok);

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

    if (pp->tok.type == tt_pp_name) {
        auto m = vec_find_if(p, pp->defs, eq(p->name, pp->tok.name));
        if (m && m->expanding)
            pp->tok.disable_expansion = true;
    }
}

bool pp_undefine(pp pp, span macro_name) {
    size_t idx = vec_find_if_index(v, pp->defs, eq(v->name, macro_name));
    if (idx == NO_INDEX) return false;
    pp_free_macro(pp->defs.data + idx);
    vec_remove_index_unordered(pp->defs, idx);
    return true;
}
