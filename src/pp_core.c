#include "fcntl.h"
#include "pp.h"
#include "sys/mman.h"
#include "sys/stat.h"
#include "unistd.h"

void pp_read_token_raw_impl(pp pp, bool include_look_ahead);

void pp_init(pp pp) {
    obstack_init(&pp->string_alloc);
}

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

void pp_add_lexer_for_file(pp pp, const char *filename) {
    int fd = open(filename, 0);
    if (fd < 0) {
        perror(filename);
        die("failed to open file");
    }

    struct stat s;
    if (fstat(fd, &s) < 0) {
        perror(filename);
        die("failed to stat file");
    }

    struct memory_map contents = {};
    if (s.st_size) {
        auto ptr = mmap(
            nullptr,
            (size_t) s.st_size,
            PROT_READ,
            MAP_PRIVATE,
            fd,
            0
        );

        if (ptr == MAP_FAILED) {
            perror(filename);
            die("failed to mmap file");
        }

        contents.ptr = ptr;
        contents.size = (size_t) s.st_size;
        vec_push(pp->memory_maps, contents);
    }

    auto name_len = strlen(filename);
    span name = {.data = obstack_copy(&pp->string_alloc, filename, name_len), .size = name_len};

    struct lexer l = {};
    lexer_init(&l, &pp->string_alloc, name, (span) {.data = contents.ptr, .size = contents.size});
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
    else if (eq(pp->tok.text, lit_span(#x))) pp->tok.type = tt_kw_##x;
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
    vec_free(m->params);
    vec_free(m->tokens);
}

void pp_free(pp pp) {
    vec_delete_els(def, pp->defs)
        pp_free_macro(def);

    vec_delete_els(m, pp->memory_maps)
        munmap((void*) m->ptr, m->size);

    vec_free(pp->look_ahead_tokens);
    vec_free(pp->token_streams);
    obstack_free(&pp->string_alloc, nullptr);
}

tok *pp_look_ahead(pp pp, size_t n) {
    if (n == 0) return &pp->tok;
    if (n <= pp->look_ahead_tokens.size)
        return &pp->look_ahead_tokens.data[n - 1];

    tok saved = pp->tok;
    for (size_t i = pp->look_ahead_tokens.size; i < n; i++) {
        pp_read_token_raw_impl(pp, false);
        vec_push(pp->look_ahead_tokens, pp->tok);
    }

    pp->tok = saved;
    tail return pp_look_ahead(pp, n);
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
            lex_free(&ts.lex);
            return;
        case ts_toks:
            vec_free(ts.toks.toks);
            if (ts.toks.m) ts.toks.m->expanding = false;
            return;
    }

    die("unreachable");
}

void pp_preprocess(pp pp) {
    if (pp->tok.type == tt_hash && pp->tok.start_of_line && pp_lexing_file(pp)) {
        pp_read_token_raw(pp);

        // TODO: handle the GNU '#' directive.
        if (pp->tok.type != tt_pp_name)
            pp_error(pp, "expected preprocessor directive");

        pp_dir(pp);
        tail return pp_preprocess(pp);
    }

    if (pp->tok.type == tt_pp_name && pp_maybe_expand_macro(pp))
        tail return pp_preprocess(pp);

    // TODO: Concatenate adjacent string literal tokens here.
}

void pp_read_token_raw_impl(pp pp, bool include_look_ahead) {
    tok_reset(&pp->tok);

    if (include_look_ahead && pp->look_ahead_tokens.size) {
        pp->tok = vec_pop_front(pp->look_ahead_tokens);
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
        case ts_toks: pp->tok = ts->toks.toks.data[ts->toks.cursor++]; break;
    }

    if (pp->tok.type == tt_eof)
        tail return pp_read_token_raw_impl(pp, include_look_ahead);
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
        auto m = vec_find_if(p, pp->defs, eq(p->name, pp->tok.text));
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
