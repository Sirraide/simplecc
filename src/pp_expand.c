#include "pp.h"

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

// ====================================================================
//  Macro Definition Parsing
// ====================================================================
static bool pp_is_param(const tok *t) {
    return t->type == tt_pp_param || t->type == tt_pp_va_args;
}

static bool pp_dir_define_check_hash_impl(macro m, size_t cursor) {
    if (m->tokens.size == cursor) return false;
    auto t = &m->tokens.data[cursor];
    return pp_is_param(t) || t->type == tt_pp_va_opt;
}

static void pp_dir_define_check_hash(pp pp, macro m, size_t cursor) {
    if (m->is_function_like && !pp_dir_define_check_hash_impl(m, cursor))
        pp_error(pp, "expected parameter name or '__VA_OPT__' after '#'");
}

static void pp_dir_define_check_hash_hash(pp pp, macro m, size_t cursor) {
    if (cursor == 1 || m->tokens.size == cursor)
        pp_error(pp, "'##' must not occur at the start or end of a macro definition");
}

static size_t pp_find_va_opt_rparen(pp pp, macro m, size_t *cursor) {
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

void pp_do_define(pp pp) {
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

// ====================================================================
//  Macro Expansion
// ====================================================================
static const tok *pp_cur(pp_expansion exp) {
    assert(exp->cursor < exp->m->tokens.size && "cursor out of bounds");
    return &exp->m->tokens.data[exp->cursor];
}

static size_t pp_get_param_index(pp_expansion exp, const tok *t) {
    assert(t->type != tt_pp_va_opt && "__VA_OPT__ cannot be handled here");
    if (t->type == tt_pp_va_args) return exp->m->params.size;
    auto idx = vec_find_if_index(p, exp->m->params, eq(*p, t->name));
    assert(idx != NO_INDEX);
    return idx;
}

static tokens *pp_get_param_tokens(pp_expansion exp, const tok *t) {
    assert(t->type == tt_pp_param || t->type == tt_pp_va_args);
    return &exp->args->data[pp_get_param_index(exp, t)];
}

static macro pp_get_expandable_macro(pp pp, span name) {
    auto macro = vec_find_if(m, pp->defs, eq(m->name, name));
    if (!macro || macro->expanding) return nullptr;
    return macro;
}

static void pp_get_macro_args(pp pp, macro m, tokens_vec *args) {
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

static macro pp_get_macro_and_args(pp pp, tokens_vec *args, span name) {
    auto m = pp_get_expandable_macro(pp, name);
    if (!m || !m->is_function_like) return m;
    if (pp_look_ahead(pp, 1)->type != tt_lparen) return nullptr;
    pp_read_token_raw(pp);
    pp_read_token_raw(pp);
    pp_get_macro_args(pp, m, args);
    return m;
}

/// This implements C2y 6.10.5.2 'Argument substitution'.
///
/// This only handles the case of there not being any preceding #
/// or ## tokens or any following ##. This *does* handle __VA_ARGS__,
/// as that is just another argument.
static tokens *pp_substitute(pp_expansion exp, size_t param_index) {
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

static void pp_stringise_str(string *s, span str, char delim) {
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

static tok pp_stringise_tokens(const tokens *toks, loc l, bool whitespace_before) {
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

static void pp_paste(pp_expansion exp, const tok *t) {
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

static tok pp_stringise(pp_expansion exp, const tok *t) {
    tokens *param = pp_get_param_tokens(exp, t);
    assert(param && "'#' must be followed by parameter");
    return pp_stringise_tokens(param, t->loc, t->whitespace_before);
}


static void pp_defer_stringise_va_opt(pp_expansion exp, const tok *hash) {
    assert(!exp->va_opt.stringise && "va_opt not reset");
    exp->va_opt.stringise = true;
    exp->va_opt.stringise_loc = hash->loc;
    exp->va_opt.stringise_whitespace_before = hash->whitespace_before;
}

static bool pp_has_variadic_args(pp_expansion exp) {
    // We treat __VA_ARGS__ as just another parameter, so its expansion can be
    // retrieved in much the same manner.
    auto param = pp_substitute(exp, exp->m->params.size);
    return param->size != 0;
}

static void pp_enter_va_opt(pp_expansion exp, const tok *va_opt_token) {
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

static bool pp_in_va_opt(pp_expansion exp) {
    return exp->va_opt.index != NO_INDEX;
}

static void pp_placemarker(pp_expansion exp) {
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

static void pp_va_opt_reset(pp_expansion exp) {
    memset(&exp->va_opt, 0, sizeof(exp->va_opt));
    exp->va_opt.index = NO_INDEX;
}

static void pp_expand_function_like_impl(pp_expansion exp) {
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

static void pp_fini_expansion(pp_expansion exp, bool start_of_line) {
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

static void pp_expand_function_like(pp pp, macro m, tokens_vec *args, loc l, bool start_of_line) {
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
#undef DIAG

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
}

static void pp_expand_object_like(pp pp, macro m, loc l, bool start_of_line) {
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

bool pp_maybe_expand_macro(pp pp) {
    tokens_vec args = {};
    loc l = pp->tok.loc;
    bool sol = pp->tok.start_of_line;
    macro m = pp_get_macro_and_args(pp, &args, as_span(pp->tok.name));

    if (m && !m->expanding) {
        if (m->is_function_like) pp_expand_function_like(pp, m, &args, l, sol);
        else pp_expand_object_like(pp, m, l, sol);
        pp_read_token_raw(pp);
        return true;
    }

    return false;
}
