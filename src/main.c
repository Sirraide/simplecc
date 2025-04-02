#include "lex.h"
#include "platform.h"
#include "pp.h"
#include "vector.h"

// ====================================================================
//  Helpers
// ====================================================================
noreturn die(const char *msg) {
    printf("fatal error: %s\n", msg);
    exit(1);
}

void print_loc(loc l) {
    printf("%.*s:%zu: ", (int) l.file.size, l.file.data, l.line);
}

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
                tail return lex(l, t);
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

void tok_reset(tok *a) {
    a->type = tt_invalid;
    a->start_of_line = false;
    a->disable_expansion = false;
    a->whitespace_before = false;
    vec_clear(a->name);
    a->val = 0;
}

void tokens_free(tokens *toks) {
    vec_delete_els(t, *toks) vec_free(t->name);
}

void tok_move_into(tok *a, tok *b) {
    tok_free(a);
    *a = tok_move(b);
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

    pp_free(&pp);
}
