#include "lex.h"

bool lex_eof(lexer *l) {
    return l->c == 0;
}

static void lex_char_raw(lexer *l) {
    if (l->char_ptr >= l->end_ptr) {
        l->c = 0;
        return;
    }

    l->c = *l->char_ptr++;
}

static void lex_char(lexer *l) {
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

static bool lex_eat(lexer *l, char c) {
    if (l->c != c)
        return false;

    lex_char(l);
    return true;
}

static void lex_skip_ws(lexer *l) {
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

static void lex_skip_line(lexer *l) {
    // Don’t eat the \n here so we can set the start of line flag.
    while (!lex_eof(l) && l->c != '\n')
        lex_char(l);
}

static bool lex_is_continue(char c) {
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

static noreturn lex_error(lexer *l, const char *msg) {
    print_loc(l->loc);
    printf("error: %s\n", msg);
    exit(1);
}

static bool is_octal_or_sep(char c) { return c == '\'' || (c >= '0' && c <= '7'); }
static bool is_hex_or_sep(char c) { return c == '\'' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }
static bool is_bin_or_sep(char c) { return c == '\'' || c == '0' || c == '1'; }
static bool is_decimal_or_sep(char c) { return c == '\'' || (c >= '0' && c <= '9'); }

static void lex_chars(lexer *l, bool(char_p)(char)) {
    while (char_p(l->c)) {
        vec_push(l->tmp, l->c);
        lex_char(l);
    }

    if (is_hex_or_sep(l->c))
        lex_error(l, "unexpected character in integer literal");
}

static void lex_number(lexer *l, tok *t, bool(digit_p)(char), u8 base) {
    lex_chars(l, digit_p);
    t->text = str_save(l->string_alloc, &l->tmp);
    t->val = 0;
    vec_for_val(c, l->tmp) {
        u64 prev = t->val;
        if (c == '\'') continue;
        if (c >= '0' && c <= '9') t->val = t->val * base + (u8) (c - '0');
        else if (c >= 'a' && c <= 'f') t->val = t->val * base + (u8) (c - 'a' + 10);
        else if (c >= 'A' && c <= 'F') t->val = t->val * base + (u8) (c - 'A' + 10);
        else die("unreachable");
        if (t->val < prev) lex_error(l, "integer literal overflow");
    }
}

static tt lex_string(lexer *l, tok *t, char delim) {
    vec_clear(l->tmp);
    while (!lex_eof(l) && l->c != delim) {
        switch (l->c) {
            default:
                vec_push(l->tmp, l->c);
                lex_char(l);
                break;
            case '\n': lex_error(l, "unclosed string literal");
            case '\\': {
                lex_char(l);
                char c = l->c;
                lex_char(l);
                switch (c) {
                    default: lex_error(l, "unsupported escape character");
                    case 'n': vec_push(l->tmp, '\n'); break;
                    case 'r': vec_push(l->tmp, '\r'); break;
                    case 't': vec_push(l->tmp, '\t'); break;
                    case 'b': vec_push(l->tmp, '\b'); break;
                    case 'f': vec_push(l->tmp, '\f'); break;
                    case 'a': vec_push(l->tmp, '\a'); break;
                    case 'v': vec_push(l->tmp, '\v'); break;
                    case '0': vec_push(l->tmp, '\0'); break;
                    case '\'': vec_push(l->tmp, '\''); break;
                    case '\"': vec_push(l->tmp, '\"'); break;
                    case '\\': vec_push(l->tmp, '\\'); break;
                }
            }
        }
    }

    if (!lex_eat(l, delim))
        lex_error(l, "string or character literal terminated by eof");

    if (delim == '\'' && l->tmp.size != 1)
        lex_error(l, "character literals must be exactly 1 character");

    t->text = str_save(l->string_alloc, &l->tmp);
    return delim == '\'' ? tt_char : tt_string;
}

tt lex(lexer *l, tok *t) {
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

            if (lex_eat(l, '*')) {
                while (!lex_eof(l)) {
                    while (!lex_eat(l, '*')) lex_char(l);
                    if (lex_eat(l, '/')) break;
                }

                if (lex_eof(l)) lex_error(l, "unclosed block comment");
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
            vec_clear(l->tmp);
            vec_push(l->tmp, c);
            lex_chars(l, lex_is_continue);
            t->text = str_save(l->string_alloc, &l->tmp);
            return tt_pp_name;
        }

        case '0' ... '9': {
            vec_clear(l->tmp);
            vec_push(l->tmp, c);
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

void tok_reset(tok *a) {
    memset(a, 0, sizeof(tok));
}

void lex_free(lexer *l) {
    vec_free(l->tmp);
}

void lexer_init(lexer *l, struct obstack *string_alloc, span filename, span contents) {
    l->string_alloc = string_alloc;
    l->loc.file = filename;
    l->loc.line = 1;
    l->char_ptr = contents.data;
    l->end_ptr = contents.data + contents.size;
    l->c = '\n'; // to set the start of line flag
}
