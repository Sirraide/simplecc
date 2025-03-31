// ====================================================================
//  Constants
// ====================================================================
#define ONE_MEGABYTE (1024 * 1024 * 1024)
#define TOK_BUF_LEN (4096)
#define noreturn __attribute__((__noreturn__)) void

#define ALL_KEYWORDS(kw)   \
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
    kw(return)             \
    kw(short)              \
    kw(signed)             \
    kw(sizeof)             \
    kw(static)             \
    kw(struct)             \
    kw(switch)             \
    kw(true)               \
    kw(typedef)            \
    kw(unsigned)           \
    kw(void)               \
    kw(while)

// ====================================================================
//  Types
// ====================================================================
typedef unsigned long size_t;
typedef long ssize_t;
typedef unsigned long uintptr_t;
typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;
typedef long i64;
typedef unsigned long u64;

// ====================================================================
//  Stdlib
// ====================================================================
void *malloc(size_t size);
void *calloc(size_t count, size_t size);
void free(void *ptr);
char *strdup(const char *str);
int strncmp(const char *s1, const char *s2, size_t n);

void perror(const char *msg);
int puts(const char *msg);
int putchar(int c);

int open(const char *path, int flags, ...);
ssize_t read(int fd, void *buf, size_t count);

noreturn exit(int code);

__attribute__((format(printf, 1, 2))) int printf(const char *format, ...);

// ====================================================================
//  Helpers
// ====================================================================
noreturn die(const char *msg) {
    printf("fatal error: %s\n", msg);
    exit(1);
}

// ====================================================================
//  Lexer
// ====================================================================
typedef struct lexer {
    char *filename;
    char *data;
    const char *char_ptr;
    const char *end_ptr;
    char c;
    size_t line;
    struct lexer *prev;
    bool start_of_line;
    bool whitespace_before;
} *lexer;

typedef enum {
    tt_eof,
    tt_pp_name,
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

#define kw(x) tt_kw_## x,
    ALL_KEYWORDS(kw)
#undef kw
} tt;

typedef struct tok {
    tt type;
    char name[TOK_BUF_LEN];
    u16 namelen;
    u64 val;
    bool start_of_line;
    bool whitespace_before;
} *tok;

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
            l->line++;
            break;
        case '\\':
            if (l->char_ptr < l->end_ptr && *l->char_ptr == '\n') {
                l->char_ptr++;
                l->line++;
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

noreturn lex_error(lexer l, const char* msg) {
    printf("%s:%zu: fatal error: %s\n", l->filename, l->line, msg);
    exit(1);
}

bool is_octal(char c) { return c >= '0' && c <= '7'; }
bool is_hex(char c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }
bool is_bin(char c) { return c == '0' || c == '1'; }
bool is_decimal(char c) { return c >= '0' && c <= '9'; }

void lex_chars(lexer l, tok t, bool (char_p)(char)) {
    while (char_p(l->c)) {
        if (t->namelen == sizeof(t->name)) lex_error(l, "identifier too long (max: 32 chars)");
        t->name[t->namelen++] = l->c;
        lex_char(l);
    }

    if (is_hex(l->c))
        lex_error(l, "unexpected character in integer literal");
}

void lex_number(lexer l, tok t, bool (digit_p)(char), u8 base) {
    lex_chars(l, t, digit_p);
    t->val = 0;
    for (u8 i = 0; i < t->namelen; i++) {
        char c = t->name[i];
        u64 prev = t->val;
        if (c >= '0' && c <= '9') t->val = t->val * base + (u8)(c - '0');
        else if (c >= 'a' && c <= 'f') t->val = t->val * base + (u8)(c - 'a' + 10);
        else if (c >= 'A' && c <= 'F') t->val = t->val * base + (u8)(c - 'A' + 10);
        else die("unreachable");
        if (t->val < prev) lex_error(l, "integer literal overflow");
    }
}

tt lex_check_kw(tok t) {
#define kw(x) if (t->namelen == sizeof(#x) - 1 && strncmp(#x, t->name, t->namelen) == 0) return tt_kw_## x;
    ALL_KEYWORDS(kw)
#undef kw
    return tt_pp_name;
}

tt lex_string(lexer l, tok t, char delim) {
    t->namelen = 0;
    while (!lex_eof(l) && l->c != delim) {
        if (t->namelen == sizeof(t->name)) lex_error(l, "string or character literal too long");
        switch (l->c) {
            default:
                t->name[t->namelen++] = l->c;
                lex_char(l);
                break;
            case '\n': lex_error(l, "unclosed string literal");
            case '\\': {
                lex_char(l);
                char c = l->c;
                lex_char(l);
                switch (c) {
                    default: lex_error(l, "unsupported escape character");
                    case 'n': t->name[t->namelen++] = '\n'; break;
                    case 'r': t->name[t->namelen++] = '\r'; break;
                    case 't': t->name[t->namelen++] = '\t'; break;
                    case 'b': t->name[t->namelen++] = '\b'; break;
                    case 'f': t->name[t->namelen++] = '\f'; break;
                    case 'a': t->name[t->namelen++] = '\a'; break;
                    case 'v': t->name[t->namelen++] = '\v'; break;
                    case '0': t->name[t->namelen++] = '\0'; break;
                    case '\'': t->name[t->namelen++] = '\''; break;
                    case '\"': t->name[t->namelen++] = '\"'; break;
                    case '\\': t->name[t->namelen++] = '\\'; break;
                }
            }
        }
    }

    if (!lex_eat(l, delim))
        lex_error(l, "string or character literal terminated by eof");

    if (delim == '\'' && t->namelen != 1)
        lex_error(l, "character literals must be exactly 1 character");

    return delim == '\'' ? tt_char : tt_string;
}

tt lex(lexer l, tok t) {
    lex_skip_ws(l);
    t->start_of_line = l->start_of_line;
    t->whitespace_before = l->whitespace_before;

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
            t->namelen = 1;
            t->name[0] = c;
            lex_chars(l, t, lex_is_continue);
            return lex_check_kw(t);
        }

        case '0' ... '9': {
            t->namelen = 0;
            if (c == '0') lex_number(l, t, is_octal, 8);
            else if (lex_eat(l, 'x') || lex_eat(l, 'X')) lex_number(l, t, is_hex, 16);
            else if (lex_eat(l, 'b') || lex_eat(l, 'B')) lex_number(l, t, is_bin, 2);
            else {
                t->namelen = 1;
                t->name[0] = c;
                lex_number(l, t, is_decimal, 10);
            }
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

        case '+': return lex_eat(l, '=') ? tt_plus_eq : lex_eat(l, '+') ? tt_plus_plus : tt_plus;
        case '&': return lex_eat(l, '&') ? tt_amp_amp : lex_eat(l, '=') ? tt_amp_eq : tt_amp;
        case '|': return lex_eat(l, '|') ? tt_pipe_pipe : lex_eat(l, '=') ? tt_pipe_eq : tt_pipe;

        case '-':
            if (lex_eat(l, '>')) return tt_arrow;
            return lex_eat(l, '=') ? tt_minus_eq : lex_eat(l, '-') ? tt_minus_minus : tt_minus;

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

// ====================================================================
//  Preprocessor
// ====================================================================
typedef struct pp {
    lexer lex;
    struct tok tok;
} *pp;

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

    lexer l = calloc(sizeof *l, 1);
    l->filename = strdup(filename);
    l->data = buffer;
    l->char_ptr = buffer;
    l->end_ptr = buffer + (size_t) n_read;
    l->prev = pp->lex;
    l->line = 1;
    l->c = '\n'; // to set the start of line flag
    pp->lex = l;
}

void pp_lex_fini(pp pp) {
    lexer l = pp->lex;
    pp->lex = l->prev;
    free(l->filename);
    free(l->data);
    free(l);
}

void pp_read_token(pp pp) {
    while (pp->lex && lex_eof(pp->lex))
        pp_lex_fini(pp);

    if (!pp->lex) {
        pp->tok.type = tt_eof;
        return;
    }

    pp->tok.type = lex(pp->lex, &pp->tok);
    if (pp->tok.type == tt_eof)
        pp_read_token(pp);
}

// ====================================================================
//  Parser
// ====================================================================
typedef struct parser {
    pp pp;
} *parser;

//void parser_parse(parser p) {
//}

// ====================================================================
//  Entry
// ====================================================================
void print_escaped_name(tok t) {
    putchar(t->type == tt_char ? '\'' : '\"');
    for (u64 i = 0; i < t->namelen; i++) {
        char c = t->name[i];
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
    while (pp->tok.type != tt_eof) {
        if (first) first = false;
        else if (pp->tok.start_of_line) putchar('\n');
        else if (pp->tok.whitespace_before) putchar(' ');
        switch (pp->tok.type) {
            case tt_eof: die("unreachable");
            case tt_pp_name: printf("%.*s", pp->tok.namelen, pp->tok.name); break;
            case tt_int_lit: printf("%zu", pp->tok.val); break;
            case tt_char: print_escaped_name(&pp->tok); break;
            case tt_string: print_escaped_name(&pp->tok); break;
            case tt_lparen: printf("("); break;
            case tt_rparen: printf(")"); break;
            case tt_lbrace: printf("{"); break;
            case tt_rbrace: printf("}"); break;
            case tt_lbrack: printf("["); break;
            case tt_rbrack: printf("]"); break;
            case tt_gt: printf(">"); break;
            case tt_gt_eq: printf(">="); break;
            case tt_lt: printf("<"); break;
            case tt_lt_eq: printf("<="); break;
            case tt_eq_eq: printf("=="); break;
            case tt_bang_eq: printf("!="); break;
            case tt_bang: printf("!"); break;
            case tt_assign: printf("="); break;
            case tt_caret: printf("^"); break;
            case tt_amp: printf("&"); break;
            case tt_pipe: printf("|"); break;
            case tt_tilde: printf("~"); break;
            case tt_amp_amp: printf("&&"); break;
            case tt_pipe_pipe: printf("||"); break;
            case tt_lt_lt: printf("<<"); break;
            case tt_gt_gt: printf(">>"); break;
            case tt_slash: printf("/"); break;
            case tt_per_cent: printf("%%"); break;
            case tt_star: printf("*"); break;
            case tt_plus: printf("+"); break;
            case tt_minus: printf("-"); break;
            case tt_plus_eq: printf("+="); break;
            case tt_minus_eq: printf("-="); break;
            case tt_star_eq: printf("*="); break;
            case tt_slash_eq: printf("/="); break;
            case tt_per_cent_eq: printf("%%="); break;
            case tt_caret_eq: printf("^="); break;
            case tt_amp_eq: printf("&="); break;
            case tt_pipe_eq: printf("|="); break;
            case tt_lt_lt_eq: printf("<<="); break;
            case tt_gt_gt_eq: printf(">>="); break;
            case tt_plus_plus: printf("++"); break;
            case tt_minus_minus: printf("--"); break;
            case tt_hash: printf("#"); break;
            case tt_hash_hash: printf("##"); break;
            case tt_semi: printf(";"); break;
            case tt_comma: printf(","); break;
            case tt_colon: printf(":"); break;
            case tt_dot: printf("."); break;
            case tt_ellipsis: printf("..."); break;
            case tt_question: printf("?"); break;
            case tt_arrow: printf("->"); break;
#define kw(x) case tt_kw_## x: printf(#x); break;
            ALL_KEYWORDS(kw)
#undef kw
        }
        pp_read_token(pp);
    }
    putchar('\n');
}

int main(int argc, char **argv) {
    if (argc < 2) die("usage: ./simplecc <file>");
    struct pp pp = {};
    pp_add_lexer(&pp, argv[1]);
    pp_read_token(&pp);
    dump_tokens(&pp);

    //struct parser parser = {.pp = &pp};
    //parser_parse(&parser);
}
