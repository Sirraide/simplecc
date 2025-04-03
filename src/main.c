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

span str_save(struct obstack *obstack, string *s) {
    if (!s->size) return (span) {};
    char *data = obstack_copy(obstack, s->data, s->size);
    return (span) {.data = data, .size = s->size};
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
    vec_for_val(c, t->text) {
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

void dump_tokens(pp *pp) {
    bool first = true;
    string s = {};
    for (;;) {
        pp_read_token(pp);
        if (pp->tok.type == tt_eof) break;
        if (first) first = false;
        else if (pp->tok.start_of_line) putchar('\n');
        else if (pp->tok.whitespace_before) putchar(' ');
        if (pp->tok.type == tt_char || pp->tok.type == tt_string) print_escaped_name(&pp->tok);
        else {
            vec_clear(s);
            pp_stringise_token(&s, &pp->tok, false);
            printf("%.*s", (int) s.size, s.data);
        }
    }
    vec_free(s);
    putchar('\n');
}

int main(int argc, char **argv) {
    if (argc < 2) die("usage: ./simplecc <file>");
    struct pp pp = {};
    pp_init(&pp);
    pp_add_lexer_for_file(&pp, argv[1]);
    dump_tokens(&pp);

    // struct parser parser = {.pp = &pp};
    // parser_parse(&parser);

    pp_free(&pp);
}
