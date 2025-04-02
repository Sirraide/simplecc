#ifndef PLATFORM_H
#define PLATFORM_H

#define noreturn  __attribute__((__noreturn__)) void
#define nodiscard __attribute__((__warn_unused_result__))

#define assert(x)                                \
    do {                                         \
        if (!(x)) die("assertion failed : " #x); \
    } while (false)

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
void *realloc(void *ptr, size_t size);
void free(void *ptr);
char *strdup(const char *str);
int strncmp(const char *s1, const char *s2, size_t n);
size_t strlen(const char *s);
int memcmp(const void *s1, const void *s2, size_t n);
void *memset(void *s, int c, size_t n);
void *memcpy(void *restrict dest, const void *restrict src, size_t n);
void *memmove(void *dest, const void *src, size_t n);
char *realpath(const char *restrict path, char *restrict resolved_path);
char *strrchr(const char *s, int c);

void perror(const char *msg);
int puts(const char *msg);
int putchar(int c);

int open(const char *path, int flags, ...);
ssize_t read(int fd, void *buf, size_t count);

noreturn exit(int code);

__attribute__((format(printf, 1, 2))) int printf(const char *format, ...);

#endif // PLATFORM_H
