#ifndef PLATFORM_H
#define PLATFORM_H

#include "assert.h"
#include "obstack.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define obstack_chunk_free  free
#define obstack_chunk_alloc malloc

#define noreturn  __attribute__((__noreturn__)) void
#define nodiscard __attribute__((__warn_unused_result__))

#if __has_attribute(musttail)
#    define tail __attribute__((musttail))
#else
#    define tail
#endif

typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;
typedef long i64;
typedef unsigned long u64;

noreturn die(const char *msg);

#endif // PLATFORM_H
