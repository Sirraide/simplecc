// R %cc %s
//
// Macro Expansion Tests
//
// NOTE: Throughout this file, empty expansions should be on a
// line of their own. Our pretty printer isn’t clever enough to
// e.g. realise that 'b' in 'a b' should be printed on a separate
// line if 'a' expands to nothing.
//

#define A
A
A A
A A A A A A

#define A 123

// + 123
// + 123 123 123 123
A
A A A A

// + A A
#undef A
A A

#define A(a) a + a

// + A
// + A)
// + +
A
A)
A()

// + 1 + 1
// + 2 + 2 3 + 3
A(1)
A(2) A(3)

#define A(x, y) x##y

// + a b ab 1234 ++ -= ==
A(,)
A(a,) A(,b) A(a,b) A(12,34) A(+, +) A(-,=) A(==,) A(,==) A(  =  ,    =)

#define A(...) m(1, 2 __VA_OPT__(,) __VA_ARGS__)

// + m(1, 2 ) m(1, 2, 1) m(1, 2, 1, 2, 3)
A() A(1) A(1, 2, 3)

#define A(a,b,c,...) X a##__VA_OPT__()##b a##__VA_OPT__(c)##b X
// + X X
// + X 1 1 X
// + X 2 2 X
// + X X
// + X X
// + X 3 X
// + X 12 12 X
// + X 12 12 X
// + X 12 12 X
// + X 12 132 X
A(,,,)
A(1,,)
A(,2,,)
A(,,3,)
A(,,,4)
A(,,3,4)
A(1,2,,)
A(1,2,3,)
A(1,2,3)
A(1,2,3,4)

#define S2(x) #x
#define S(x) S2(x)

// + "X X"
// + "X 1 1 X"
// + "X 2 2 X"
// + "X X"
// + "X X"
// + "X 3 X"
// + "X 12 12 X"
// + "X 12 12 X"
// + "X 12 12 X"
// + "X 12 132 X"
S(A(,,,))
S(A(1,,))
S(A(,2,,))
S(A(,,3,))
S(A(,,,4))
S(A(,,3,4))
S(A(1,2,,))
S(A(1,2,3,))
S(A(1,2,3))
S(A(1,2,3,4))

// + "A(,,,)"
// + "A(1,,)"
// + "A(,2,,)"
// + "A(,,3,)"
// + "A(,,,4)"
// + "A(,,3,4)"
// + "A(1,2,,)"
// + "A(1,2,3,)"
// + "A(1,2,3)"
// + "A(1,2,3,4)"
S2(A(,,,))
S2(A(1,,))
S2(A(,2,,))
S2(A(,,3,))
S2(A(,,,4))
S2(A(,,3,4))
S2(A(1,2,,))
S2(A(1,2,3,))
S2(A(1,2,3))
S2(A(1,2,3,4))

#define A(x, y) (x, y)
#define VaFirst(x, ...) x
#define Default42(x, ...) A(x, VaFirst(__VA_ARGS__ __VA_OPT__(,) 42))

// + (1, 2)
// + (1, 2)
// + (1, 42)
// + (1, 42)
Default42(1, 2)
Default42(1, 2, 3)
Default42(1,)
Default42(1)

#define L (
#define R )
#define Id(x) x
#define Q(x) x + x
#define E()

// + 1 + 1
// + 1 + 1
// + 1 + 1
Id(Q L 1 R)
Id(Q Id(L) 1 Id(R))
Id(Q Id(E)() Id(L) 1 Id(R))


// + f(2 * (f(2 * (z[0]))));
#define f(a) f(x * (a))
#define x 2
#define z z[0]
f(f(z));

#define A B
#define B A

// + A B
A B

#define A A B C
#define B B C A
#define C C A B

// + A B C A B A C A B C A
A

// + int i(void)
#define i(x) h(x
#define h(x) x(void)
extern int i(i));
