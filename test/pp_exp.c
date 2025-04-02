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

// + 123
// + 123 123 123 123
#define A 123
A
A A A A

// + A A
#undef A
A A

// + A
// + A)
// + +
#define A(a) a + a
A
A)
A()

// + 1 + 1
// + 2 + 2 3 + 3
A(1)
A(2) A(3)

// + a b ab 1234 ++ -= ==
#define A(x, y) x##y
A(,)
A(a,) A(,b) A(a,b) A(12,34) A(+, +) A(-,=) A(==,) A(,==) A(  =  ,    =)

// + m(1, 2 ) m(1, 2, 1) m(1, 2, 1, 2, 3)
#define A(...) m(1, 2 __VA_OPT__(,) __VA_ARGS__)
A() A(1) A(1, 2, 3)

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
#define A(a,b,c,...) X a##__VA_OPT__()##b a##__VA_OPT__(c)##b X
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
#define S2(x) #x
#define S(x) S2(x)
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

// + (1, 2)
// + (1, 2)
// + (1, 42)
// + (1, 42)
#define A(x, y) (x, y)
#define VaFirst(x, ...) x
#define Default42(x, ...) A(x, VaFirst(__VA_ARGS__ __VA_OPT__(,) 42))
Default42(1, 2)
Default42(1, 2, 3)
Default42(1,)
Default42(1)

// + 1 + 1
// + 1 + 1
// + 1 + 1
#define L (
#define R )
#define Id(x) x
#define Q(x) x + x
#define E()
Id(Q L 1 R)
Id(Q Id(L) 1 Id(R))
Id(Q Id(E)() Id(L) 1 Id(R))

// + f(2 * (f(2 * (z[0]))));
#define f(a) f(x * (a))
#define x 2
#define z z[0]
f(f(z));

// + A B
#define A B
#define B A
A B

// + A B C A B A C A B C A
#define A A B C
#define B B C A
#define C C A B
A

// + int i(void)
#define i(x) h(x
#define h(x) x(void)
extern int i(i));

// + a: 2 + M_0(3)(4)(5);
// + b: 4 + 4 + 3 + 2 + 1 + M_0(3)(2)(1);
#define M_0(x) M_ ## x
#define M_1(x) x + M_0(0)
#define M_2(x) x + M_1(1)
#define M_3(x) x + M_2(2)
#define M_4(x) x + M_3(3)
#define M_5(x) x + M_4(4)
a: M_0(1)(2)(3)(4)(5);
b: M_0(5)(4)(3)(2)(1);

// + c: m a X
#define n(v) v
#define l m
#define m l a
c: n(m) X

// + A: Y
#define X() Y
#define Y() X
A: X()()()

// + B: f()
// + C: for()
#define f(x) h(x
#define for(x) h(x
#define h(x) x()
B: f(f))
C: for(for))

#define IDENTITY1(x) x
#define IDENTITY2(x) IDENTITY1(x) IDENTITY1(x) IDENTITY1(x) IDENTITY1(x)
#define IDENTITY3(x) IDENTITY2(x) IDENTITY2(x) IDENTITY2(x) IDENTITY2(x)
#define IDENTITY4(x) IDENTITY3(x) IDENTITY3(x) IDENTITY3(x) IDENTITY3(x)
#define IDENTITY5(x) IDENTITY4(x) IDENTITY4(x) IDENTITY4(x) IDENTITY4(x)
#define IDENTITY6(x) IDENTITY5(x) IDENTITY5(x) IDENTITY5(x) IDENTITY5(x)
#define IDENTITY7(x) IDENTITY6(x) IDENTITY6(x) IDENTITY6(x) IDENTITY6(x)
#define IDENTITY8(x) IDENTITY7(x) IDENTITY7(x) IDENTITY7(x) IDENTITY7(x)
#define IDENTITY9(x) IDENTITY8(x) IDENTITY8(x) IDENTITY8(x) IDENTITY8(x)
#define IDENTITY0(x) IDENTITY9(x) IDENTITY9(x) IDENTITY9(x) IDENTITY9(x)
IDENTITY0()

// + first second third
#define FOO() BAR() second
#define BAR()
first FOO() third

