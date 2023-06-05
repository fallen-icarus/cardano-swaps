/// A fraction of integers. This type can be used to represent decimals on-chain.
///
/// In order to guarantee the invariant of the denominator always being positive, the type
/// constructors are not exposed. Instead, a `Rational` can be created with `unsafe_ratio`, `ratio`,
/// or `from_integer`. All operations on the `Rational` type enforce this invariant.
///
/// All arithmetic operations on the `Rational` type automatically reduce the resulting fraction by
/// dividing the numerator and denominator by their greatest common divisor.
///
/// Operations on `Rational` can easily be piped together:
/// ```aiken
/// from_integer(21)
///     |> add(rat1)
///     |> multiply(rat2)
///     |> subtract(rat3)
/// ```
pub opaque type Rational {
  numerator: Int,
  denominator: Int,
}

/// Creates a `Rational` from a numerator and denominator. 
///
/// This will throw an error if the denominator is zero. If you can tolerate a size increase, and
/// care about safety, use `ratio` instead.
pub fn unsafe_ratio(numerator: Int, denominator: Int) -> Rational {
  if denominator > 0 {
    let gcd_ = gcd(numerator,denominator)
    Rational{ numerator: numerator / gcd_, denominator: denominator / gcd_}
  } else if denominator < 0 {
    unsafe_ratio(-numerator,-denominator)
  } else {
    error @"unsafe_ratio denominator == zero"
  }
}

test unsafe_ratio_test1() {
  unsafe_ratio(0, 1) == Rational { numerator: 0, denominator: 1 }
}

test unsafe_ratio_test2() {
  unsafe_ratio(6, 10) == Rational { numerator: 3, denominator: 5 }
}

test unsafe_ratio_test3() {
  unsafe_ratio(6, -10) == Rational { numerator: -3, denominator: 5 }
}

test unsafe_ratio_test4() {
  unsafe_ratio(-6,-10) == Rational { numerator: 3, denominator: 5 }
}

test unsafe_ratio_test5() {
  unsafe_ratio(-6,10) == Rational { numerator: -3, denominator: 5 }
}

/// Safely constructs a `Rational` type from a numerator and denominator. Will return
/// `None` if the denominator is zero.
pub fn ratio(numerator: Int, denominator: Int) -> Option<Rational> {
  if denominator > 0 {
    let gcd_ = gcd(numerator,denominator)
    Some(
      Rational { numerator: numerator / gcd_, denominator: denominator / gcd_ }
    )
  } else if denominator < 0 {
    ratio(-numerator,-denominator)
  } else {
    None
  }
}

test ratio_test1() {
  ratio(0, 1) == Some(Rational { numerator: 0, denominator: 1 })
}

test ratio_test2() {
  ratio(1, 0) == None
}

test ratio_test3() {
  ratio(6, 10) == Some(Rational { numerator: 3, denominator: 5 })
}

test ratio_test4() {
  ratio(6, -10) == Some(Rational { numerator: -3, denominator: 5 })
}

test ratio_test5() {
  ratio(-6,-10) == Some(Rational {numerator: 3, denominator: 5})
}

/// Returns the numerator of its argument.
pub fn numerator(x: Rational) -> Int {
  let Rational(num,_) = x
  num
}

test numerator_test() {
  numerator(Rational{numerator: 10, denominator: 13}) == 10
}

/// Returns the denominator of its argument.
pub fn denominator(x: Rational) -> Int {
  let Rational(_,den) = x
  den
}

test denominator_test() {
  denominator(Rational{numerator: 10, denominator: 13}) == 13
}

/// Converts an `Int` into the equivalent `Rational`.
pub fn from_integer(x: Int) -> Rational {
  Rational { numerator: x, denominator: 1 }
}

test from_integer_test() {
  from_integer(3) == Rational{numerator: 3, denominator: 1}
}

pub fn negate(x: Rational) -> Rational {
  let Rational(num,den) = x
  Rational { numerator: -num, denominator: den }
}

test negate_test() {
  negate(Rational { numerator: 10, denominator: 21 }) == Rational {
    numerator: -10,
    denominator: 21,
  }
}

/// Returns the absolute value of the fraction.
pub fn abs(x: Rational) -> Rational {
  let Rational(num,den) = x
  when num < 0 is {
    True ->
      Rational { numerator: -num, denominator: den }
    _ ->
      x
  }
}

test abs_test1() {
  abs(Rational { numerator: -1, denominator: 10 }) == Rational {
    numerator: 1,
    denominator: 10,
  }
}

test abs_test2() {
  abs(Rational { numerator: 1, denominator: 10 }) == Rational {
    numerator: 1,
    denominator: 10,
  }
}

/// Returns the reciprical of a fraction. Since the reciprical of zero is mathematically
/// undefined, this function will throw an error when taking the reciprical of zero.
pub fn recip(x: Rational) -> Rational {
  let Rational(num,den) = x
  if num == 0 {
    error @"Denominator of recip cannot be 0"
  } else if num < 0 {
    Rational { numerator: -den, denominator: -num }
  } else {
    Rational { numerator: den, denominator: num }
  }
}

test recip_test1() {
  recip(Rational { numerator: 3, denominator: 7 }) == Rational {
    numerator: 7,
    denominator: 3,
  }
}

test recip_test2() {
  recip(Rational { numerator: -3, denominator: 7 }) == Rational {
    numerator: -7,
    denominator: 3,
  }
}

/// Reduce a fraction by dividing the numerator and denominator by the greatest common divisor.
pub fn reduce(x: Rational) -> Rational {
  let Rational(num,den) = x
  let gcd_ =
    gcd(num, den)
  Rational { numerator: num / gcd_, denominator: den / gcd_ }
}

test reduce_test() {
  reduce(Rational(10,6)) == Rational(5,3)
}

/// Add two `Rational` types.
pub fn add(x: Rational, y: Rational) -> Rational {
  let Rational(x_num, x_den) = x
  let Rational(y_num, y_den) = y
  let new_num =
    x_num * y_den + y_num * x_den
  let new_den =
    x_den * y_den
  let gcd_ =
    gcd(new_num, new_den)

  Rational { numerator: new_num / gcd_, denominator: new_den / gcd_ }
}

test add_test1() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 3, denominator: 10 }
  add(rat_1, rat_2) == Rational { numerator: 7, denominator: 10 }
}

test add_test2() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  add(rat_1, rat_2) == Rational { numerator: 21, denominator: 40 }
}

test add_test3() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let sum =
    add(rat_1, rat_2)
      |> add(rat_3)
  sum == Rational { numerator: 33, denominator: 40 }
}

test add_test4() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let rat_4 =
    Rational { numerator: 21, denominator: 100 }
  let sum =
    add(rat_1, rat_2)
      |> add(rat_3)
      |> add(rat_4)
  sum == Rational { numerator: 207, denominator: 200 }
}

test add_test5() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let rat_4 =
    Rational { numerator: 21, denominator: 100 }
  let rat_5 =
    Rational { numerator: 3, denominator: 5 }
  let rat_6 =
    Rational { numerator: 2, denominator: 8 }
  let rat_7 =
    Rational { numerator: 4, denominator: 10 }
  let rat_8 =
    Rational { numerator: 22, denominator: 100 }
  let sum =
    add(rat_1, rat_2)
      |> add(rat_3)
      |> add(rat_4)
      |> add(rat_5)
      |> add(rat_6)
      |> add(rat_7)
      |> add(rat_8)
  sum == Rational { numerator: 501, denominator: 200 }
}

/// Subtract two `Rational` types.
pub fn subtract(x: Rational, y: Rational) -> Rational {
  let Rational(x_num,x_den) = x
  let Rational(y_num,y_den) = y
  let new_num =
    x_num * y_den - y_num * x_den
  let new_den =
    x_den * y_den
  let gcd_ =
    gcd(new_num, new_den)

  Rational { numerator: new_num / gcd_, denominator: new_den / gcd_ }
}

test subtract_test1() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 3, denominator: 10 }
  subtract(rat_1, rat_2) == Rational { numerator: 1, denominator: 10 }
}

test subtract_test2() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  subtract(rat_1, rat_2) == Rational { numerator: 11, denominator: 40 }
}

test subtract_test3() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let sum =
    subtract(rat_1, rat_2)
      |> subtract(rat_3)
  sum == Rational { numerator: -1, denominator: 40 }
}

test subtract_test4() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let rat_4 =
    Rational { numerator: 21, denominator: 100 }
  let sum =
    subtract(rat_1, rat_2)
      |> subtract(rat_3)
      |> subtract(rat_4)
  sum == Rational { numerator: -47, denominator: 200 }
}

test subtract_test5() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let rat_4 =
    Rational { numerator: 21, denominator: 100 }
  let rat_5 =
    Rational { numerator: 3, denominator: 5 }
  let rat_6 =
    Rational { numerator: 2, denominator: 8 }
  let rat_7 =
    Rational { numerator: 4, denominator: 10 }
  let rat_8 =
    Rational { numerator: 22, denominator: 100 }
  let sum =
    subtract(rat_1, rat_2)
      |> subtract(rat_3)
      |> subtract(rat_4)
      |> subtract(rat_5)
      |> subtract(rat_6)
      |> subtract(rat_7)
      |> subtract(rat_8)
  sum == Rational { numerator: -341, denominator: 200 }
}

/// Multiply two `Rational` types.
pub fn multiply(x: Rational, y: Rational) -> Rational {
  let Rational(x_num,x_den) = x
  let Rational(y_num,y_den) = y
  let new_num =
    x_num * y_num
  let new_den =
    x_den * y_den
  let gcd_ =
    gcd(new_num, new_den)

  Rational { numerator: new_num / gcd_, denominator: new_den / gcd_ }
}

test multiply_test1() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 3, denominator: 10 }
  multiply(rat_1, rat_2) == Rational { numerator: 3, denominator: 25 }
}

test multiply_test2() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  multiply(rat_1, rat_2) == Rational { numerator: 1, denominator: 20 }
}

test multiply_test3() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let sum =
    multiply(rat_1, rat_2)
      |> multiply(rat_3)
  sum == Rational { numerator: 3, denominator: 200 }
}

test multiply_test4() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let rat_4 =
    Rational { numerator: 21, denominator: 100 }
  let sum =
    multiply(rat_1, rat_2)
      |> multiply(rat_3)
      |> multiply(rat_4)
  sum == Rational { numerator: 63, denominator: 20000 }
}

test multiply_test5() {
  let rat_1 =
    Rational { numerator: 2, denominator: 5 }
  let rat_2 =
    Rational { numerator: 1, denominator: 8 }
  let rat_3 =
    Rational { numerator: 3, denominator: 10 }
  let rat_4 =
    Rational { numerator: 21, denominator: 100 }
  let rat_5 =
    Rational { numerator: 3, denominator: 5 }
  let rat_6 =
    Rational { numerator: 2, denominator: 8 }
  let rat_7 =
    Rational { numerator: 4, denominator: 10 }
  let rat_8 =
    Rational { numerator: 22, denominator: 100 }
  let sum =
    multiply(rat_1, rat_2)
      |> multiply(rat_3)
      |> multiply(rat_4)
      |> multiply(rat_5)
      |> multiply(rat_6)
      |> multiply(rat_7)
      |> multiply(rat_8)
  sum == Rational { numerator: 2079, denominator: 50000000 }
}

/// Rounds the argument down towards negative infinity.
pub fn truncate(x: Rational) -> Int {
  let Rational(num,den) = x
  num / den
}

test truncate_test1() {
  let num =
    unsafe_ratio(13, 10) |> truncate
  num == 1
}

test truncate_test2() {
  let num =
    unsafe_ratio(10, 10) |> truncate
  num == 1
}

test truncate_test3() {
  let num = unsafe_ratio(-3,5) |> truncate
  num == -1
}

test truncate_test4() {
  let num = unsafe_ratio(-5,5) |> truncate
  num == -1
}

test truncate_test5() {
  let num = unsafe_ratio(-6,5) |> truncate
  num == -2
}

/// Check if one fraction is less than another.
pub fn lt(x: Rational, y: Rational) -> Bool {
  let Rational(x_num,x_den) = x
  let Rational(y_num,y_den) = y
  x_num * y_den < y_num * x_den
}

test lt_test1() {
  lt(unsafe_ratio(11, 13), unsafe_ratio(21, 21))
}

test lt_test2() {
  !lt(unsafe_ratio(14, 13), unsafe_ratio(21, 21))
}

test lt_test3() {
  !lt(unsafe_ratio(14, 13), unsafe_ratio(14, 13))
}

/// Check if one fraction is less than or equal to another.
pub fn lte(x: Rational, y: Rational) -> Bool {
  let Rational(x_num,x_den) = x
  let Rational(y_num,y_den) = y
  x_num * y_den <= y_num * x_den
}

test lte_test1() {
  lte(unsafe_ratio(11, 13), unsafe_ratio(21, 21))
}

test lte_test2() {
  !lte(unsafe_ratio(14, 13), unsafe_ratio(21, 21))
}

test lte_test3() {
  lte(unsafe_ratio(14, 13), unsafe_ratio(14, 13))
}

/// Check if one fraction is greater than or equal to another.
pub fn gte(x: Rational, y: Rational) -> Bool {
  let Rational(x_num,x_den) = x
  let Rational(y_num,y_den) = y
  x_num * y_den >= y_num * x_den
}

test gte_test1() {
  !gte(unsafe_ratio(11, 13), unsafe_ratio(21, 21))
}

test gte_test2() {
  gte(unsafe_ratio(14, 13), unsafe_ratio(21, 21))
}

test gte_test3() {
  gte(unsafe_ratio(14, 13), unsafe_ratio(14, 13))
}

/// Check if one fraction is greater than another.
pub fn gt(x: Rational, y: Rational) -> Bool {
  let Rational(x_num,x_den) = x
  let Rational(y_num,y_den) = y
  x_num * y_den > y_num * x_den
}

test gt_test1() {
  !gt(unsafe_ratio(11, 13), unsafe_ratio(21, 21))
}

test gt_test2() {
  gt(unsafe_ratio(14, 13), unsafe_ratio(21, 21))
}

test gt_test3() {
  !gt(unsafe_ratio(14, 13), unsafe_ratio(14, 13))
}

/// Rounds the argument up towards positive infinity.
pub fn ceiling(x: Rational) -> Int {
  let Rational(num,den) = x
  if num % den == 0 {
    num / den
  } else {
    num / den + 1
  }
}

test ceiling_test1() {
  ceiling(unsafe_ratio(13, 5)) == 3
}

test ceiling_test2() {
  ceiling(unsafe_ratio(15, 5)) == 3
}

test ceiling_test3() {
  ceiling(unsafe_ratio(16,5)) == 4
}

test ceiling_test4() {
  ceiling(unsafe_ratio(-3,5)) == 0
}

test ceiling_test5() {
  ceiling(unsafe_ratio(-5,5)) == -1
}

test ceiling_test6() {
  ceiling(unsafe_ratio(-6,5)) == -1
}

/// Return the proper fraction of the argument.
pub fn proper_fraction(x: Rational) -> (Int, Rational) {
  let Rational(num, den) =
    x
  (num / den, Rational { numerator: num % den, denominator: den })
}

test proper_fraction_test1() {
  proper_fraction(unsafe_ratio(10, 7)) == (
    1,
    Rational { numerator: 3, denominator: 7 },
  )
}

test proper_fraction_test2() {
  proper_fraction(unsafe_ratio(-10, 7)) == (
    -2,
    Rational { numerator: 4, denominator: 7 },
  )
}

/// Round the argument to the nearest whole number. If the argument is equidistant between two
/// values, the nearest even number will be given.
pub fn round(x: Rational) -> Int {
  let (n, r) =
    proper_fraction(x)
  let m =
    if lt(r, from_integer(0)) {
      n - 1
    } else {
      n + 1
    }
  let flag =
    subtract(abs(r), unsafe_ratio(1, 2))
  if lt(flag, from_integer(0)) {
    n
  } else if flag == from_integer(0) {
    if n % 2 == 0 {
      n
    } else {
      m
    }
  } else {
    m
  }
}

test round_test1() {
  round(unsafe_ratio(-10, 7)) == -1
}

test round_test2() {
  round(unsafe_ratio(10, 7)) == 1
}

test round_test3() {
  round(unsafe_ratio(3, 2)) == 2
}

test round_test4() {
  round(unsafe_ratio(5, 2)) == 2
}

/// Find the greatest common divisor of two integers. The calculated gcd will always be
/// positive as long as the second integer is positive.
pub fn gcd(x: Int, y: Int) -> Int {
  when y is {
    0 ->
      x
    _ ->
      gcd(y, x % y)
  }
}

test gcd_test1() {
  gcd(10, 300) == 10
}

test gcd_test2() {
  gcd(-10, 300) == 10
}

/// The `Rational` for zero.
pub fn zero() -> Rational {
  Rational(0,1)
}