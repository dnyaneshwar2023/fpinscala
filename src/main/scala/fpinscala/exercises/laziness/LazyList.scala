package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{cons, empty, fibsViaUnfold}

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this.foldRight[List[A]](Nil)(_ :: _)

  def toListRecursive: List[A] = this match
    case Cons(h, t) => h() :: t().toListRecursive
    case Empty => Nil

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
    case Cons(h, t) if (n == 1) => cons(h(), empty)
    case _ => empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] = this.foldRight(empty)((a, acc) => if (p(a)) then cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, acc) => p(a) && acc)


  def headOption: Option[A] = this.foldRight(None: Option[A])((a, acc) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail *))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = LazyList.cons(a, single)
    single

  def from(n: Int): LazyList[Int] = cons(n, from(n + 1))

  private def generateFib(a: => Int, b: => Int): LazyList[Int] =
    cons(a, generateFib(b, a + b))

  lazy val fibs: LazyList[Int] = generateFib(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case None => empty
    case Some((value, s)) => cons(value, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] = unfold((0, 1))((current, next) => Some(current, (next, current + next)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(state => Some(state, state + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
