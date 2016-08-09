package funsets

/**
 * Coursera Functional Programming Principles in Scala
 * https://www.coursera.org/learn/progfun1/home/welcome
 * WEEK 02 Assignment 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (i: Int) => (elem == i)

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (i: Int) => s(i) || t(i)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`. 
   */
  def intersect(s: Set, t: Set): Set = (i: Int) => s(i) && t(i)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (i: Int) => s(i) && !t(i)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (i: Int) => (p(i) && s(i))

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether ALL bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -1 * bound || bound < a) true // termination of recursion.
      else if (contains(s, a)) {
        if (p(a)) iter(a - 1)
        else false
      } 
      // If it is not within s, it does not matter. continue next.
      else iter(a - 1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   * 
   * Using forall, implement a function exists which tests 
   * whether a set contains at least one element for which 
   * the given predicate is true. Note that the functions 
   * forall and exists behave like the universal and 
   * existential quantifiers of first-order logic.
   * 
   * [Logic]
   * Check if all integers do NOT satisfy p within s by forall(s, p).
   * if it is false, then there is at least one integer that is
   * in s and satisfy p.
   */
  /*
  def inv(f: Int => Boolean):(Int => Boolean) = (x => !f(x))
  def inv(f: Int => Boolean):(Int => Boolean) = (!f(_))
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, inv(p))
  */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

  /**
   * Returns a set T transformed by applying `f` to each element of `s`.
   * [Logic]
   * Pick a point Y in T. Check if there is X which satisfies:
   * 1. s(X) -> true
   * 2. f(X) -> Y
   */
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, (x => (f(x) == y)))

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
