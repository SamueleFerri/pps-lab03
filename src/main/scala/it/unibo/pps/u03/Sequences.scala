package u03

import u03.Optionals.Optional

import scala.annotation.tailrec
import scala.collection.View.Concat

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @annotation.tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(_, t) if n > 0 => skip(t)(n - 1)
      case _ => s

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case _ if first == Nil() || second == Nil() => Nil()
      case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case _ if s1 == Nil() && s2 == Nil() => Nil()
      case (Cons(h, t), _) => Cons(h, concat(t, s2))
      case (_, Cons(h, t)) => Cons(h, concat(t, s1))

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, Nil()) => s
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, Nil()) => mapper(h)
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      @annotation.tailrec
      def recMin(s: Sequence[Int], currentMin: Optional[Int]): Optional[Int] = s match
        case Nil() => currentMin
        case Cons(h, t) =>
          val nextMin = currentMin match
            case Optional.Empty() => Optional.Just(h)
            case Optional.Just(v) => if (h < v) Optional.Just(h) else Optional.Just(v)
          recMin(t, nextMin)
      recMin(s, Optional.Empty())

    def min2(s: Sequence[Int]): Optional[Int] =
      @annotation.tailrec
      def recMin2(s: Sequence[Int], currentMin: Optional[Int]): Optional[Int] = (s, currentMin) match
        case (Nil(), _) => currentMin
        case (Cons(h, t), Optional.Empty()) => recMin2(t, Optional.Just(h))
        case (Cons(h, t), Optional.Just(lastMin)) => recMin2(t, if (h < lastMin) Optional.Just(h) else Optional.Just(lastMin))
      recMin2(s, Optional.Empty())

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      def recEvenIndices(s: Sequence[A], indices: Int): Sequence[A] = s match
        case Cons(h, t) if indices % 2 == 0 => Cons(h, recEvenIndices(t, indices + 1))
        case Cons(_, t) => recEvenIndices(t, indices + 1)
        case Nil() => Nil()
      recEvenIndices(s, 0)

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    @tailrec
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) if h.equals(elem) => true
      case Cons(_, t) => contains(t)(elem)
      case Nil() => false
      

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = ???

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = ???

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
