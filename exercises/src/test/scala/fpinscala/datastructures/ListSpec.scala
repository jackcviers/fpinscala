package fpinscala.datastructures

import List._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ListSpec extends Specification with ScalaCheck {
  "List" >> {
    "exercise1" >> {
      x must_== 3
    }
    "tail" >> {
      prop { a: List[Char] =>
        tail(a) == (a match {
          case Cons(x, xs) => xs
          case Nil => Nil
        })
      }
    }
    "length" >> {
      prop { (a: List[Char]) =>
        List.length(a) must_== (a match {
          case Nil => 0
          case _ => 2
        })
      }
    }
    "setHead" >> {
      prop { (a: List[Char], b: Char) =>
        (setHead(a)(b) match {
          case Cons(x, _) => x
          case _ => b.toInt + 1
        }) must_== b
      } && prop { (a: List[Char], b: Char) =>
        (setHead(a)(b) match {
          case Cons(_, xs) => xs
          case _ => Nil
        }) must_== tail(a)
      }
    }
    "drop" >> {
      drop(List(1, 2, 3), 2) must_== List(3)
      drop(List(1, 2, 3), 3) must_== Nil
      drop(List(1, 2, 3), 0) must_== List(1, 2, 3)
    }

    "dropWhile" >> {
      prop { (a: List[Char], f: Char => Boolean) =>
        def loop(cs: List[Char]): Boolean = cs match {
          case Cons(x, xs) if !f(x) => loop(xs)
          case Nil => true
          case _ => false
        }
        loop(dropWhile(a)(f)) must_== true
      }
    }

    "init" >> {
      init(List(1,2,3)) must_== List(1,2)
    }

    "filter" >> {
      def isOdd(i: Int) = i % 2 != 0
      def neg[A](f: A => Boolean): A => Boolean = !f(_)
      def loopFilter(xs: List[Int]):List[Int] = xs match {
        case Cons(x, xs) if isOdd(x) => loopFilter(xs)
        case Cons(x, xs) => Cons(x, loopFilter(xs))
        case Nil => Nil
      }

      prop{ a:List[Int] =>
        filter(a)(_ % 2 != 0) must_== loopFilter(a)
      }
    }

  }
  def genNil = const(Nil)
  def genList = for {
    i <- arbitrary[Char]
    b <- arbitrary[Char]
  } yield Cons(i, List(b))
  def genListInt = for {
    i <- arbitrary[Int]
    b <- arbitrary[Int]
  } yield Cons(i, List(b))

  implicit def abList: Arbitrary[List[Char]] = Arbitrary(Gen.oneOf(genNil, genList))
  implicit def abListInt: Arbitrary[List[Int]] = Arbitrary(Gen.oneOf(genNil, genListInt))
}
