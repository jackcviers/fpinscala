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


      prop {a: List[Char] =>
        tail(a) == (a match{
          case Cons(x, xs) => xs
          case Nil => Nil
        })
      }
    }
  }
  def genNil = const(Nil)
  def genList = for{
    i <- arbitrary[Char]
    b <- arbitrary[Char]
  } yield Cons(i, List(b))

  implicit def abList: Arbitrary[List[Char]] = Arbitrary(Gen.oneOf(genNil, genList))
}
