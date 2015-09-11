package fpinscala.errorhandling

import Either._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class EitherSpec extends Specification with ScalaCheck {

  "Either[A, B]" >> {

    "map[B]" >> {
      prop { (f: String => Int ) =>
        Right("a") map(f) must_== Right(f("a"))
      } && prop { (f: String => Int) =>
        Left("Error") map(f) must_== Left("Error")
      }
    }


  }
}
