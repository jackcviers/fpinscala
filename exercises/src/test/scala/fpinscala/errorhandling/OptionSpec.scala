package fpinscala.errorhandling

import Option._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scala.util.Try

class OptionSpec extends Specification with ScalaCheck {
  "Option." >> {
    "variance" >> {
      def mean(xs: Seq[Double]) = Try((xs sum) / (xs length)).toOption
      prop { xs: Seq[Double] =>
        (xs != Seq.empty && mean(xs).get != Double.NaN) ==> {
          variance(xs).getOrElse(0) must_== (mean(xs.map { x: Double => math.pow(x - mean(xs).get, 2) })).get
        }
      }
    }
    "map2" >> {
      implicit def arbitraryOptionInt[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] = Arbitrary {
        val genMaybe = for { e <- Arbitrary.arbitrary[T] } yield Some(e)
        val genNone = for { e <- Arbitrary.arbitrary[T] } yield None
        Gen.oneOf(genMaybe, genNone)
      }
      prop { (maybeA: Option[Int], maybeB: Option[String], f: (Int, String) => String) =>
        (maybeA == None) ==> {
          map2(maybeA, maybeB)(f) must_== (maybeA match {
            case Some(c) => maybeB match {
              case Some(d) => f(c, d)
              case None => None
            }
            case None => None
          })
        }
      }
    }
  }
}
