package fpinscala.errorhandling

import Option._

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
  }
}
