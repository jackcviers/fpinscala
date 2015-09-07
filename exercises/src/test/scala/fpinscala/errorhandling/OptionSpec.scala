package fpinscala.datastructures

import Option._

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification


class OptionSpec extends Specification with ScalaCheck {
  "Option." >> {
    "variance"  >> {
      prop { xs: Seq[Int] => xs != Seq.empty ==>
        variance(xs) must_== Some(
      }
    }
  }
}
