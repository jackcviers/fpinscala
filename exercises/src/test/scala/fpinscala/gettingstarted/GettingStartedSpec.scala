package fpinscala.gettingstarted

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scala.collection.JavaConversions._
import PolymorphicFunctions._

class GettingStartedSpec extends Specification with ScalaCheck {

  "PolymorphicFunctions" >> {
    "isSorted[A]" >> {
      def gt(i: Char, j: Char) = i >= j
      def gtS(i: Short, j: Short) = i >= j
      prop { (as: Array[Char]) =>
        isSorted(as.sorted, gt)
      } && prop{as: Array[Short] =>
        isSorted(as.sorted, gtS)
      }
    }
    "curry[A, B, C]" >> {
      prop{ (f: (Int, Int) => Int, i: Int)  =>
        curry(f)(i)(i) must_== f(i, i)
      }
    }
    "uncurry[A, B, C]" >> {
      prop{ (f: (Int) => (Int) => Int, i: Int) =>
       uncurry(f)(i, i) must_== f(i)(i)
      }
    }
    "compose[A,B,C]" >> {
      prop{(f: Char => String, g: Int => Char, i:Int) =>
        compose(f,g)(i) must_== f(g(i))
      }
    }
  }

}
