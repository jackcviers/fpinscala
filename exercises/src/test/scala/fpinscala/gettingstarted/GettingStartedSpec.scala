package fpinscala.gettingstarted

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scala.collection.JavaConversions._

class GettingStartedSpec extends Specification with ScalaCheck {


  "PolymorphicFunctions.sortInfo[Int]" >> {
    def gt(i: Char, j: Char) = i >= j
    prop { (as: Array[Char]) =>
      PolymorphicFunctions.isSorted(as.sorted, gt)
    }
  }
}

// import org.specs2.mutable.Specification
// import org.specs2.ScalaCheck

// class GettingStartedSpec extends Specification with ScalaCheck
// // class GettingStartedSpec extends Specification with ScalaCheck
// // class GettingStartedSpec extends Specification with ScalaCheck
