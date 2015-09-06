package fpinscala.datastructures

import List._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification


class ListSpec extends Specification with ScalaCheck {
  "List" >> {
    "exercise1" >> {
      x must_== 3
    }

  }
}
