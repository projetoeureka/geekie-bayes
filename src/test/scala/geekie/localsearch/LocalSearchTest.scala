package geekie.localsearch

import org.scalatest.{Matchers, FlatSpec}


class LocalSearchTest extends FlatSpec with Matchers {

  // Let's try to find the local (which is also global) maximum of f(x) = -(x-2)^2 - 10
  // We expect to find a solution near x = 2, in which the value is -10.
  trait Fixture {

    class Func(val x: Double) extends LocallyOptimizable[Func] {

      // Neighborhood is x-.001 and x+.001
      override def neighborhood = List(new Func(x - (1e-3)), new Func(x + (1e-3)))

      override def objective: Double = -1*Math.pow(x - 2, 2) - 10
    }

    val localSearch = new LocalSearch[Func]
  }

  "Local search" should "find the local (and global) optimum of the proposed function" in new Fixture {
    val init = new Func(-3)
    val max = localSearch(init)
    max.x should equal (2.0 +- 1e-3)
    max.objective should equal (-10.0 +- 1e-3)
  }

}
