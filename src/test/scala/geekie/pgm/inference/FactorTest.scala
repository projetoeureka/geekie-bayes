package geekie.pgm.inference

import geekie.pgm.{NodesObservations, CPD}
import geekie.utils.BooleanCombinations
import org.scalatest.{Matchers, FlatSpec}


class FactorTest extends FlatSpec with Matchers {

  trait Fixture {
      val cpd = CPD(List("B", "C"), List(.1, .2, .3, .4))
      val factor = Factor("A", cpd)
  }

  "Factor scope" should "be equal to CPD's parent plus the node itself" in new Fixture {
    factor.scope.toList should contain theSameElementsAs(Set("A", "B", "C"))
  }

  /*
    Coordinates tests
   */

  "Getting joint" should "throw if not all observations are supplied" in new Fixture {
    intercept[IllegalArgumentException] {
      factor.coords(NodesObservations(List(
        ("A", true), ("B", false)
      ).toMap))
    }
  }

  "Coordinates for base case (all false)" should "be (0, 0, 0)" in new Fixture {
      val coords = factor.coords(NodesObservations(List(
        ("A", false), ("B", false), ("C", false)
      ).toMap))

      coords should contain theSameElementsInOrderAs List(0, 0, 0)
  }

  "Coordinates for last case (all true)" should "be (1, 1, 1)" in new Fixture {
    val coords = factor.coords(NodesObservations(List(
      ("A", true), ("B", true), ("C", true)
    ).toMap))

    coords should contain theSameElementsInOrderAs List(1, 1, 1)
  }

  "Coordinates for only A => true" should "be (1, 0, 0) [row major]" in new Fixture {
    val coords = factor.coords(NodesObservations(List(
      ("B", false), ("C", false), ("A", true)
    ).toMap))
    coords should contain theSameElementsInOrderAs List(1, 0, 0)
  }

  "Coordinates for only B => true" should "be (0, 1, 0)" in new Fixture {
    val coords = factor.coords(NodesObservations(List(
      ("B", true), ("C", false), ("A", false)
    ).toMap))
    coords should contain theSameElementsInOrderAs List(0, 1, 0)
  }

  /*
    Attribution tests
   */

  "Attribution for base case (all false)" should "match cpd's value" in new Fixture {
    val obs = NodesObservations(List(
      ("A", false), ("B", false), ("C", false)
    ).toMap)
    val joint = factor(obs)
    val expected = cpd.condProb(obs("A"), obs - "A")
    joint should equal (expected +- 1e-6)
  }

  "Attribution for last case (all true)" should "match cpd's value" in new Fixture {
    val obs = NodesObservations(List(
      ("A", true), ("B", true), ("C", true)
    ).toMap)
    val joint = factor(obs)
    val expected = cpd.condProb(obs("A"), obs - "A")
    joint should equal (expected +- 1e-6)
  }

  "Attribution for only A => true" should "match cpd's value" in new Fixture {
    val obs = NodesObservations(List(
      ("C", false), ("A", true), ("B", false)
    ).toMap)
    val joint = factor(obs)
    val expected = cpd.condProb(true, obs - "A")
    joint should equal (expected +- 1e-6)
  }

  "Attribution for only B => true" should "match cpd's value" in new Fixture {
    val obs = NodesObservations(List(
      ("C", false), ("A", false), ("B", true)
    ).toMap)
    val joint = factor(obs)
    val expected = cpd.condProb(obs("A"), obs - "A")
    joint should equal (expected +- 1e-6)
  }

  /*
    Sumout tests
   */
  "Summing out a variable that is not in scope" should "throw exception" in new Fixture {
    intercept[IllegalArgumentException] {
      factor.sumout("D")
    }
  }

  "Summing out a variable" should "yield a Factor with one less dimension" in new Fixture {
    val coords = factor.sumout("C").coords(NodesObservations(List(
      ("B", true), ("A", true)
    ).toMap))
    coords should have length factor.scope.size - 1
  }

  "Summing out a variable" should "yield a Factor which scope does not contain that variable" in new Fixture {
    factor.sumout("C").scope.toList should contain theSameElementsInOrderAs List("A", "B")
  }

  "Summing out a variable" should "yield a Factor with the expected values" in new Fixture {
    val newFactor = factor.sumout("C")

    BooleanCombinations(newFactor.scope.size).foreach {
        case List(va, vb) => {
          val obs = NodesObservations(List(("A", va), ("B", vb)).toMap)
          newFactor(obs) should equal (
            cpd.condProb(va, obs + ("C", false)) +
            cpd.condProb(va, obs + ("C", true))
          +- 1e-6)
        }
    }
  }

  "Summing out a variable with evidence" should "yield a Factor which scope does not contain the variable" in new Fixture {
    val evidence = NodesObservations() + ("B", true)
    factor.sumout("C", evidence).scope.toList should contain theSameElementsAs List("A", "B")
  }

  "Summing out a variable with evidence" should "yield a Factor with the expected values" in new Fixture {
    val evidence = NodesObservations() + ("B", true)
    val newFactor = factor.sumout("C", evidence).sumout("B", evidence)

    BooleanCombinations(newFactor.scope.size).foreach {
      case List(va) => {
        newFactor(NodesObservations() + ("A", va)) should equal (
          cpd.condProb(va, NodesObservations() + ("A", va) + ("B", true) + ("C", false)) +
          cpd.condProb(va, NodesObservations() + ("A", va) + ("B", true) + ("C", true))
            +- 1e-6)
      }
    }
  }

  /*
    Multiplication tests
   */

  trait TwoFactorsFixture {
    val aCPD = CPD(List("B", "C"), List(.1, .2, .3, .4))
    val thisFactor = Factor("A", aCPD)

    val bCPD = CPD(List("D", "E"), List(.3, .4, .5, .6))
    val thatFactor = Factor("C", bCPD)
  }

  "Multiplying two factors" should "yield a factor which scope is the union of the two" in new TwoFactorsFixture {
    val newFactor = thisFactor * thatFactor
    newFactor.scope.toList should contain theSameElementsInOrderAs List("A", "B", "C", "D", "E")
  }

  "Multiplying two factors" should "yield a factor which dimension is the same as the lenght of its scope" in new TwoFactorsFixture {
    val newFactor = thisFactor * thatFactor
    val obs = NodesObservations(List(
      ("A", false), ("B", false), ("C", false), ("D", false), ("E", false)
    ).toMap)
    newFactor.coords(obs) should have length newFactor.scope.size
  }

  "Multiplying two factors" should "have the expected values" in new TwoFactorsFixture {
    val newFactor = thisFactor * thatFactor
    BooleanCombinations(newFactor.scope.size).foreach {
      combs => {
        val obs = NodesObservations(newFactor.scope.zip(combs).toMap)

        newFactor(obs) should equal(
          thisFactor(NodesObservations(obs.filterKeys(thisFactor.scope.contains(_)))) *
            thatFactor(NodesObservations(obs.filterKeys(thatFactor.scope.contains(_))))
        )
      }
    }
  }
}
