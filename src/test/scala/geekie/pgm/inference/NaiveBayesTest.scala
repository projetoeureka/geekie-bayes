package geekie.pgm.inference

import geekie.pgm.{CPD, BayesNet, NodesObservations}
import org.scalatest.{Matchers, FlatSpec}


class NaiveBayesTest extends FlatSpec with Matchers {
  trait NaiveBayesNet {

    val net = BayesNet[String](Map(
      "A"  -> CPD(List(), List(.7)),
      "B1" -> CPD(List("A"), List(.4, .5)),
      "B2" -> CPD(List("A"), List(.4, .5)),
      "B3" -> CPD(List("A"), List(.4, .5)),
      "B4" -> CPD(List("A"), List(.4, .5))
    ))

    val nb = NaiveBayes(net)
  }

  // Helper function to assert expected values
  // Returns the probability of `A` given `Bi`s
  def expectedProb(nBTrue: Int, nBFalse: Int): Double = {
    val num = .7*Math.pow(.5, nBTrue)*Math.pow((1-.5), nBFalse)
    val den = .7*Math.pow(.5, nBTrue)*Math.pow((1-.5), nBFalse) + (1-.7)*Math.pow(.4, nBTrue)*Math.pow((1-.4), nBFalse)
    num/den
  }

  "Trying to infer more than one query variable" should "throw an exception (not implemented yet)" in new NaiveBayesNet {
    intercept[IllegalArgumentException] {
      nb.infer(Set("A", "B"), NodesObservations[String]())
    }
  }

  "Probability of 'A' with no evidence" should "be its own priori" in new NaiveBayesNet {
    val evidence = NodesObservations[String]()
    val factor = nb.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    //factor(obs) should equal (0.7 +- 1e-6)
    factor(obs) should equal (expectedProb(0, 0) +- 1e-6)
  }

  "Probability of 'A' given 'B1' true" should "match the updated priori" in new NaiveBayesNet {
    val evidence = NodesObservations() + ("B1", true)
    val factor = nb.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (expectedProb(1, 0) +- 1e-6)
  }

  "Probability of 'A' given 'B1' false" should "match the updated priori" in new NaiveBayesNet {
    val evidence = NodesObservations() + ("B1", false)
    val factor = nb.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (expectedProb(0, 1) +- 1e-6)
  }

  "Probability of 'A' with all 'Bi's true" should "match the expectation" in new NaiveBayesNet {
    val evidence = NodesObservations(Map(
      "B1" -> true,
      "B2" -> true,
      "B3" -> true,
      "B4" -> true
    ))
    val factor = nb.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (expectedProb(4, 0) +- 1e-6)
  }

  "Probability of 'A' with 2 'Bi's true and 2 'Bi's false" should "match the expectation" in new NaiveBayesNet {
    val evidence = NodesObservations(Map(
      "B1" -> true,
      "B2" -> true,
      "B3" -> false,
      "B4" -> false
    ))
    val factor = nb.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (expectedProb(2, 2) +- 1e-6)
  }
}

