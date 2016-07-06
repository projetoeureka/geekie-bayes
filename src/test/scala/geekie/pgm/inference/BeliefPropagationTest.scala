package geekie.pgm.inference

import geekie.pgm.{NodesObservations, CPD, BayesNet}
import org.scalatest.{Matchers, FlatSpec}


class BeliefPropataionTest extends FlatSpec with Matchers {

  "Trying to use BP on a non-tree" should "throw an exception" in {
    val net = BayesNet[String](Map(
      "A" -> CPD(List(), List(.9)),
      "B" -> CPD(List("A"), List(.8, .6)),
      "C" -> CPD(List("A", "B"), List(.1, .3, .8, .4))
    ))
    intercept[IllegalArgumentException] {
      BeliefPropagation(net).infer("A", NodesObservations[String]())
    }
  }

  trait SimpleTreeNet {
    val net = BayesNet[String](Map(
      "A" -> CPD(List(), List(.9)),
      "B" -> CPD(List("A"), List(.8, .6)),
      "C" -> CPD(List("A"), List(.1, .3)),
      "D" -> CPD(List("C"), List(.7, .9))
    ))
    val bp = BeliefPropagation(net)
  }

  "Trying to infer more than one query variable" should "throw an exception (not implemented yet)" in new SimpleTreeNet {
    intercept[UnsupportedOperationException] {
      bp.infer(Set("A", "B"), NodesObservations[String]())
    }
  }

  "Probability of A with no evidence" should "be the prior" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = bp.infer(Set("A"), evidence)
    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.9 +- 1e-6)
  }

  "Probability of B with no evidence" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = bp.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.62 +- 1e-6)
  }

  "Probability of C with no evidence" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = bp.infer(Set("C"), evidence)
    val obs = NodesObservations() + ("C", true)
    factor(obs) should equal (0.28 +- 1e-6)
  }

  "Probability of B with evidence A true" should "match B's CPD" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("A", true)
    val factor = bp.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.6 +- 1e-6)
  }

  "Probability of D with evidence B true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("B", true)
    val factor = bp.infer(Set("D"), evidence)
    val obs = NodesObservations() + ("D", true)
    factor(obs) should equal (0.754838 +- 1e-6)
  }

  "Probability of B with evidence C true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("C", true)
    val factor = bp.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.607142 +- 1e-6)
  }

  "Probability of B with evidence C true and D false" should "be the same as only C true evidence" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("C", true) + ("D", false)
    val factor = bp.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.607142 +- 1e-6)
  }

  "Probability of B with evidence D false" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("D", false)
    val factor = bp.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.622950 +- 1e-6)
  }

  "Probability of C with evidence A false and D true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("A", false) + ("D", true)
    val factor = bp.infer(Set("C"), evidence)
    val obs = NodesObservations() + ("C", true)
    factor(obs) should equal (0.125 +- 1e-6)
  }

  "Two consecutive inferences" should "produce the expected results (tree is 'reset')" in new SimpleTreeNet {
    val evidence1 = NodesObservations[String]() + ("A", false) + ("D", true)
    val factor1 = bp.infer(Set("C"), evidence1)
    val obs1 = NodesObservations() + ("C", true)
    factor1(obs1) should equal (0.125 +- 1e-6)

    val evidence2 = NodesObservations[String]() + ("C", true) + ("D", false)
    val factor2 = bp.infer(Set("B"), evidence2)
    val obs2 = NodesObservations() + ("B", true)
    factor2(obs2) should equal (0.607142 +- 1e-6)
  }

}
