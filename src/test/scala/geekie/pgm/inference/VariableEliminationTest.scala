package geekie.pgm.inference

import geekie.pgm.{NodesObservations, CPD, BayesNet}
import org.scalatest.{Matchers, FlatSpec}


class VariableEliminationTest extends FlatSpec with Matchers {
  trait SimpleNet {
    val net = BayesNet[String](Map(
      "A" -> CPD(List("B", "C"), List(.1, .2, .3, .4)),
      "B" -> CPD(List(), List(.5)),
      "C" -> CPD(List(), List(.6))
    ))
    val ve = VariableElimination(net)
  }

  "Probability of 'A' with no evidence" should "match expectation" in new SimpleNet {
    val evidence = NodesObservations[String]()
    val factor = ve.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.26 +- 1e-6)
  }

  "Probability of 'A' with evidence 'C' is false" should "match expectation" in new SimpleNet {
    val evidence = NodesObservations() + ("C", false)
    val factor = ve.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.2 +- 1e-6)
  }

  "Probability of 'B' with evidence 'A' is true" should "match expectation" in new SimpleNet {
    val evidence = NodesObservations() + ("A", true)
    val factor = ve.infer(Set("B"), evidence)

    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.692307 +- 1e-6)
  }

  trait SimpleTreeNet {
    val net = BayesNet[String](Map(
      "A" -> CPD(List(), List(.9)),
      "B" ->  CPD(List("A"), List(.8, .6)),
      "C" ->  CPD(List("A"), List(.1, .3)),
      "D" ->  CPD(List("C"), List(.7, .9))
    ))
    val ve = VariableElimination(net)
  }

  "Probability of A with no evidence" should "be the prior" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = ve.infer(Set("A"), evidence)
    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.9 +- 1e-6)
  }

  "Probability of B with no evidence" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = ve.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.62 +- 1e-6)
  }

  "Probability of C with no evidence" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = ve.infer(Set("C"), evidence)
    val obs = NodesObservations() + ("C", true)
    factor(obs) should equal (0.28 +- 1e-6)
  }

  "Probability of B with evidence A true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("A", true)
    val factor = ve.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.6 +- 1e-6)
  }

  "Probability of D with evidence B true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("B", true)
    val factor = ve.infer(Set("D"), evidence)
    val obs = NodesObservations() + ("D", true)
    factor(obs) should equal (0.754838 +- 1e-6)
  }

  "Probability of B with evidence C true and D false" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("C", true) + ("D", false)
    val factor = ve.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.607142 +- 1e-6)
  }

  trait AlarmNet {
    val net = BayesNet[String](Map(
      "B" -> CPD(List(), List(.001)),
      "E" -> CPD(List(), List(.002)),
      "A" -> CPD(List("B", "E"), List(.001, .29, .94, .95)),
      "J" -> CPD(List("A"), List(.05, .9)),
      "M" -> CPD(List("A"), List(.01, .7))
    ))

    val ve = VariableElimination(net)
  }

  "Probability of 'A'larm ringing with no evidence" should "be as expected" in new AlarmNet {
    val prob = ve.infer(Set("A"), NodesObservations[String]())
    val obs = NodesObservations() + ("A", true)
    prob(obs) should equal (0.00251 +- 1e-5)
  }

  "Probability of 'B'urglary with everything else observed" should "be correct" in new AlarmNet {
    val evidence = NodesObservations(Map(
      "E" -> false,
      "A" -> true,
      "M" -> true,
      "J" -> false
    ))
    val factor = ve.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should be (.484785972 +- 1e-6)
  }

  "Probability of 'B'urglary with observed 'M'ary and 'J'ohn" should "be correct" in new AlarmNet {
    val evidence = NodesObservations(Map(
      "M" -> true,
      "J" -> true
    ))
    val factor = ve.infer(Set("B"), evidence)

    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.284171 +- 1e-6)
  }


  "Probability of 'M'ary calling with everything else observed" should "be correct" in new AlarmNet {
    val evidence = NodesObservations(Map(
      "B" -> false,
      "E" -> false,
      "A" -> true,
      "J" -> false
    ))
    val factor = ve.infer(Set("M"), evidence)

    val obs = NodesObservations() + ("M", true)
    factor(obs) should be (.7 +- 1e-6)
  }

  "Probability of 'A'larm with unobserved 'M'ary" should "be correct" in new AlarmNet {
    val evidence = NodesObservations(Map(
      "E" -> false,
      "B" -> false,
      "J" -> true
    ))
    val factor = ve.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should be (0.017699 +- 1e-6)
  }

  "Probability of 'B'urglary with no evidence" should "match expectation" in new AlarmNet {
    val factor = ve.infer(Set("B"), NodesObservations[String]())

    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.001 +- 1e-5)
  }

}
