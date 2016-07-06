package geekie.pgm.inference

import geekie.pgm.{NodesObservations, CPD, BayesNet}
import org.scalatest.{Matchers, FlatSpec}


class EnumerationTest extends FlatSpec with Matchers {
  trait SimpleNet {
    val net = BayesNet[String](Map(
      "A" -> CPD(List("B", "C"), List(.1, .2, .3, .4)),
      "B" -> CPD(List(), List(.5)),
      "C" -> CPD(List(), List(.6))
    ))
    val en = Enumeration(net)
  }

  "Trying to infer more than one query variable" should "throw an exception (not implemented yet)" in new SimpleNet {
    intercept[UnsupportedOperationException] {
      en.infer(Set("A", "B"), NodesObservations[String]())
    }
  }

  "Probability of 'A' with no evidence" should "match expectation" in new SimpleNet {
    val evidence = NodesObservations[String]()
    val factor = en.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.26 +- 1e-6)
  }

  "Probability of 'A' with evidence 'C' is false" should "match expectation" in new SimpleNet {
    val evidence = NodesObservations() + ("C", false)
    val factor = en.infer(Set("A"), evidence).normalize

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.2 +- 1e-6)
  }

  "Probability of 'B' with evidence 'A' is true" should "match expectation" in new SimpleNet {
    val evidence = NodesObservations() + ("A", true)
    val factor = en.infer(Set("B"), evidence).normalize

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
    val en = Enumeration(net)
  }

  "Probability of A with no evidence" should "be the prior" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = en.infer(Set("A"), evidence)
    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.9 +- 1e-6)
  }

  "Probability of B with no evidence" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = en.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.62 +- 1e-6)
  }

  "Probability of C with no evidence" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]()
    val factor = en.infer(Set("C"), evidence)
    val obs = NodesObservations() + ("C", true)
    factor(obs) should equal (0.28 +- 1e-6)
  }

  "Probability of B with evidence A true" should "match B's CPD" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("A", true)
    val factor = en.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.6 +- 1e-6)
  }

  "Probability of D with evidence B true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("B", true)
    val factor = en.infer(Set("D"), evidence)
    val obs = NodesObservations() + ("D", true)
    factor(obs) should equal (0.754838 +- 1e-6)
  }

  "Probability of B with evidence C true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("C", true)
    val factor = en.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.607142 +- 1e-6)
  }

  "Probability of B with evidence C true and D false" should "be the same as only C true evidence" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("C", true) + ("D", false)
    val factor = en.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.607142 +- 1e-6)
  }

  "Probability of B with evidence D false" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("D", false)
    val factor = en.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.622950 +- 1e-6)
  }

  "Probability of C with evidence A false and D true" should "match expectation" in new SimpleTreeNet {
    val evidence = NodesObservations[String]() + ("A", false) + ("D", true)
    val factor = en.infer(Set("C"), evidence)
    val obs = NodesObservations() + ("C", true)
    factor(obs) should equal (0.125 +- 1e-6)
  }

  trait AlarmNet {
    val net = BayesNet[String](Map(
      "B" -> CPD(List(), List(.001)),
      "E" -> CPD(List(), List(.002)),
      "A" -> CPD(List("B", "E"), List(.001, .29, .94, .95)),
      "J" -> CPD(List("A"), List(.05, .9)),
      "M" -> CPD(List("A"), List(.01, .7))
    ))

    val en = Enumeration(net)
  }

  "Probability of 'A'larm ringing with no evidence" should "be as expected" in new AlarmNet {
    val prob = en.infer(Set("A"), NodesObservations[String]())
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
    val factor = en.infer(Set("B"), evidence)
    val obs = NodesObservations() + ("B", true)
    factor(obs) should be (.484785972 +- 1e-6)
  }

  "Probability of 'B'urglary with observed 'M'ary and 'J'ohn" should "be correct" in new AlarmNet {
    val evidence = NodesObservations(Map(
      "M" -> true,
      "J" -> true
    ))
    val factor = en.infer(Set("B"), evidence)

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
    val factor = en.infer(Set("M"), evidence)

    val obs = NodesObservations() + ("M", true)
    factor(obs) should be (.7 +- 1e-6)
  }

  "Probability of 'A'larm with unobserved 'M'ary" should "be correct" in new AlarmNet {
    val evidence = NodesObservations(Map(
      "E" -> false,
      "B" -> false,
      "J" -> true
    ))
    val factor = en.infer(Set("A"), evidence)

    val obs = NodesObservations() + ("A", true)
    factor(obs) should be (0.017699 +- 1e-6)
  }

  "Probability of 'B'urglary with no evidence" should "match expectation" in new AlarmNet {
    val factor = en.infer(Set("B"), NodesObservations[String]())

    val obs = NodesObservations() + ("B", true)
    factor(obs) should equal (0.001 +- 1e-6)
  }


  trait NaiveBayesNet {
    val net = BayesNet[String](Map(
      "A"  -> CPD(List(), List(.7)),
      "B1" -> CPD(List("A"), List(.4, .5)),
      "B2" -> CPD(List("A"), List(.4, .5)),    // ... because each factor sums out to 1.0
      "B3" -> CPD(List("A"), List(.4, .5)),
      "B4" -> CPD(List("A"), List(.4, .5))
    ))
    val en = Enumeration(net)
  }

  "Naive probability of 'A' with no evidence" should "match its priori" in new NaiveBayesNet {
    // ... because each factor sums out to 1.0
    val factor = en.infer(Set("A"), NodesObservations[String]())

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal (0.7 +- 1e-6)
  }

  "Naive probability of 'A' with 'B1' observed true" should "match the expected value" in new NaiveBayesNet {
    val factor = en.infer(Set("A"), NodesObservations() + ("B1", true))

    val obs = NodesObservations() + ("A", true)
    factor(obs) should equal ((.7*.5)/(.7*.5 + .3*.4) +- 1e-6)
  }
}
