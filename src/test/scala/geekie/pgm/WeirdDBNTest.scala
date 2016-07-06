package geekie.pgm

import geekie.pgm.inference.Enumeration
import org.scalatest.{Matchers, FlatSpec}


class WeirdDBNTest extends FlatSpec with Matchers {

  trait Fixture {
    val knowledgeGraph = BayesNet[String](Map(
      "K1" -> CPD(List(), List(.4)),
      "K2" -> CPD(List("K1"), List(.4, .6))
    ))

    val priors = Map("K1" -> .4, "K2" -> .4)

    def priorFactory(node: String, priors: Map[String, Double]): (String, CPD[String]) =
      (node+"_T-1", CPD(List(), List(priors(node))))

    def transitionModelFactory(node: String): CPD[String] =
      CPD(List(node+"_T-1"), List(.4, .99))

    def sensorModelFactory(node: String): (String, CPD[String]) =
      ("A_"+node, CPD(List(node), List(.2, .99)))

    val dbn = WeirdDBN(knowledgeGraph, priors, priorFactory, transitionModelFactory, sensorModelFactory)

    def inferenceFactory(bayesNet: BayesNet[String]) = Enumeration(bayesNet)
  }

  "The unrolled Bayesian Net" should "contain priors and transition models" in new Fixture {
    dbn.unrolledNet.CPDbyNode.keys should contain theSameElementsAs Set("K1", "K2", "K1_T-1", "K2_T-1")
  }

  it should "have merged the transition model for K1 correctly" in new Fixture {
    val K1CPD = dbn.unrolledNet.CPDbyNode("K1")
    K1CPD.parents should contain theSameElementsAs Set("K1_T-1")

    K1CPD.condProb(true, NodesObservations() + ("K1_T-1", false)) should equal (.4 +- 1e-6)
    K1CPD.condProb(true, NodesObservations() + ("K1_T-1", true)) should equal (.99 +- 1e-6)
  }

  it should "have merged the transition model for K2 correctly" in new Fixture {
    val K2CPD = dbn.unrolledNet.CPDbyNode("K2")
    K2CPD.parents should contain theSameElementsAs Set("K1", "K2_T-1")

    K2CPD.condProb(true, NodesObservations() + ("K1", false) + ("K2_T-1", false)) should equal (.3076 +- 1e-4)
    K2CPD.condProb(true, NodesObservations() + ("K1", false) + ("K2_T-1", true)) should equal (.9850 +- 1e-4)
    K2CPD.condProb(true, NodesObservations() + ("K1", true) + ("K2_T-1", false)) should equal (.5000 +- 1e-4)
    K2CPD.condProb(true, NodesObservations() + ("K1", true) + ("K2_T-1", true)) should equal (.9933 +- 1e-4)
  }

  "Stepping" should "produce the right updated priors for evidence K2 = false" in new Fixture {
    val dbn_tplus1 = dbn.step("K2", false, inferenceFactory)

    // Values extracted from modeling this Bayesian network with SamIam
    dbn_tplus1.priors("K1") should equal (.5593 +- 1e-4)
    dbn_tplus1.priors("K2") should equal (.0230 +- 1e-4)
  }

  "Stepping" should "produce the right updated priors for evidence K2 = true" in new Fixture {
    val dbn_tplus1 = dbn.step("K2", true, inferenceFactory)

    // Values extracted from modeling this Bayesian network with SamIam
    dbn_tplus1.priors("K1") should equal (.6662 +- 1e-4)
    dbn_tplus1.priors("K2") should equal (.9034 +- 1e-4)
  }

  "Stepping" should "produce the right updated priors for evidence K1 = false" in new Fixture {
    val dbn_tplus1 = dbn.step("K1", false, inferenceFactory)

    // Values extracted from modeling this Bayesian network with SamIam
    dbn_tplus1.priors("K1") should equal (.0214 +- 1e-4)
    dbn_tplus1.priors("K2") should equal (.5811 +- 1e-4)
  }

  "Stepping" should "produce the right updated priors for evidence K1 = true" in new Fixture {
    val dbn_tplus1 = dbn.step("K1", true, inferenceFactory)

    // Values extracted from modeling this Bayesian network with SamIam
    dbn_tplus1.priors("K1") should equal (.8964 +- 1e-4)
    dbn_tplus1.priors("K2") should equal (.6850 +- 1e-4)
  }
}
