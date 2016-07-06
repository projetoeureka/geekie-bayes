package geekie.pgm

import org.scalatest.{Matchers, FlatSpec}


class LogLikelihoodProviderTest extends FlatSpec with Matchers {

  trait Fixture {
    val modelFts = List(
      FreqTable(List("A", "B"), List(100,  350,  280, 120))
    )

    val sampleFts = List(
      FreqTable(List("A", "B"), List(11,  20,  17, 15))
    )

    val modelProvider = ModelProvider(modelFts.toIterator)
    val logLikelihoodProvider = LogLikelihoodProvider(ModelProvider(modelFts.toIterator), sampleFts)
  }

  "LogLikelihoodProvider" should "produce the expected log-likelihood for sample" in new Fixture {
    val parentsByNode = Map("A" -> Set("B"))

    val modelCPD = modelProvider.CPDforParents("A", Set("B"))

    val observations = List(
      NodesObservations() +("A", false) +("B", false),
      NodesObservations() +("A", false) +("B", true),
      NodesObservations() +("A", true) +("B", false),
      NodesObservations() +("A", true) +("B", true)
    )

    val expectedLogLikelihood = (for (obs <- observations) yield {
      val countsInSample = sampleFts(0).getObservationCounts(obs)
      val obsLikelihood = modelCPD.condProb(obs("A"), obs - "A")
      countsInSample * Math.log(obsLikelihood)
    }) sum

    logLikelihoodProvider.logLikelihood(parentsByNode) should equal (expectedLogLikelihood +- 1e-6)
  }
}

