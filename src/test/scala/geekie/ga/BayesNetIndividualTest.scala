package geekie.ga

import geekie.pgm._
import org.scalatest.{FlatSpec, Matchers}


class BayesNetIndividualTest extends FlatSpec with Matchers {

trait Fixtures {
  val maxParents = 3

  val originalbayesNet = BayesNet[String](Map(
    "A" -> CPD(List[String](), List(0.3)),
    "B" -> CPD(List[String](), List(0.7)),
    "C" -> CPD(List("A", "B"), List(0.1, 0.5, 0.3, 0.9))
  ))

  val originalSample = List.fill(10000)(originalbayesNet.drawObservation())

  val modelProvider = ModelProvider(FreqTable.combinationsFromSample(originalSample, maxParents))
  val sampleFreqTables = FreqTable.combinationsFromSample(originalSample.take(100), maxParents)
  val logLikelihoodProvider = LogLikelihoodProvider(modelProvider, sampleFreqTables.toList)

  val indiv1 = BayesNetIndividual.makeRandomIndividual(
    List("A", "B"),
    modelProvider,
    logLikelihoodProvider,
    .1,
    .2,
    maxParents)

  val indiv2 = BayesNetIndividual.makeRandomIndividual(
    List("B", "C"),
    modelProvider,
    logLikelihoodProvider,
    .1,
    .2,
    maxParents)
  }

  "BayesNetIndividual" should "be tested" in {

  }
}
