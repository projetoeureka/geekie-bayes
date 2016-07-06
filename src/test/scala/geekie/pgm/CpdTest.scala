package geekie.pgm

import org.apache.commons.math3.stat.inference.TestUtils._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Prop, Properties}


object CpdTest extends Properties("CPD") {

  def nodeList3 = List("Na", "Nb", "Nc")

  def genCpd = Gen.listOfN(8, Gen.choose(0.0, 1.0))

  def genCpdExtreme = Gen.listOfN(8, Gen.oneOf(0.0, 1.0))

  def genBool3 = Gen.listOfN(3, arbitrary[Boolean])

  property("Draw follows the probability") = Prop.forAll(genCpd, genBool3) {
    (pdf, ans) =>
      val obs = NodesObservations(nodeList3 zip ans toMap)
      val cpd = CPD(nodeList3, pdf)

      val n = 1000
      val a: Long = Stream.fill(n)(if (cpd.draw(obs)) 1 else 0) sum

      val p = cpd.condProb(nodeObservation = true, obs)

      val pValue = chiSquareTest(Array(1 - p, p), Array(n - a, a))

      pValue > 0.001
  }

  property("Draw obeys extremes") = Prop.forAll(genCpdExtreme, genBool3) {
    (pdf, ans) =>
      val obs = NodesObservations(nodeList3 zip ans toMap)
      val cpd = CPD(nodeList3, pdf)

      val n = 1000
      val a: Long = Stream.fill(n)(if (cpd.draw(obs)) 1 else 0) sum

      val p = cpd.condProb(nodeObservation = true, obs)

      p == 0.0 && a == 0 || p == 1.0 && a == n
  }

  property("Probabilities add up to unity") = Prop.forAll(genCpd, genBool3) {
    (pdf, ans) =>
      val obs = NodesObservations(nodeList3 zip ans toMap)
      val cpd = CPD(nodeList3, pdf)

      val p = cpd.condProb(nodeObservation = true, obs)
      val pneg = cpd.condProb(nodeObservation = false, obs)

      p + pneg == 1
  }

}
