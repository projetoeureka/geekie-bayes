package geekie.pgm

import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Arbitrary.arbitrary

import scala.util.Random

object FreqTableSpecification extends Properties("FreqTable") {

  def nodeList3 = List("nA", "nB", "nC")

  def genFreq = Gen.listOfN(8, Gen.choose(0L, 10000L))

  property("Addition") = Prop.forAll(genFreq, genFreq) {
    (v, w) =>
      val res = v zip w map { case (a, b) => a + b }

      val ftA = FreqTable(nodeList3, v)
      val ftB = FreqTable(nodeList3, w)
      val ftC = ftA + ftB

      ftA + ftB == ftC && ftC.nodes == nodeList3
  }

  property("Build from boolean") = Prop.forAll(Gen.listOfN(3, arbitrary[Boolean])) {
    (byte: Seq[Boolean]) =>
      val sel = (0 /: byte) { (acc, b) => 2 * acc + (if (b) 1 else 0) }
      val expected = List.fill(8)(0L).updated(sel, 1L)

      val ftA = FreqTable.fromBoolean(nodeList3, byte)
      val ftB = FreqTable(nodeList3, expected)

      ftA == ftB
  }

  property("Marginalization") = Prop.forAll(genFreq) {
    (ll) =>
      val ft = FreqTable(nodeList3, ll)
      val ta = ft.marginalize("nA").counts == List(ll(0) + ll(4), ll(1) + ll(5), ll(2) + ll(6), ll(3) + ll(7))
      val tb = ft.marginalize("nB").counts == List(ll(0) + ll(2), ll(1) + ll(3), ll(4) + ll(6), ll(5) + ll(7))
      val tc = ft.marginalize("nC").counts == List(ll(0) + ll(1), ll(2) + ll(3), ll(4) + ll(5), ll(6) + ll(7))
      val total = (ft /: Random.shuffle(nodeList3)) { (t, n) => t.marginalize(n) }
      val td = total.counts(0) == ll.sum
      ta && tb && tc && td
  }
}
