package geekie.pgm

import org.apache.commons.math3.stat.inference.TestUtils._
import org.scalacheck.{Gen, Prop, Properties}

object BayesNetTest extends Properties("BayesNet") {

  val minP = 1e-2
  val maxP = 1.0 - minP
  val genProbs = Gen.oneOf(Gen.const(minP), Gen.const(maxP), Gen.choose(minP, maxP), Gen.choose(minP, maxP))

  val genNet = Gen.listOfN(9, genProbs) map { l =>
    BayesNet[String](Map(
      "A" -> CPD(List[String](), List(l(0))),
      "B" -> CPD(List("A"), List(l(1), l(2))),
      "C" -> CPD(List("A"), List(l(3), l(4))),
      "D" -> CPD(List("B", "C"), List(l(5), l(6), l(7), l(8)))
    ))
  }

  property("Draw follows expected marginal probabilities") = Prop.forAll(genNet) { net =>

    val pA = net.CPDbyNode("A").condProb(true, NodesObservations(Map[String, Boolean]()))
    val pB_a = net.CPDbyNode("B").condProb(true, NodesObservations(Map("A" -> false)))
    val pB_A = net.CPDbyNode("B").condProb(true, NodesObservations(Map("A" -> true)))
    val pC_a = net.CPDbyNode("C").condProb(true, NodesObservations(Map("A" -> false)))
    val pC_A = net.CPDbyNode("C").condProb(true, NodesObservations(Map("A" -> true)))
    val pD_bc = net.CPDbyNode("D").condProb(true, NodesObservations(Map("B" -> false, "C" -> false)))
    val pD_bC = net.CPDbyNode("D").condProb(true, NodesObservations(Map("B" -> false, "C" -> true)))
    val pD_Bc = net.CPDbyNode("D").condProb(true, NodesObservations(Map("B" -> true, "C" -> false)))
    val pD_BC = net.CPDbyNode("D").condProb(true, NodesObservations(Map("B" -> true, "C" -> true)))

    val pa = 1 - pA
    val pB = pB_A * pA + pB_a * pa
    val pb = 1 - pB
    val pC = pC_A * pA + pC_a * pa
    val pc = 1 - pC

    val pb_a = 1 - pB_a
    val pc_a = 1 - pC_a
    val pb_A = 1 - pB_A
    val pc_A = 1 - pC_A

    val pbc = pb_a * pc_a * pa + pb_A * pc_A * pA
    val pbC = pb_a * pC_a * pa + pb_A * pC_A * pA
    val pBc = pB_a * pc_a * pa + pB_A * pc_A * pA
    val pBC = pB_a * pC_a * pa + pB_A * pC_A * pA

    val pD = pD_BC * pBC + pD_bC * pbC + pD_Bc * pBc + pD_bc * pbc

    val probs = Map("A" -> pA, "B" -> pB, "C" -> pC, "D" -> pD)

    val n = 10000
    val sample = (Map("A" -> 0L, "B" -> 0L, "C" -> 0L, "D" -> 0L) /: Stream.fill(n)(net.drawObservation())) {
      (acc, obs) => (acc /: obs.value) {
        case (a, (k, v)) => a + (k -> (a(k) + (if (v) 1L else 0L)))
      }
    }

    def bernoulliTest(p: Double, a: Long, n: Long, threshold: Double = 2e-4) =
      chiSquareTest(Array(1 - p, p), Array(n - a + 1, a + 1)) > threshold

    sample map { case (node, a) =>
      bernoulliTest(probs(node), a, n)
    } forall (_ == true)
  }
}
