package geekie.pgm

import org.scalatest.{Matchers, FlatSpec}


class ModelProviderTest extends FlatSpec with Matchers {

  trait Fixture {
    val fts = List(
                                   //00  01  10  11
      FreqTable(List("A", "B"), List( 2,  3,  7, 11)),

                                        //000  001  010  011  100  101  110  111
      FreqTable(List("A", "B", "C"), List(  1,   2,   3,   4,   5,   6,   7,   8))
    )
    val mp = ModelProvider(fts.toIterator)
  }

  "ModelProvider" should "produce the expected CPDs for P(A|B)" in new Fixture {
    val b0: Double = 2 + 7
    val b1: Double = 3 + 11
    val expectedCPDForA = CPD(List("B"), List(7/b0, 11/b1))

    mp.CPDforParents("A", Set("B")) should equal (expectedCPDForA)
  }

  "it" should "produce the expected CPD for P(A|B, C)" in new Fixture {
    val bc00: Double = 1 + 5
    val bc01: Double = 2 + 6
    val bc10: Double = 3 + 7
    val bc11: Double = 4 + 8

    val expectedCPDForA = CPD(List("B", "C"), List(5/bc00, 6/bc01, 7/bc10, 8/bc11))

    mp.CPDforParents("A", Set("B", "C")) should equal (expectedCPDForA)
  }

}
