package geekie.pgm

import org.scalatest._


class DagTest extends FlatSpec with Matchers {

  "A DAG" should "allow edges to be inserted" in {
    val xx = DAG(Set(0, 1, 2, 3))
      .append(Edge(0, 1))
      .append(Edge(1, 2))
      .append(Edge(2, 3))
    xx.remove(Edge(0, 1)).remove(Edge(1, 2)).remove(Edge(2, 3)) should be(DAG(Set(0, 1, 2, 3)))
  }

  it should "detect cycles" in {
    val xx = DAG(Set(0, 1, 2, 3))
      .append(Edge(0, 1))
      .append(Edge(1, 2))
      .append(Edge(2, 3))
    xx.canAppend(Edge(0, 2)) should be(right = true)
    xx.canAppend(Edge(0, 3)) should be(right = true)
    xx.canAppend(Edge(1, 3)) should be(right = true)
    xx.canAppend(Edge(2, 0)) should be(right = false)
    xx.canAppend(Edge(3, 0)) should be(right = false)
    xx.canAppend(Edge(3, 1)) should be(right = false)
    xx.remove(Edge(1, 2)).canAppend(Edge(3, 1)) should be(right = true)
  }

  it should "throw cycleException when you try to close a cycle" in {
    val xx = DAG(Set(0, 1, 2, 3))
      .append(Edge(0, 1))
      .append(Edge(1, 2))
      .append(Edge(2, 3))
    a[cycleException] should be thrownBy {
      val yy = xx.append(Edge(3, 0))
    }
  }

  it should "throw nodeException when you try to add and edge with an invalid node" in {
    val xx = DAG(Set(0, 1, 2, 3))
      .append(Edge(0, 1))
      .append(Edge(1, 2))
    a[nodeNotFoundException] should be thrownBy {
      val yy = xx.append(Edge(2, 4))
    }
  }

  it should "produce correct children and parents" in {
    val xx = DAG(Set(0, 1, 2, 3, 4))
      .append(Edge(0, 1))
      .append(Edge(3, 1))
      .append(Edge(1, 2))
      .append(Edge(1, 4))

    xx.children(1) should contain theSameElementsAs(Set(0, 3))
    xx.parents(1) should contain theSameElementsAs(Set(2, 4))
  }
}
