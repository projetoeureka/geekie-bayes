package geekie.pgm.inference

import org.scalatest.{Matchers, FlatSpec}


class ScopeTest extends FlatSpec with Matchers {

  "Scope" should "be instantiatable from traversables" in {
    val scope = Scope(Traversable("A", "B", "C"))
    scope.size should equal (3)
  }

  "Iterating on a scope" should "go over nodes in order" in {
    val scope = Scope(Traversable("B", "Z", "A"))
    scope.toList should equal (List("A", "B", "Z"))
  }

  "Scope" should "be able to be iterated over multiple times" in {
    val scope = Scope(Traversable("B", "Z", "A"))
    scope.map(n => n).toList should equal (List("A", "B", "Z"))
    scope.map(n => n).toList should equal (List("A", "B", "Z"))
  }

  "Zipping boolean combinations with the scope" should "produce list in expected order" in {
    val scope = Scope(Traversable("B", "C", "A"))
    val zipped = scope.zip(List(false, false, true))
    zipped.toList should equal (List(("A", false), ("B", false), ("C", true)))
  }

  trait Fixture {
    val scope1 = Scope(Seq("C", "A", "B"))
    val scope2 = Scope(Seq("D", "C", "E"))
  }

  "Adding two scopes" should "produce a 'union' scope" in new Fixture {
    val scope = scope1 ++ scope2
    scope.toList should contain theSameElementsAs Set("A", "B", "C", "D", "E")
  }

  "Subtracting two scopes" should "produce a 'diff' scope" in new Fixture {
    val scope = scope1 -- scope2
    scope.toList should contain theSameElementsAs Set("A", "B")
  }

  "Adding a variable that already is in scope" should "not change it's scope" in new Fixture {
    val scope = scope1 + "A"
    scope.toList should contain theSameElementsAs Set("A", "B", "C")
  }

  "Adding a variable" should "work" in new Fixture {
    val scope = scope1 + "D"
    scope.toList should contain theSameElementsAs Set("D", "C", "B", "A")
  }

  "Subtracting a variable" should "produce a new scope without it" in new Fixture {
    (scope1 - "C").toList should contain theSameElementsAs Set("A", "B")
  }

  "Subtracting a variable that is not in scope" should "not cause errors" in new Fixture {
    (scope1 - "D").toList should contain theSameElementsAs Set("A", "B", "C")
  }

}
