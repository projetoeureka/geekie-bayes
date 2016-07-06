package geekie.utils

object ListSwapMethod {

  implicit class ListSwapMethodClass[T](ll: List[T]) {
    def swap(a: Int, b: Int) = ll.updated(a, ll(b)).updated(b, ll(a))
  }
}
