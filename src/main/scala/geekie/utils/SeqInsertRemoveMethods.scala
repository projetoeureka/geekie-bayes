package geekie.utils

object SeqInsertRemoveMethods {
  implicit class ListUtilityMethodClass[T](ll: Seq[T]) {
    def insert(i: Int, value: T) = ll.take(i) ++ Seq(value) ++ ll.drop(i)
    def remove(i: Int) = ll.take(i) ++ ll.drop(i + 1)
  }
}
