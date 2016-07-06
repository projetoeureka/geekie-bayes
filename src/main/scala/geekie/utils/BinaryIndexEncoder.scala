package geekie.utils

object BinaryIndexEncoder {
  def apply(index: Int, wordLength: Int) =
    for (b <- wordLength -1 to 0 by -1) yield (index >> b) % 2 == 1
}
