package geekie.utils

object BooleanCombinations {
  def apply(nBits: Int): List[List[Boolean]] =
    if (nBits == 0) List(List())
    else for {
      b <- List(false, true)
      rest <- BooleanCombinations(nBits - 1)
    } yield b :: rest

  /**
   * `nBits` boolean combinations but with the least significant byte first
   * @param nBits
   * @return
   */
  def rev(nBits: Int): List[List[Boolean]] = apply(nBits).map(_.reverse)
}
