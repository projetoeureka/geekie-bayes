package geekie.utils

object BinaryIndexDecoder {
  def apply(observations: Seq[Boolean]) =
    (0 /: observations) { (acc: Int, obs: Boolean) =>
      acc * 2 + (if (obs) 1 else 0)
    }
}
