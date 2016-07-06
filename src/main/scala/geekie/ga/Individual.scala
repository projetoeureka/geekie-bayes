package geekie.ga


trait Individual[T <: Individual[T]] {
  def fitness: Double

  def reproduce(mate: T): T = crossover(mate).mutate

  def crossover(mate: T): T

  def mutate: T
}
