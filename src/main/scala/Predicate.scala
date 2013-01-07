abstract sealed case class Predicate()

case class VAR(x: Int, y: Int, value: Int) extends Predicate

case class NOT(p: Predicate) extends Predicate