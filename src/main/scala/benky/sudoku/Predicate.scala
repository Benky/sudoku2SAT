package benky.sudoku

/**
 * Simple representation of predicate
 */
abstract sealed case class Predicate()

/**
 * Variable
 * @param x row
 * @param y column
 * @param value value
 */
case class VAR(x: Int, y: Int, value: Int) extends Predicate

/**
 * Negation of given predicate
 * @param p predicate
 */
case class NOT(p: Predicate) extends Predicate
