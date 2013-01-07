package benky.sudoku.format

import benky.sudoku.{NOT, VAR, Predicate}

/**
 * Prints given collection of predicates as a human readable bool expression
 */
class BoolExpressionOutputFormat extends OutputFormat {
  def format(lines: Seq[Seq[Predicate]]) {
    val convertedLines = lines.map(line => "( " + line.map(printableVar(_)).mkString(" OR ") + " )")
    println(convertedLines.sortBy(-_.length).mkString(" AND \n"))
  }

  /**
   * Converts given predicate to human readable string
   */
  def printableVar(p: Predicate): String = p match {
    case VAR(x, y, v) => "S_%s_%s_%s".format(x, y, v)
    case NOT(pp) => "NOT(" + printableVar(pp) + ")"
  }

}
