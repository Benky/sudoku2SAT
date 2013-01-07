package benky.sudoku.format

import benky.sudoku.Predicate

trait OutputFormat {

  def format(lines: Seq[Seq[Predicate]])

}
