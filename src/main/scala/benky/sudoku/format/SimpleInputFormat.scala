package benky.sudoku.format

import io.Source

/**
 * Reads Sudoku puzzle input values from given file. Empty places in Sudoku should be filled by '0'.
 */
class SimpleInputFormat extends InputFormat {
  def readFile(name: String) = Source
    .fromFile(name)
    .getLines()
    .map(_.zipWithIndex)
    .zipWithIndex
    .flatMap {
    case (line, row) => line.toList.filter(_._1 != '0').map {
      case (c, column) => (row, column, c.toInt - 48)
    }
  }

}
