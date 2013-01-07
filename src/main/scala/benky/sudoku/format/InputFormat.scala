package benky.sudoku.format

trait InputFormat {
  def readFile(fileName: String): Iterator[(Int, Int, Int)]
}
