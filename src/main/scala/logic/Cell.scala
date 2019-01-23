package logic

/**
  * Class Cell represents a one of the 81 cells in a sudoku matrix
  *
  * @param lineNr       line and column number define the position of the cell
  * @param columnNr
  * @param value        The number printed on this cell (given or figured out)
  * @param valueOptions List of remaining options for this cell
  */


case class Cell(val lineNr: Int, val columnNr: Int, var value: Int = 0, var valueOptions: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)) {

  /**
    * Construct using only the position information of the cell
    *
    * @param lineNr
    * @param columnNr
    */
  def this(lineNr: Int, columnNr: Int) {
    this(lineNr, columnNr, 0, List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  //The 3x3 box, where the cell is:
  val boxNr = (lineNr / 3) * 3 + columnNr / 3


  def ==(that: Cell): Boolean =
    this.columnNr == that.columnNr &&
      this.lineNr == that.lineNr


  def value_(newValue: Int): Boolean =

    if (newValue == 0 || this.value != 0 || !valueOptions.contains(newValue))
      false
    else {
      value = newValue
      valueOptions = valueOptions.filter(_ == newValue)
      true
    }


  /**
    * Comparing the options of two cells, normally sharing a line, a column or a box
    * If n cells share the same list of n options
    * then other cells in the common area con't have a value from these options.
    * @param that  the other cell
    * @return
    */
  def sameOptions(that: Cell) =
    this.valueOptions.forall(o => that.valueOptions.contains(o)) &&
      this.valueOptions.length == that.valueOptions.length

}
