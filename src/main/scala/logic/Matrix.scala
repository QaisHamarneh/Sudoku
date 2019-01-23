package logic


class Matrix(val size: Int) {

  val matrixArray: Array[Array[Cell]] = Array.ofDim[Cell](size, size)

  def initMatrix() = {
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        matrixArray(i)(j) = Cell(i, j)
      }
    }
  }

  initMatrix()

  /**
    * for a given cell, return an array of the cells on the same column
    *
    * @param cell
    * @return an Array of Cells
    */

  def lineArray(cell: Cell): Array[Cell] = {
    return matrixArray(cell.lineNr).filter(_.columnNr != cell.columnNr)
  }

  /**
    * for a given cell, return an array of the cells on the same column
    *
    * @param cell
    * @return an Array of Cells
    */
  def columnArray(cell: Cell): Array[Cell] = {
    val column = new Array[Cell](size)
    for (i <- 0 until size)
      column(i) = matrixArray(i)(cell.columnNr)
    return column.filter(_.lineNr != cell.lineNr)
  }

  /**
    * for a given cell, return an array of the cells in the same box
    *
    * @param cell
    * @return an Array of Cells
    */
  def boxArray(cell: Cell): Array[Cell] = {
    val box: Array[Cell] = new Array(size)
    var index = 0
    for (line <- matrixArray)
      for (i <- 0 until size)
        if (line(i).boxNr == cell.boxNr){
          box(index) = line(i)
          index += 1
          }
    return box.filter(c => c.lineNr != cell.lineNr || c.columnNr != cell.columnNr)
  }

  /**
    * find all the cells, which share a line, a column or a box with a given cell.
    *
    * @param cell
    * @return Array of Cells
    */

  def done: Boolean = {
    matrixArray.forall(line => !line.map(box => box.value).contains(0))
  }

  def commonCells(cell: Cell): Array[Cell] =
    boxArray(cell) ++
      columnArray(cell) ++
      matrixArray(cell.lineNr)

  /**
    * Eliminate the numbers in common from the list of options in a given cell.
    * If there is only one remaining option,
    * then this option is the value of the cell.
    *
    * @param cell
    */
  def eliminateCommonNumbers(cell: Cell): Unit = {
    val common = commonCells(cell).map(b => b.value)

    cell.valueOptions = cell.valueOptions filterNot common.contains

    if (cell.valueOptions.length == 1)
      cell.value_(cell.valueOptions(0))
  }

  /**
    * run all the cells and filter their options
    */
  def filterAllCellsForCommons = {
    for (line <- matrixArray)
      for (cell <- line if cell.value == 0) {
        eliminateCommonNumbers(cell)
      }
    for (line <- matrixArray)
      for (cell <- line if cell.value == 0)
        if (cell.valueOptions.length == 1)
          cell.value_(cell.valueOptions(0))
  }

  /**
    * If one of the options of a given cell
    * is unique to that cell in the given line, column or box,
    * then the value of that cell must be that option.
    */
  def keepTheOnlyOptions = {
    def onlyOptionInList(number: Int, cell: Cell): Boolean = {
      lineArray(cell).forall(b => !b.valueOptions.contains(number)) ||
        columnArray(cell).forall(b => !b.valueOptions.contains(number)) ||
        boxArray(cell).forall(b => !b.valueOptions.contains(number))
    }

    for (line <- matrixArray)
      for (cell <- line if cell.value == 0) {
        for (i <- cell.valueOptions) {
          if (onlyOptionInList(i, cell)) cell.value_(i)
        }
      }
  }

  /**
    * Finds if there has been no changes
    *
    * @param that
    * @return
    */

  def ==(that: Matrix): Boolean = {
    for (line <- 0 until size) {
      for (column <- 0 until size) {
        if (this.matrixArray(line)(column).value != that.matrixArray(line)(column).value ||
          !this.matrixArray(line)(column).sameOptions(that.matrixArray(line)(column))) {
          return false
        }
      }
    }
    return true
  }

  def makeACopy: Matrix = {
    val copy = new Matrix(size)
    for (line <- 0 until size)
      for (column <- 0 until size) {
        copy.matrixArray(line)(column).value = this.matrixArray(line)(column).value
        copy.matrixArray(line)(column).valueOptions = copy.matrixArray(line)(column).valueOptions.
          filter(this.matrixArray(line)(column).valueOptions.contains(_))
      }
    return copy
  }

  def violation: Boolean = {
    for (line <- matrixArray) {
      for (cell <- line) {
        if (cell.valueOptions.length == 0) {
          return true
        }
        val common = commonCells(cell).map(b => b.value).filter(_ != 0)
        if (common.contains(cell.value)) {
          return true
        }
      }
    }
    return false
  }

  /**
    * The preemptive set algorithm for solving sudoku:
    * If n cells share the same list of n options
    * then other cells in the common area con't have a value from these options.
    *
    * @param cell
    * @param domain   can be a line, a column or a box
    */
  def preemptiveSet(cell: Cell, domain: Array[Cell]) = {
    if (cell.valueOptions.length >= 2 && cell.valueOptions.length <= 6) {

      // Find other cells in this domain that have the same options
      val thisCell_Set = domain.filter(c => c.valueOptions.forall(o => cell.valueOptions.contains(o)))

      var theRest: Array[Cell] = new Array(0)
      if (thisCell_Set.length == cell.valueOptions.length) {
        theRest = domain.filter(c => !thisCell_Set.contains(c))
      }
      theRest.foreach(c => c.valueOptions = c.valueOptions.filter(!cell.valueOptions.contains(_)))
      for (box <- domain)
        if (box.valueOptions.length == 1 && box.value == 0)
          box.value_(box.valueOptions(0))
    }
  }

  /**
    * Go through the cells and find any preemtive sets (see above)
    * Update the options and values accordingly
    */

  def eliminatePreemptiveSet = {
    for (line <- matrixArray)
      for (cell <- line) {
        preemptiveSet(cell, matrixArray(cell.lineNr))
        preemptiveSet(cell, Array(cell) ++ columnArray(cell))
        preemptiveSet(cell, Array(cell) ++ boxArray(cell))
      }
    for (line <- matrixArray)
      for (box <- line)
        if (box.valueOptions.length == 1 && box.value == 0)
          box.value_(box.valueOptions(0))
  }


  //The final answer
  var answer: Option[Matrix] = None

  /**
    * Try to solve the sudoku without any guess work first
    * If stuck then guess one number and try again
    * after guessing
    * Violation: The guessing was wrong and it lead to unvalid case
    *            Return one step and guess another option
    * Stuck: guess one more number and repeat
    */

  def solve: Unit = {
    var stop = false
    var stuck = false
    while (!this.done && !stuck) {
      val copy: Matrix = this.makeACopy     // make a copy of the matrix before trying to solve
      this.filterAllCellsForCommons
      this.keepTheOnlyOptions
      this.eliminatePreemptiveSet
      if (this == copy) stuck = true       // There has been no changes
    }
    if (stuck && !this.done) {             // This is true iff stuck -> start guessing
      for (line <- 0 until size if !stop) {
        for (cell <- 0 until size if this.matrixArray(line)(cell).value == 0 && !stop) {
          for (option <- this.matrixArray(line)(cell).valueOptions if !stop) {
            val copy = this.makeACopy
            copy.matrixArray(line)(cell).value_(option)     // when guessing we work only on the copy
            var copyStuck = false
            while (!copy.done && !copyStuck & !copy.violation) {
              val copyOfCopy = copy.makeACopy
              copy.filterAllCellsForCommons
              copy.keepTheOnlyOptions
              copy.eliminatePreemptiveSet
              if (copy == copyOfCopy) copyStuck = true
            }
            if (copy.done) {
              answer = Some(copy)
              stop = true
            }
          }
        }
      }
    }
  }


}
