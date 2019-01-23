package gui

import logic.Matrix

import scala.swing.{Action, BoxPanel, Button, FlowPanel, Label, MainFrame, Orientation, Point, Separator, TextField}

object Window {


  def main(args: Array[String]): Unit = {
    mainWindow.visible = true

  }


  var sudoku = new Matrix(9)
  val mainWindow = new MainFrame {
    title = "solve sudoku"
    location = new Point(550, 150)
  }

  var flowList: List[FlowPanel] = Nil

  for (i <- 0 until 9 reverse) {

    var txFdList: List[TextField] = Nil
    var line = new FlowPanel

    for (j <- 0 until 9 reverse) {

      var txFd = new TextField
      txFd.columns = 2
      txFdList ::= txFd

    }
    line.contents ++= txFdList
    flowList ::= line
  }

  val resultsLabel = new Label
  resultsLabel.text = "                 "

  val solveAction = new Action("solve") {
    def apply() {
      for (f <- 0 until flowList.length) {
        for (c <- 0 until flowList(f).contents.length) {
          flowList(f).contents(c) match {
            case tx: TextField => {
              var y = 0
              if (tx.text == "") y = 0
              else {
                try {
                  y = tx.text.toInt
                } catch {
                  case notNumber: NumberFormatException => {
                    resultsLabel.text = "Please enter only numbers between 1 and 9"
                    return
                  }
                  case ex: Exception => 10
                }
              }
              if (y > 9 || y < 0) {
                resultsLabel.text = "Please enter only numbers between 1 and 9"
                return
              }
              else if (y != 0) sudoku.matrixArray(f)(c).value_(y)
            }
          }
        }
      }
      sudoku.solve
      val answer = sudoku.answer match {
        case None => sudoku
        case Some(ans) => ans
      }
      if (!answer.done) resultsLabel.text = "Not enough numbers to solve the Sudoku"
      for (f <- 0 until flowList.length) {
        for (c <- 0 until flowList(f).contents.length) {
          flowList(f).contents(c) match {
            case tx: TextField => {
              tx.text = answer.matrixArray(f)(c).value.toString
            }
          }
        }
      }
    }
  }


  val clearAction = new Action("clear") {
    def apply() {
      for (f <- 0 until flowList.length) {
        for (c <- 0 until flowList(f).contents.length) {
          flowList(f).contents(c) match {
            case tx: TextField => {
              tx.text = ""
            }
          }
        }
      }
    }
  }

  val resultFlow = new FlowPanel
  resultFlow.contents ++= Seq(resultsLabel)
  val buttonFlow = new FlowPanel
  val solveButton = new Button(solveAction)
  val clearButton = new Button(clearAction)
  buttonFlow.contents += clearButton
  buttonFlow.contents += solveButton


  val panelBox = new BoxPanel(Orientation.Vertical)
  panelBox.contents ++= flowList
  panelBox.contents += resultFlow
  panelBox.contents += buttonFlow

  mainWindow.contents = panelBox

}
