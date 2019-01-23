# Sudoku

# Solving sudoku using human-like approach.
  - This code uses the programming language Scala and the gui-library swing.
  - It solves any sudoku problem using three algorithms before it starts guessing:
    1- Eliminating obvious options from each cell's list of options meaning
    if a number already exists in the same row, column or box (called later a domain) then it is not a valid option
    2- If a cell has a unique option in some domain, then it must be the value of the cell.
    3- Preemptive sets: if n number of cells share a unique set of n options in some domain,
    then the other cells in that domain cannot have any of these n values as options.
  - If the privious steps get stuck, the program start guessing one number at the time.
  
# About this project:
- This was the first project I write on my own outside of university. 
- I kept everything the way it was writen, just updated the documentation to make more readable.
- The gui is very basic and using scala.swing. Which is now a seldemly used library, which is why I didn't update the gui files.
