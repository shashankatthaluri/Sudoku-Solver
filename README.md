# 🧮 Sudoku Solver Project 🧮
Sudoku is a logic-based placement puzzle where the goal is to fill a 9×9 grid with digits so that each column, row, and 3×3 section contain the digits between 1 to 9. This project implements a sudoku solver using backtracking algorithm in Fortran.

## 🕹 How to play Sudoku 🕹
- The grid is divided into 9 boxes, each containing 9 slots.
- Some slots within the grid already contain digits. These digits cannot be changed.
- The remaining slots should be filled with digits 1-9 so that no digit is repeated in any row, column or 3x3 box.

## 🧩 How it Works
This program uses elimination and backtracking algorithms to solve sudoku puzzles of varying difficulties. Input puzzles are read from a text file, then the program analyzes the puzzle and systematically fills in values until it is completed!

## Backtracking Algorithm 🧮
Backtracking is an algorithmic technique for solving problems recursively by trying to build a solution incrementally, abandoning a solution ("backtracking") as soon as it is determined that the solution cannot possibly be completed to a valid one.

The key steps are:

- Find an empty slot
- Try inserting 1-9 in the slot one by one
- Check if it leads to a valid partial solution
- If valid, try to fill the next empty slot
- If at any point it leads to invalid solution, backtrack and try next number

## Fortran Code 💻
The Fortran code implements the backtracking algorithm to solve a sudoku puzzle. It takes the partially filled sudoku board as input and outputs the solved board.

Some key aspects:
-Board is stored as a 2D array of integers
-Functions to check row, column and box validity
-Backtracking function recursively tries inserting numbers
-Prints the solved board at the end

Its a basic sudoku solver using backtracking method coded in Fortran 95 language. 
The idea was extended to make the sudoku into tors form and I already found a pattern where for a every number there is some situation or position the number must be repeated.

Let me know if you need any clarification or have additional questions! 😊
