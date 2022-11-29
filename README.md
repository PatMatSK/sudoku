# Sudoku Solver

To find solution of your sudoku just start sudoku.rkt and type into REPL:

(solve-sudoku matrix)

where instead 'matrix' place your sudoku matrix.
Matrix should be list of 9 lists of 9 elements( 0 <= elem <= 9 ).
0 represents empty place. If solution does not exist or given matrix is incorrect, returns #f.
In tester.rkt file you can find correct usage.
You find there also 50 sudoku examples (grid01,...,grid50) with correct 
results to check. 
