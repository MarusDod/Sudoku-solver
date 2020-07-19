#Sudoku-solver

To run this program you need to install clisp:

	>sudo apt-get install clisp

To run the repl:

	>clisp

Once you are in the repl, load the main file:

	>(load 'sudoku.lisp)

To create a board:

	>(setf random-variable-name (create-board '(random-9x9-list)))

To solve the board

	>(resolve random-variable-name)

There's already a built-in board too:

	>(resolve (create-board example))
