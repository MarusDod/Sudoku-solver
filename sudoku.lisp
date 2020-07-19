;all null cells, will be replaced this list
(defconstant exp '(1 2 3 4 5 6 7 8 9))


;creates 2d array from a 9x9 linked-list
(defun create-board (lst)
    (make-array '(9 9) :initial-contents (replace-zeros lst))
)

;replaces all zeros with exp global variable
(defun replace-zeros (lst)
    (if (null lst) nil
        (cons (mapcar #'(lambda (x) (if (zerop x) exp (list x))) (car lst)) (replace-zeros (cdr lst)))
    )
)

;linked-list example
(setf example '((0 0 1 0 0 8 0 6 0) 
                (8 4 0 0 9 0 0 5 0) 
                (0 0 9 7 0 0 2 0 8) 
                (1 0 0 4 0 5 8 0 0) 
                (0 6 0 0 7 0 0 9 0) 
                (0 0 3 1 0 9 0 0 7) 
                (4 0 5 0 0 7 6 0 0) 
                (0 7 0 0 1 0 0 8 2) 
                (0 1 0 6 0 0 5 0 0)))

;create 2d array from example
(setf board (create-board example))

;calls the main solving function, and converts to 2d array of lists to 2d array of numbers
(defun resolve (m)
    (flatten-board (res m))
)

;recursively solves the game, until solution is found
(defun res (m)
    (if (solved m) m (res (red m)))
)

;solves table once, for every element that contains a single number, all removes all other instances of that number in the same row/column/square
(defun red (m) 
        (dotimes (x (first (array-dimensions m)))
            (dotimes (y (first (array-dimensions m)))
                (if (= (length (aref m x y)) 1) 
                    (list (remove-line m x y) (remove-square m x y))
                )
            )
        )
        m
)

;checks if it's done, if all elements are single numbers then it cant be reduced any further
(defun solved (m)
    (let ((a (- (first (array-dimensions m)) 1)) (b T))
        (dotimes (x a)
                    (loop for y from 0 to a do
                        (if (> (length (aref m x y)) 1)
                            (setf b nil)
                        ) 
                    )
        )
        b
    )
)

(defun flatten-board (m)
    (dotimes (x (first (array-dimensions m)))
        (dotimes (y (second (array-dimensions m)))
            (setf (aref m x y) (car (aref m x y)))
        )
    )
    m
)

;copies 2d array by value to prevent side effects
(defun copy-array (m)
            (let ((a (make-array (array-dimensions m))))
                (loop for x from 0 to (- (first (array-dimensions m)) 1) do
                    (loop for y from 0 to (- (second (array-dimensions m)) 1) do
                        (setf (aref a x y) (aref m x y))
                    )
                )
            a
            )
)

;iterates 3x3 square and removes all instances of the number at m(i,j) inside that square
(defun remove-square (m i j)
    (let ((q (near-three i)) (w (near-three j)) (val (car (aref m i j))))
        (loop for x from q to (+ q 2) do
            (loop for y from w to (+ w 2) do
               (del m x y val) 
            )
        )
    )
)

;nearest lowest number divisible by 3
(defun near-three (x) 
    (- x (mod x 3))
)

;remove all instances of number at m(i,j) in the same column/row
(defun remove-line (m i j)
    (let ((val (car (aref m i j))))
    (loop for x from 0 to (- (first (array-dimensions m)) 1) do
        (del m x j val)
    )
    (loop for y from 0 to (- (second (array-dimensions m)) 1) do
        (del m i y val)
    )
    )
)

;deletes a number from the list
(defun del (m i j val)
    (if (> (length (aref m i j)) 1)
        (setf (aref m i j) (remove-if-not #'(lambda (x) (not (= x val))) (aref m i j)))
    )    
)



















