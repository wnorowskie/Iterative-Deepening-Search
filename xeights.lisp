;; =====================================
;;  CMPU-365, Spring 2023
;;  Implementation of 8s-tile-puzzle domain
;;  Asmt. 2
;; ===================================== 
;;  Each domain must define a type of MOVE struct, a STATE struct, a
;;  FETCH-LEGAL-MOVES method that takes a STATE as input, and a
;;  DO-MOVE method that takes a STATE struct and a MOVE struct as inputs,
;;  and generates a list (new-state action-cost) as output.
;;  Must also define STATES-EQUAL? and STATE-IS-GOAL? methods.
;; =====================================

;;  Some useful constants

(defvar +blank-symbol+ '_)
(defvar +blank-num+ 0)
(defvar goal-rows #(0 0 0 0 1 2 2 2 1))
(defvar goal-cols #(0 0 1 2 2 2 1 0 0))

;;  EIGHTS-MOVE struct
;; --------------------------------------------------
;;  Contains sufficient info for DO-MOVE to do the move!

(defstruct (eights-move (:print-function show))
  drow ;; The change in the blank's row
  dcol ;; The change in the blank's col (only one can be non-zero)
  name ;; A symbol, useful for printing
  )

;;  SHOW method for an EIGHTS-MOVE struct
;; --------------------------------------------------

(defmethod show ((mv eights-move) str depth)
  (declare (ignore depth))
  (format str "Eights MOVE: ~A~%" (eights-move-name mv)))

;;  EIGHTS struct  -- Same as for Asmt. 1
;; ---------------------------------------------------

(defstruct (eights (:print-function show))
  (locations (make-array '(3 3) 
			 :initial-contents 
			 '((1 2 3) (8 0 4) (7 6 5))))
  ;; The position of the blank is given by:
  (blank-row 1)
  (blank-col 1))

;;  SHOW method for an EIGHTS struct
;; ----------------------------------------------------
;;  INPUTS:  GAME, an eights tile puzzle
;;           STR, an output stream (probably just T)
;;           DEPTH, ignored
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays the given game in the interactions window.

(defmethod show ((game eights) str depth)
  (declare (ignore depth))
  (let ((locs (eights-locations game)))
    (format str "~%")
    ;; Walk through the rows and columns of the puzzle
    (dotimes (row 3)
      (dotimes (col 3)
	;; display the tile at the current location
	(let ((tile (aref locs row col)))
	  (format str "~A " (if (= tile +blank-num+) +blank-symbol+ tile))))
      (format str "~%"))))

;;  EIGHTS-GOAL-ARRAY -- same as in Asmt. 1

(defvar eights-goal-array (make-array '(3 3) 
				      :initial-contents
				      '((1 2 3) (8 0 4) (7 6 5))))
;;  EIGHTS-GOAL -- same as in Asmt. 1

(defvar eights-goal (make-eights :locations eights-goal-array
				 :blank-row 1
				 :blank-col 1))


;;  Each domain must define two methods:  STATES-ARE-EQUAL? and STATE-IS-GOAL?
;;  That's why the XPROBLEM struct no longer contains state-eq-func and goal-test-func fields.

;;  STATES-ARE-EQUAL? 
;; ---------------------------------
;;  INPUTS:  Two instances of EIGHTS tile puzzle structs
;;  OUTPUT:  T if they represent the same state.

(defmethod states-are-equal? ((st1 eights) (st2 eights))
  (equalp (eights-locations st1) (eights-locations st2)))

;;  STATE-IS-GOAL?
;; --------------------------------------
;;  INPUT:  An EIGHTS tile-puzzle instance
;;  OUTPUT:  T if that input represents a goal state.

(defmethod state-is-goal? ((st eights))
  (states-are-equal? st eights-goal))


;; =====================================================================
;;  NOTE:  The FETCH-LEGAL-MOVES and DO-MOVE methods are defined in the
;;         body of this LET*.  That way, these methods can make use of
;;         the local variables created by this LET*.
;; =====================================================================

(let* (;; Four EIGHTS-MOVE instances
       (mv-north (make-eights-move :drow -1 :dcol 0 :name 'north))
       (mv-south (make-eights-move :drow 1 :dcol 0 :name 'south))
       (mv-east (make-eights-move :drow 0 :dcol 1 :name 'east))
       (mv-west (make-eights-move :drow 0 :dcol -1 :name 'west))
       ;; A hash table that will provide the legal moves for each position
       (mv-hash (make-hash-table :test #'equal))
       ;; A vector of the 4 EIGHTS-MOVE structs
       (mv-vector (make-array 4 :initial-contents (list mv-north mv-south mv-east mv-west))))
  
  ;; We'll use the following MAPCAR to initialize the MV-HASH hash-table
  (mapcar #'(lambda (pair)
	      ;; PAIR has the form (BPOSN MV-INDICES) where
	      ;;   BPOSN is a pair (e.g., (0 1)) specifying a position on the puzzle
	      ;;   MV-INDICES is a list of indice
	      ;;     instances in MV-VECTOR are legal for the position BPOSN.
	      (let ((bposn (first pair))
		    (mv-indices (second pair)))
		;; Set the hash-table entry for BPOSN to the list of EIGHTS-MOVE instances
		;; that are legal moves if the blank is at BPOSN.
		(setf (gethash bposn mv-hash)
		  (mapcar #'(lambda (mv-index) (aref mv-vector mv-index))
			  mv-indices))))
	  '(((0 0) (1 2))
	    ((0 1) (1 2 3))
	    ((0 2) (1 3))   ;; e.g., If blank is at (0 2), then SOUTH and WEST are the legal moves
	    ((1 0) (0 1 2))
	    ((1 1) (0 1 2 3)) ;; e.g., If blank is at (1 1), then all four moves are legal
	    ((1 2) (0 1 3))
	    ((2 0) (0 2))
	    ((2 1) (0 2 3))
	    ((2 2) (0 3))))


  ;;  FETCH-LEGAL-MOVES:  Just look up the legal moves corresponding to the blank's current position.
  ;; --------------------------------------------------------------------------------------------------
  
  (defmethod fetch-legal-moves ((state eights))
    (let* ((brow (eights-blank-row state))
	   (bcol (eights-blank-col state))
	   (bposn (list brow bcol)))
      (gethash bposn mv-hash)))
  
  ;;  DO-MOVE
  ;; -------------------------------------------------------------
  ;;  Since we'll only apply this to LEGAL moves, we don't have to worry
  ;;  about returning NIL.
  
  (defmethod do-move ((state eights) (mv eights-move))
    (let* (;; BROW, BCOL = blank's current position
	   (brow (eights-blank-row state))
	   (bcol (eights-blank-col state))
	   (curr-locs (eights-locations state))
	   ;; NEW-LOCS:  a new array for holding the result of the move
	   (new-locs (make-array '(3 3)))
	   (drow (eights-move-drow mv))
	   (dcol (eights-move-dcol mv)))
      ;; Copy the array contents from CURR-LOCS to NEW-LOCS
      (dotimes (row 3)
	(dotimes (col 3)
	  (setf (aref new-locs row col) (aref curr-locs row col))))
      ;; Make two changes to NEW-LOCS due to the blank having swapped places
      ;; with another tile.
      (setf (aref new-locs brow bcol) 
	(aref curr-locs (+ brow drow) (+ bcol dcol)))
      (setf (aref new-locs (+ brow drow) (+ bcol dcol)) 
	+blank-num+)
      ;; Return a list of the form (NEW-STATE COST), where cost = 1
      (list (make-eights :locations new-locs
			 :blank-row (+ brow drow)
			 :blank-col (+ bcol dcol))
	    1))))

;;  MAKE-RANDOM-EIGHTS --  Same as in Asmt. 1
;; ------------------------------------------------------

(defun make-random-eights (n)
  (let (;; PUZZ:  a new puzzle starting out in goal state
	(puzz (make-eights)))
    ;; Then attempt to do N randomly selected moves (some may be illegal)
    (dotimes (i n)
      (let* ((moves (fetch-legal-moves puzz))
	     (rand-move (nth (random (length moves)) moves)))
	;; do randomly selected move (don't care about cost)
      (setf puzz (first (do-move puzz rand-move)))))
    puzz))




;;  FETCH-N-ELTS-ACC / FETCH-N-ELTS  -- helper functions that enable
;;  FANCY-SHOW-EIGHTS-SOLN to display long sequences of EIGHTS states, 10 to a row.

;;  FETCH-N-ELTS
;; --------------------------------------
;;  INPUTS:  N, a number
;;           LISTY, a list
;;  OUTPUT:  A pair of the form (FIRST-N RESTY)
;;           where FIRST-N is a list of the first N elements of LISTY 
;;           and RESTY is all the rest of the elements of LISTY.

;;  FETCH-N-ELTS-ACC:  Accumulator-based helper function for FETCH-N-ELTS

(defun fetch-n-elts-acc (n listy acc)
  (if (or (<= n 0) (null listy))
      (list (reverse acc) listy)
    (fetch-n-elts-acc (1- n) (rest listy) (cons (first listy) acc))))

(defun fetch-n-elts (n listy)
  (fetch-n-elts-acc n listy nil))

;;  FANCY-SHOW-EIGHTS-SOLN
;; ---------------------------------------------

(defun fancy-show-eights-soln (goal-node)
  (let* ((nodes (xbuild-path goal-node))
	 (states (mapcar #'xnode-state nodes))
	 (grids (mapcar #'eights-locations states))
	 (first10-and-resty (fetch-n-elts 10 grids))
	 (first-ten-grids (first first10-and-resty)))
    (setf grids (second first10-and-resty))
    (while first-ten-grids
      (dotimes (row 3)
	(dolist (gr first-ten-grids)
	  (dotimes (col 3)
	    (format t "~s " (if (zerop (aref gr row col)) '_ (aref gr row col))))
	  (format t "  "))
	(format t "~%"))
      (format t "~%")
      (setf first10-and-resty (fetch-n-elts 10 grids))
      (setf first-ten-grids (first first10-and-resty))
      (setf grids (second first10-and-resty)))))

;;  MANHATTAN-DISTANCE
;; ----------------------------------------------
;;  INPUT:  An EIGHTS struct
;;  OUTPUT:  The total of all the manhattan distances of each tile
;;           to its goal position.

(defmethod manhattan-distance ((state eights))
  (let* ((locs (eights-locations state))
	 (counter 0))
    ;; For each location (row, col)...
    (dotimes (row 3)
      (dotimes (col 3)
	(let* (;; Fetch the tile at that location
	       (tile (aref locs row col))
	       ;; And fetch the row and column of that tile in
	       ;; the goal array
	       (g-row (svref goal-rows tile))
	       (g-col (svref goal-cols tile)))
	  ;; Only deal with tiles, not the blank!
	  (when (not (= tile +blank-num+))
	    (incf counter (+ (abs (- g-row row))
			     (abs (- g-col col))))))))
    counter))

;;  MANHATTAN-H
;; -------------------------------------
;;  INPUT:  XNODE
;;  OUTPUT:  The result of calling manhattan-distance on that xnode's state
;;            (which had better be an EIGHTS struct!)

(defun manhattan-h (xnode)
  (manhattan-distance (xnode-state xnode)))
