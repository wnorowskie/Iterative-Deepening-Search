;; =====================================
;;  CMPU-365, Spring 2023
;;  Extended IDS for different "value" functions
;;  Eric Wnorowski
;; =====================================


;;  XEXPAND -- Similar to EXPAND from Asmt. 1
;; -----------------------------------------------------------------
;;   Takes a VALUE-FUNC as an input.  This function is used to compute
;;      the value of a child-node immediately after it is created.
;;      The VALUE-FUNC takes one input: an XNODE.  For IDS-UCS the
;;      value function will be #'XNODE-COST (i.e., cost so far).  For
;;      IDS-A* it will be F(n) = G(n) + H(n).  (More on this later.)
;;      For plain old IDS it will be #'XNODE-DEPTH.

(defun xexpand (xnode rezzy &key (value-func #'xnode-depth))
  (labels (;; XEXPAND-ACC:  Recursive helper function that walks through the legal MOVES
	   ;;               to create corresponding XNODES.  CHILDREN is the accumulator
	   ;;               of child XNODES.
	   (xexpand-acc (moves children)
	     (cond
	      ;; Case 1:  No more moves/actions to do
	      ((null moves)
	       ;; Increment the num-nodes field of REZZY (num
	       (incf (results-num-nodes-generated rezzy) (length children))
	       ;; Return the list of child nodes
	       children)
	      ;; Case 2:  More moves/actions to do
	      (t
		   ;; let move be first action struct of moves
		   ;;     xs be the xnode-state
		   ;;	  new-state be the child state based on move
		   ;;     cost be the cost of recent move
	       (let* ((move (car moves))
		 		  (xs (do-move (xnode-state xnode) move))
				  (new-state (first xs))
				  (cost (second xs)))
			;; recursive call on the rest of the moves
				(xexpand-acc (cdr moves)
				;; add newest node to the accumulator
					(cons (make-xnode :state new-state
									  :parent xnode
									  :action move
									  :depth (+ 1 (xnode-depth xnode))
									  :cost (+ cost (xnode-cost xnode))
									  ;; get value based on value based on value-func on this node
									  :value (funcall value-func (make-xnode  :state new-state
																			  :parent xnode
																			  :action move
																			  :depth (+ 1 (xnode-depth xnode))
																			  :cost (+ cost (xnode-cost xnode)))))
                                      ;; acc
					children)))))))
		;; labels body
		(xexpand-acc (fetch-legal-moves (xnode-state xnode)) nil)))

(defun xdls (xprob limit master-rezzy &key (value-func #'xnode-depth))
	(let ((root (make-root-xnode xprob)))
	;; Start of labels recursive
		(labels ((xdls-rec (xnode)
	;; let value be the xnode-value with regard to the value-func
    (let ((value (funcall value-func xnode)))
			(cond 
			;; if value is greater than limit return value
				((> value limit) value)
			;; if xnode-state is goal then return xnode
				((state-is-goal? (xnode-state xnode)) xnode)
			;; else
				(t (let* ((children (xexpand xnode master-rezzy))
                  (sorted-children (sort children #'< :key #'xnode-value))
                  (min-value-unexplored-children nil))
				  ;; for number of child nodes in expansion
						(dolist (child sorted-children)
						;; must also do XDLS on child
							(let ((child-answer (xdls-rec child)))
								(cond
								;; xdls-rec on child is not nil
								((numberp child-answer)
								;; if min-value-unexplored-children is null
								(if (null min-value-unexplored-children)
									;; then set it to the result of xdls on child
									(setf min-value-unexplored-children child-answer)
									;; otherwise set to the min
									(setf min-value-unexplored-children (min min-value-unexplored-children))))
								;; xdls on child is null
								((null child-answer)
								;; set min-value to min-value
								(setf min-value-unexplored-children min-value-unexplored-children))
								;; else stop and return child-answer
								(t  
								(return-from xdls-rec child-answer)))))
					;; after dolist()
						(return-from xdls-rec min-value-unexplored-children)))))))
		;; Body of the labels
			(xdls-rec root))))

;;  XIDS -- Extended Iterative Deepening Search (based on a VALUE-FUNC)
;; -------------------------------------------------------------------------
;;  INPUTS:  XPROB, an XPROBLEM struct
;;           VALUE-FUNC, used to compute the value of each generated node
;;  OUTPUT:  A goal node or NIL.

(defun xids (xprob &key (value-func #'xnode-depth))
  (let* (;; Initialize a RESULTS struct
	 (master-rezzy (make-results :start-time (get-universal-time)))
	 ;; First iteration calls XLDS with a LIMIT of 0 (i.e., will only explore root node)
	 (xdls-answer (xdls xprob 0 master-rezzy :value-func value-func)))
    ;; As long as XDLS hit the cutoff without finding a goal....
    (while (and xdls-answer
		(not (xnode-p xdls-answer)))
      ;; It means that xdls-answer is a number:  MIN-VAL-UNEXPLORED-CHILD!
      ;; Use that number as the cutoff for the next iteration:
      (setf xdls-answer (xdls xprob xdls-answer master-rezzy :value-func value-func)))
    ;; After the WHILE:
    ;; Finish collecting info for the RESULTS struct
    (setf (results-end-time master-rezzy) (get-universal-time))
    (setf (results-elapsed-time master-rezzy) (- (results-end-time master-rezzy)
						 (results-start-time master-rezzy)))
    (setf (results-goal-node master-rezzy) xdls-answer)
    ;; Return the RESULTS struct
    master-rezzy))

;;  XIDS-ASTAR
;; ----------------------------------------------------
;;  INPUTS:  XPROB, an XPROBLEM instance
;;           HEURISTIC, a (domain dependent) heuristic function to apply to nodes
;;  OUTPUT:  A goal node or NIL.

(defun xids-astar (xprob &key (heuristic #'manhattan-h))
  ;; Call XIDS on XPROB with a VALUE-FUNC that computes F(n) = G(n) + H(n)
  (xids xprob :value-func #'(lambda (xnode)
				;; cost + heuristic
			      (+ (xnode-cost xnode) (funcall heuristic xnode))
			      )))