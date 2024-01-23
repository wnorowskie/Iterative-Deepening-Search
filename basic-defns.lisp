;; ========================================
;;   CMPU-365, Spring 2023
;;   Asmt. 2
;; ========================================
;;   FILE:  basic-defns.lisp 
;; ========================================
;;   Data structures and related low-level functions in preparation
;;   for implementing general-purpose iterative-deepening algorithm.
;; ---------------------------------------------------------------------
;;   This file is similar to basic-defns.lisp from asmt 1, except that
;;   the data structures are somewhat different, and the approach
;;   to implementing domains (e.g., eights-puzzle, cities) is different.


;;  XPROBLEM -- only includes one field now!
;; ----------------------------------------------

(defstruct xproblem
  init-state		
  )

;;  XNODE -- same as NODE, but with COST and VALUE fields
;; -------------------------------------------------------------

(defstruct (xnode (:print-function print-xnode))
  state
  parent
  action
  (depth 0)
  (cost 0)   ;; cost so far from root node
  (value 0)  ;; value of this node (e.g., f(n) for A* search)
  )

(defun print-xnode (xnode str depth)
  ;; To avoid compiler warnings about an input we never use.
  (declare (ignore depth))
  (let ((st (xnode-state xnode)))
    ;; Display the most recent action (i.e., the one that got us here)
    (format str "XNODE:  (action = ~A) (depth = ~A) (value = ~A) ~%" (xnode-action xnode)
	    (xnode-depth xnode) (xnode-value xnode))
    ;; Display the current state
    (format str "         STATE: ~A~%" st)))

;;  MAKE-ROOT-XNODE -- similar to MAKE-ROOT-NODE, but for XPROBLEM/XNODE structs
;; ----------------------------------------------------------------------------------

(defun make-root-xnode (xprob)
  (make-xnode :state (xproblem-init-state xprob)))

;;  XCYCLE?  --  Same as CYCLE?, but for XNODES
;; ------------------------------------------------

(defun xcycle? (state xnode)
  ;; NOTE:  Each domain must implement a STATES-ARE-EQUAL? *METHOD*
  ;;        That's why XPROBLEM doesn't include a STATE-EQ-FUNC field.
  (and xnode
       (or (states-are-equal? state (xnode-state xnode))
	   (xcycle? state (xnode-parent xnode)))))

;; ------------------------------------------------------------
;;  RESULTS struct -- Same as for Asmt. 1
;; ------------------------------------------------------------
;;  The start time, end time, and elapsed time are all measured by
;;  "internal-run-time" which is typically measured in milliseconds.
;;  See the global constant INTERNAL-TIME-UNITS-PER-SECOND, which
;;  equals 1000 on our system.

(defstruct (results (:print-function print-results))
  (goal-node nil)          ;; a goal node, if found; otherwise, NIL or *CUTOFF*
  (num-nodes-generated 0)  ;; the number of nodes generated during the search
  (num-nodes-explored 0)   ;; the number of nodes explored during the search
  (start-time NIL)	   ;; when the search started (msecs)
  (end-time NIL)           ;; when the search stopped (msecs)
  (elapsed-time NIL)       ;; how long the search took (msecs)
  )

(defun print-results (rezzy str depth &key (show-path? nil))
  (declare (ignore depth))
  (format str "==========~% RESULTS~%==========~%")
  (format str "Generated ~A nodes, explored ~A nodes, in ~A seconds~%" 
	  (results-num-nodes-generated rezzy)
	  (results-num-nodes-explored rezzy)
	  (results-elapsed-time rezzy))
  (cond 
   ;; Case 1:  Found a goal!
   ((xnode-p (results-goal-node rezzy))
    (format str "Found a goal!~%~%")
    (when show-path?
      (format str "~A~%" (xbuild-path (results-goal-node rezzy)))))
   ;; Case 2:  No goal!
   (t
    (format str "No goal!  :(~%"))))

;;  XBUILD-PATH -- Similar to BUILD-PATH from Asmt. 1
;; -----------------------------------------------------------

(defun xbuild-path (goal-node)
  ;; BUILD-PATH-ACC is a recursive helper function
  ;; Note the use of LABELS (similar to LETREC in Scheme)
  (labels ((build-path-acc (xnode acc)
	     ;; Base Case:  NODE is NIL 
	     ;;   This happens when previous function call involved
	     ;;   the root node.  The root node's parent is NIL.
	     ;;   In this case, we are done!
	     (if (not xnode)
		 ;; So return the accumulator
		 acc
	       ;; Recursive Case:  
	       ;;   Accumulate the current node as we move
	       ;;   to the PARENT...
	       (build-path-acc (xnode-parent xnode) (cons xnode acc)))))
    
    ;; Body of LABELS:  call the recursive helper with ACC=NIL
    (build-path-acc goal-node nil)))


