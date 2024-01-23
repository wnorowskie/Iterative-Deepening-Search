;; ==================================
;;  CMPU-365, Spring 2023
;;  testing.lisp
;;  Eric Wnorowski
;; ==================================
;;  Load this file to compile and load all files for this assignment
;;  and run all tests.

(load "asmt-helper.lisp")

(header "2023 Asmt. 2" "Extended IDS")

;;  Compile-and-load all files

(maker *list-o-files*)

;; ------------------------
;;  TEST EIGHTS
;; ------------------------
;;  Generate random eights problem by doing n randomly chosen moves
;;  from goal position.  Ensures that optimal solution will have at
;;  most n steps.
(format t "Testing the XEIGHTS!~%~%~%")

(defun make-random-eights-xprob (n)
  (make-xproblem :init-state (make-random-eights n)))

;;  *PR* -- a random eights problem with a possibly long optimal solution
;;  *MR* -- the MASTER-RESULTS struct from doing IDA* search on *PR*

(setf *pr* (make-random-eights-xprob 200))
(setf *mr* (xids-astar *pr*))  ;; using MANHATTAN DISTANCE heuristic by default
(print-results *mr* t 0)

;; FANCY-SHOW-EIGHTS-SOLN:  concisely shows solution path with 10 puzzles per row
(fancy-show-eights-soln (results-goal-node *mr*))

;; TEST TWO:
(setf *ppr* (make-random-eights-xprob 50))
(setf *mmr* (xids-astar *ppr*))  ;; using MANHATTAN DISTANCE heuristic by default
(print-results *mmr* t 0)

;; FANCY-SHOW-EIGHTS-SOLN:  concisely shows solution path with 10 puzzles per row
(fancy-show-eights-soln (results-goal-node *mmr*))

;; TEST THREE: 
(setf *pprr* (make-random-eights-xprob 50))
(setf *mmrr* (xids-astar *ppr*))  ;; using MANHATTAN DISTANCE heuristic by default
(print-results *mmrr* t 0)

;; FANCY-SHOW-EIGHTS-SOLN:  concisely shows solution path with 10 puzzles per row
(fancy-show-eights-soln (results-goal-node *mmrr*))

;; -----------------------
;;  TEST CITIES
;; -----------------------
;; arad test
(format t "Testing the CITIES domain!~%~%~%")
(setf *cpr* (make-xproblem :init-state (make-cities :curr-loc 'arad)))
(setf *cmr* (xids-astar *cpr* :heuristic #'cities-heuristic))
(print-results *cmr* t 0)

;; hirsova test
(setf *ccpr* (make-xproblem :init-state (make-cities :curr-loc 'hirsova)))
(setf *ccmr* (xids-astar *ccpr* :heuristic #'cities-heuristic))
(print-results *ccmr* t 0)

;; oradea
(setf *ccppr* (make-xproblem :init-state (make-cities :curr-loc 'oradea)))
(setf *ccmmr* (xids-astar *ccppr* :heuristic #'cities-heuristic))
(print-results *ccmmr* t 0)

;; ------------------------
;; TEST XFIFTEENS
;; ------------------------
;;  Generate random fifteens problem by doing n randomly chosen moves
;;  from goal position.  Ensures that optimal solution will have at
;;  most n steps.
(format t "Testing the XFIFTEENS!~%~%~%")

(defun make-random-fifteens-xprob (n)
  (make-xproblem :init-state (make-random-fifteens n)))

;; *FR* -- a random fifteens problem with a possibly long optimal solution
;; *RS* -- the MASTER-RESULTS struct from doing IDA* search on *FR*

(setf *fr* (make-random-fifteens-xprob 20))
(setf *rs* (xids-astar *fr* :heuristic #'f-manhattan-h));; using F-MANHATTAN-DISTANCE heuristic
(print-results *rs* t 0)

;; FANCY-SHOW-EIGHTS-SOLN:  concisely shows solution path with 10 puzzles per row
(fancy-show-fifteens-soln (results-goal-node *rs*))

(setf *ffr* (make-random-fifteens-xprob 50))
(setf *rrs* (xids-astar *ffr* :heuristic #'f-manhattan-h));; using F-MANHATTAN-DISTANCE heuristic
(print-results *rrs* t 0)