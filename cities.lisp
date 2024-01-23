
;;  CITIES-MOVE struct
;; ----------------------------------------------
;;  Information sufficient for DO-MOVE to do a move

(defstruct (cities-move (:print-function show))
  from      ;; from city
  to        ;; to city
  distance  ;; distance
  )

;;  SHOW method for CITIES-MOVE struct
;; -------------------------------------------

(defmethod show ((mv cities-move) str depth)
  (declare (ignore depth))
  (format str "Cities MOVE:  ~A --[~A]--> ~A~%"
	  (cities-move-from mv)
	  (cities-move-distance mv)
	  (cities-move-to mv)))

;;  CITIES struct
;; --------------------------------------------
;;  Just specifies which city you're in currently.

(defstruct (cities (:print-function show))
  curr-loc
  )

;;  SHOW method for CITIES struct
;; -----------------------------------------------

(defmethod show ((ctz cities) str depth)
  (declare (ignore depth))
  (format str "CITIES: Current location: ~A~%" (cities-curr-loc ctz)))

;;  The following LET* includes all of the information from the MAP
;;  regarding distances between pairs of cities.  The FETCH-LEGAL-MOVES
;;  and DO-MOVE methods are defined in the body of this LET* so that they
;;  can access this info.

(let* ((trip-hash (make-hash-table :test #'equal))
       (trip-info '((arad zerind 75)
		    (arad timisoara 118)
		    (arad sibiu 140)
		    (zerind oradea 71)
		    (oradea sibiu 151)
		    (timisoara lugoj 111)
		    (lugoj mehadia 70)
		    (mehadia dobreta 75)
		    (dobreta craiova 120)
		    (craiova rimnicu_vilcea 146)
		    (rimnicu_vilcea sibiu 80)
		    (sibiu fagaras 99)
		    (fagaras bucharest 211)
		    (rimnicu_vilcea pitesti 97)
		    (craiova pitesti 138)
		    (pitesti bucharest 101)
		    (bucharest giurgiu 90)
		    (bucharest urziceni 85)
		    (urziceni hirsova 98)
		    (hirsova eforie 86)
		    (urziceni vaslui 142)
		    (vaslui iasi 92)
		    (iasi neamt 87))))
  ;; Walk through the list of MAP info
  (dolist (triple trip-info)
    ;; Each triple has the form (CITY-A CITY-B DISTANCE)
    (let ((city-a (first triple))
	  (city-b (second triple))
	  (distance (third triple)))
      ;; Push a CITIES-MOVE instance onto the list of legal moves for CITY-A
      (push (make-cities-move :from city-a :to city-b :distance distance)
	    (gethash city-a trip-hash))
      ;; Push a CITIES-MOVE instance onto the list of legal moves for CITY-B
      (push (make-cities-move :from city-b :to city-a :distance distance)
	    (gethash city-b trip-hash))))
  
  ;;  FETCH-LEGAL-MOVES
  ;; --------------------------------------------------
  ;;  Just look up the list of legal moves for the current city in the trip-hash
  
  (defmethod fetch-legal-moves ((state cities))
    (gethash (cities-curr-loc state) trip-hash))

  ;;  DO-MOVE
  ;; ------------------------------------------------------
  ;;  Just move to the TO city specified by the move and record the cost
  ;;  (i.e., distance traveled).  The return value is a pair (NEW-STATE COST).
  (defmethod do-move ((state cities) (mv cities-move))
    (list (make-cities :curr-loc (cities-move-to mv))
	  (cities-move-distance mv)))
  
  ;;  STATES-ARE-EQUAL?
  ;; -------------------------------------------
  ;;  .. if they are at the same city
  
  (defmethod states-are-equal? ((st1 cities) (st2 cities))
    (eql (cities-curr-loc st1) (cities-curr-loc st2)))
  
  ;;  STATE-IS-GOAL?
  ;; ---------------------------------
  ;;  .. if currently at Bucharest!
  
  (defmethod state-is-goal? ((st cities))
    (eql (cities-curr-loc st) 'bucharest))
  
  )

;; The followng LET creates and initializes a hash-table with all of the
;; straight-line distances (SLDs) from cities to Bucharest.
;; The CITIES-HEURISTIC function is defined in the body of the LET
;; so that it can access the hash-table.

(let ((sld-hash (make-hash-table)))
  (mapcar #'(lambda (city-sld-pair)
	      (setf (gethash (first city-sld-pair) sld-hash) (second city-sld-pair)))
	  '((arad 366)
	    (bucharest 0)
	    (craiova 160)
	    (dobreta 242)
	    (eforie 161)
	    (fagaras 178)
	    (giurgui 77)
	    (hirsova 151)
	    (iasi 226)
	    (lugoj 244)
	    (mehadia 241)
	    (neamt 234)
	    (oradea 380)
	    (pitesti 98)
	    (rimnicu_vilcea 193)
	    (sibiu 253)
	    (timisoara 329)
	    (urziceni 80)
	    (vaslui 199)
	    (zerind 374)))

  ;; CITIES-HEURISTIC
  ;; --------------------------------------------
  ;;  Just fetch the straight-line distance (SLD) to Bucharest from the current city
  
  (defun cities-heuristic (xnode)
    (gethash (cities-curr-loc (xnode-state xnode)) sld-hash)))

