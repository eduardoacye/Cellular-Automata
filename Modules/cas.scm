;;; DEFINITION OF THE CELLULAR AUTOMATA SYSTEM (CAS) MODULE
;;; THIS FILE IS LOCATED AT "/usr/share/guile/site/cas/cas.scm"
;;; THE PATH "/usr/share/guile/site/" IS BY DEFAULT IN GUILE'S
;;; %load-path.

;;; WRITTEN BY EDUARDO ACUNA Y. IN 2014

(define-module (cas cas)
  #:use-module (ice-9 threads)
  #:use-module (sfml sfml)
  #:export (start-simulation
	    list-rules
	    in-state
	    is
	    cell
	    list-states
	    state
	    start-simulation
	    color
	    neighborhood
	    world
	    old-world
	    view
	    rules
	    states
	    world-ref
	    moore
	    von-neumann
	    with-wrap
	    without-wrap
	    fps
	    neumannize!
	    moorelicious!))

;;; NOTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THIS PROGRAM USES THE SUBSET OF BINDINGS TO SFML 2.1
;;; THE INSTRUCTIONS TO COMPILE THE LIBRARY AND THE CON-
;;; FIGURATION OF THE MODULE ARE SHIPPED WITH THIS FILE.

;;; HOW TO READ THE CODE ;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;

;;; HOW TO DESIGN A BIDIMENTIONAL CELLULAR AUTOMATA WITH CAS ;;;;;;;;;;;;
;;;
;;;
;;;
;;;

;;; SIMULATION FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start-simulation cs wr wc)
  (set! cell-size cs)
  (set! world-rows wr)
  (set! world-cols wc)
  (set! paused #t)
  (set! world (make-array 0 world-rows world-cols))
  (set! old-world (make-array 0 world-rows world-cols))
  (make-thread loop))

(define (wrap!)
  (set! world-ref with-wrap))

(define (unwrap!)
  (set! world-ref without-wrap))

(define (fps n)
  (window-set-framerate-limit! window n))

(define (neumannize!)
  (set! neighborhood von-neumann))

(define (moorelicious!)
  (set! neighborhood moore))

;;; LANGUAGE CONSTRUCTS FOR THE CELLULAR AUTOMATA SYSTEM. ;;;;;;;;;;;;;;
(define list-rules list)
(define in-state cons)
(define is list)
(define-syntax cell
  (syntax-rules (->)
    ((cell pred -> val)
     (cons pred val))))
(define list-states list)
(define-syntax state
  (syntax-rules (colored)
    ((state a colored b)
     (cons a b))))

;;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define neighborhood #f)
(define world-ref #f)
(define rules #f)
(define states #f)

;;; PARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cell-size #f)
(define world-rows #f)
(define world-cols #f)

;;; DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define world #f)
(define old-world #f)

;;; PREDEFINED WORLD FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (without-wrap w i j)
  "GET THE [i j] CELL OF w WITHOUT WRAPPING THE WORLD"
  (if (array-in-bounds? w i j)
      (array-ref w i j)
      0))

(define (with-wrap w i j)
  "GET THE [i j] CELL OF w WRAPPING THE WORLD"
  (let ((rows world-rows)
	(cols world-cols))
    (array-ref w (modulo i rows) (modulo j cols))))

;;; PREDEFINED NEIGHBORHOODS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (von-neumann w i j)
  "RETURN A LIST OF THE VON-NEUMANN NEIGHBORS OF THE [i j] CELL"
  (list (world-ref w (1- i) j)
	(world-ref w (1+ i) j)
	(world-ref w i (1- j))
	(world-ref w i (1+ j))))

(define (moore w i j)
  "RETURN A LIST OF THE MOORE NEIGHBORS OF THE [i j] CELL"
  (append (von-neumann w i j)
	  (list (world-ref w (1- i) (1+ j))
		(world-ref w (1+ i) (1+ j))
		(world-ref w (1- i) (1- j))
		(world-ref w (1+ i) (1- j)))))

;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define window #f)
(define event #f)
(define paused #f)
(define background #f)

;;; AUXILIARY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (view w)
  "PRITTY PRINTS THE 2D WORLD"
  (define rows world-rows)
  (define cols world-cols)
  (let loop-rows ((r 0))
    (unless (>= r rows)
      (let loop-elms ((c 0))
	(unless (>= c cols)
	  (display (array-ref w r c))
	  (display " ")
	  (loop-elms (1+ c))))
      (newline)
      (loop-rows (1+ r))))
  (newline))

(define (copy-world)
  "UPDATE OLD-WORLD TO HAVE WORLD DATA"
  (array-copy! world old-world))

(define (for-each-cell world apply-rules)
  "ITERATE OVER WORLD APPLYING THE GIVEN FUNCTION F(i,j)"
  (array-index-map! world apply-rules))

;;; CELLULAR AUTOMATA SYSTEM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (apply-rules i j)
  (define (find-rule alist i j)
    "FINDS THE APPROPIATE RULE FOR THE CELL OR IDENTITY"
    (cond ((null? alist)
	   (lambda (i j)
	     (world-ref old-world i j)))
	  (((car (car alist)) i j) (cdr (car alist)))
	  (else (find-rule (cdr alist) i j))))
  (define corresponding-rules (assoc-ref rules (world-ref old-world i j)))
  (define change (find-rule corresponding-rules i j))
  (if (procedure? change)
      (change i j)
      change))

(define (copy-world)
  "UPDATE OLD-WORLD TO HAVE WORLD DATA"
  (array-copy! world old-world))

(define (for-each-cell world apply-rules)
  "ITERATE OVER WORLD APPLYING THE GIVEN FUNCTION F(i,j)"
  (array-index-map! world apply-rules))

;;; GRAPHICS RELATED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (color r g b)
  (color-make r g b 255))

(define (init-sfml)
  "SFML VARIABLES AND PARAMETERS INITIALIZATION"
  (set! window (window-make (* cell-size world-cols)
			    (* cell-size world-rows)
			    32
			    "C.A.S. (PAUSE)"))
  (fps 15)
  (set! event (event-make))
  (copy-world)
  (set! background (color 20 20 40)))
(define (render)
  "RENDERS THE WORLD INTO A GRAPHIC WINDOW"
  (define (draw-rectangle! window i j)
    (define r (rectangle-make cell-size cell-size))
    (rectangle-set-position! r (* cell-size j) (* cell-size i))
    (rectangle-set-outline-color! r background)
    (rectangle-set-outline-thickness! r (/ cell-size 10.0))
    (rectangle-set-fill-color! r ((assoc-ref states
					     (world-ref world i j))))
    (window-draw-rectangle! window r))
  (window-clear! window background)
  (for-each-cell world (lambda (i j)
			 (draw-rectangle! window i j)
			 (world-ref world i j)))
  (window-display! window))

(define (proccess-input)
  "CHECKS FOR USER INPUT [PAUSE SIMULATION, EXIT SIMULATION, CLEAR SIMULATION]"
  (define (get-mouse-column window)
    (floor (/ (car (mouse-position window))
	      cell-size)))
  (define (get-mouse-row window)
    (floor (/ (cdr (mouse-position window))
	      cell-size)))
  (while (window-poll-event! window event)
    (case (event-type event)
      ((Closed)
       (window-close window))
      ((KeyPressed)
       (case (event-key-code event)
	 ((Escape) (begin
		     (array-fill! world 0)
		     (array-fill! old-world 0)))
	 ((Space) (if paused
		      (begin
			(set! paused #f)
			(window-set-title! window "C.A.S."))
		      (begin
			(set! paused #t)
			(window-set-title! window "C.A.S. (PAUSE)"))))))
      ((MouseButtonPressed)
       (let ((mouse-x (get-mouse-column window))
	     (mouse-y (get-mouse-row window)))
	 (array-set! world
		     (modulo (1+ (world-ref world
					    mouse-y
					    mouse-x))
			     (length states))
		     mouse-y
		     mouse-x))))))

(define (loop)
  "SIMULATION LOOP"
  (init-sfml)
  (while (window-open? window)
    (unless paused
      (copy-world)
      (for-each-cell world apply-rules))
    (render)
    (proccess-input)))

;;; GAME OF LIFE EXAMPLE
(define (lonely? i j)
  (define n (apply + (neighborhood old-world i j)))
  (< n 2))

(define (stable? i j)
  (define n (apply + (neighborhood old-world i j)))
  (or (= n 2)
      (= n 3)))

(define (overcrowded? i j)
  (define n (apply + (neighborhood old-world i j)))
  (> n 3))

(define (reproducing? i j)
  (define n (apply + (neighborhood old-world i j)))
  (= n 3))


(define gol-states
  (list-states (state 0 colored (λ () (color 10 10 10)))
	       (state 1 colored (λ () (color 191 191 191)))))

(define gol-rules
  (list-rules (in-state 1 (is (cell lonely? -> 0)
			      (cell stable? -> 1)
			      (cell overcrowded? -> 0)))
	      (in-state 0 (is (cell reproducing? -> 1)))))

(set! rules gol-rules)
(set! states gol-states)
(set! neighborhood moore)
(set! world-ref with-wrap)
