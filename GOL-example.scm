(use-modules (cas cas))

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
  (list-states (state 0 colored (λ () (color 0 0 0)))
	       (state 1 colored (λ () (color 255 255 255)))))

(define gol-rules
  (list-rules (in-state 1 (is (cell lonely? -> 0)
			      (cell stable? -> 1)
			      (cell overcrowded? -> 0)))
	      (in-state 0 (is (cell reproducing? -> 1)))))

(set! rules gol-rules)
(set! states gol-states)
(set! neighborhood moore)
(set! world-ref with-wrap)
