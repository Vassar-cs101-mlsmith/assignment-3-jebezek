;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101  
; Fall 2018
; Assign 3
; Jessica Bezek
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 10) "solid" "cyan")
             (+ RADIUS 10) (/ HEIGHT 2) -13 13))

(define BALL-AT-RIGHT 
  (make-ball (circle (+ RADIUS 25) "solid" "magenta")
             (+ RADIUS 425) (/ HEIGHT 3) 3 3)) 

(define BALL-AT-TOP 
  (make-ball (circle RADIUS "solid" "yellow")
             (/ WIDTH 10) RADIUS -7 -7)) 

(define BALL-AT-BOTTOM 
  (make-ball (circle (+ RADIUS 18) "solid" "green")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 18)) -20 20))

; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB (list BALL-AT-LEFT BALL-AT-RIGHT BALL-AT-TOP BALL-AT-BOTTOM))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
(define (fun-for-ball b) 
  (...(ball-im b)...
   ...(ball-x b)...(ball-y b)...
   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
(define (fun-for-list-of-balls lob) 
  (cond
    [(empty? lob)...] 
    [else (...(fun-for-ball (first lob))...
           ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b)
  (/ (image-width (ball-im b)) 2)) 

(check-expect (ball-radius BALL-AT-LEFT) 35)
(check-expect (ball-radius BALL-AT-RIGHT) 50)
(check-expect (ball-radius BALL-AT-TOP) 25) 
(check-expect (ball-radius BALL-AT-BOTTOM) 43)


; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
  (<= (ball-y b) (ball-radius b)))

(check-expect (top-edge? BALL-AT-TOP) #true)
(check-expect (top-edge? (make-ball (circle (+ RADIUS 10) "solid" "cyan")
                                    (+ RADIUS 10) (+ RADIUS 10) -13 13)) #true)
(check-expect (top-edge? BALL-AT-BOTTOM) #false)
(check-expect (top-edge? BALL-AT-LEFT) #false)


; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
  (>= (ball-y b) (- HEIGHT (ball-radius b))))

(check-expect (bottom-edge? BALL-AT-TOP) #false)
(check-expect (bottom-edge? BALL-AT-BOTTOM) #true)
(check-expect (bottom-edge? BALL-AT-LEFT) #false)
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 25) "solid" "magenta")
                                       (+ RADIUS 400) (- HEIGHT (+ RADIUS 25)) -3 3)) #true)


; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (<= (ball-x b) (ball-radius b)))

(check-expect (left-edge? BALL-AT-TOP) #false)
(check-expect (left-edge? BALL-AT-BOTTOM) #false)
(check-expect (left-edge? BALL-AT-LEFT) #true)
(check-expect (left-edge? BALL-AT-RIGHT) #false)


; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b)
  (>= (ball-x b) (- WIDTH (ball-radius b))))

(check-expect (right-edge? BALL-AT-TOP) #false)
(check-expect (right-edge? BALL-AT-BOTTOM) #false)
(check-expect (right-edge? BALL-AT-LEFT) #false)
(check-expect (right-edge? BALL-AT-RIGHT) #true)


; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (ball-dx b) (- (ball-dy b))))

(check-expect (reverse-up-down BALL-AT-TOP)
              (make-ball (ball-im BALL-AT-TOP) (ball-x BALL-AT-TOP) (ball-y BALL-AT-TOP) (ball-dx BALL-AT-TOP) (- (ball-dy BALL-AT-TOP))))
(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 50) "solid" "green")
                                          (+ RADIUS 21) (/ HEIGHT 200) -18 -18)) (make-ball (circle (+ RADIUS 50) "solid" "green")
                                                                                            (+ RADIUS 21) (/ HEIGHT 200) -18 18))
(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 2) "solid" "yellow")
                                          (+ RADIUS 100) (/ HEIGHT 20) -8 -8)) (make-ball (circle (+ RADIUS 2) "solid" "yellow")
                                                                                          (+ RADIUS 100) (/ HEIGHT 20) -8 8))


; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (- (ball-dx b)) (ball-dy b)))

(check-expect (reverse-left-right BALL-AT-LEFT)
              (make-ball (ball-im BALL-AT-LEFT) (ball-x BALL-AT-LEFT) (ball-y BALL-AT-LEFT) (- (ball-dx BALL-AT-LEFT)) (ball-dy BALL-AT-LEFT)))
(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 50) "solid" "green") (+ RADIUS 200) (/ HEIGHT 15) -18 -18))
                                  (make-ball (circle (+ RADIUS 50) "solid" "green") (+ RADIUS 200) (/ HEIGHT 15) 18 -18))
(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 20) "solid" "magenta") (+ RADIUS 33) (/ HEIGHT 150) -3 3))
                                  (make-ball (circle (+ RADIUS 20) "solid" "magenta") (+ RADIUS 33) (/ HEIGHT 150) 3 3))
              

; ball -> ball
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (bottom-edge? b) (top-edge? b)) (reverse-up-down b)]
    [else b])) 

(check-expect (bounce-up-down BALL-AT-TOP)
              (make-ball (circle RADIUS "solid" "yellow") (/ WIDTH 10) RADIUS -7 7))
(check-expect (bounce-up-down BALL-AT-BOTTOM)
              (make-ball (ball-im BALL-AT-BOTTOM) (ball-x BALL-AT-BOTTOM) (ball-y BALL-AT-BOTTOM) (ball-dx BALL-AT-BOTTOM) (- (ball-dy BALL-AT-BOTTOM)))) 
(check-expect (bounce-up-down (make-ball (circle (+ RADIUS 18) "solid" "green")
                                         (/ WIDTH 4) (- HEIGHT (+ RADIUS 200)) -20 20)) (make-ball (circle (+ RADIUS 18) "solid" "green")
                                                                                                   (/ WIDTH 4) (- HEIGHT (+ RADIUS 200)) -20 20)) 


; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
  (cond
    [(or (left-edge? b) (right-edge? b)) (reverse-left-right b)]
    [else b]))

(check-expect (bounce-left-right BALL-AT-LEFT)
              (make-ball (circle (+ RADIUS 10) "solid" "cyan") (+ RADIUS 10) (/ HEIGHT 2) 13 13))
(check-expect (bounce-left-right BALL-AT-RIGHT)
              (make-ball (ball-im BALL-AT-RIGHT) (ball-x BALL-AT-RIGHT) (ball-y BALL-AT-RIGHT) (- (ball-dx BALL-AT-RIGHT)) (ball-dy BALL-AT-RIGHT)))
(check-expect (bounce-left-right (make-ball (circle RADIUS "solid" "yellow")
                                            (/ WIDTH 10) (+ RADIUS 3) -7 7))
              (make-ball (circle RADIUS "solid" "yellow")
                         (/ WIDTH 10) (+ RADIUS 3) -7 7))


; ball -> ball
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (make-ball (ball-im b) (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy b)) (ball-dx b) (ball-dy b)))

(check-expect (move-ball BALL-AT-TOP)
              (make-ball (ball-im BALL-AT-TOP) (+ (ball-x BALL-AT-TOP) (ball-dx BALL-AT-TOP)) (+ (ball-y BALL-AT-TOP) (ball-dy BALL-AT-TOP)) (ball-dx BALL-AT-TOP) (ball-dy BALL-AT-TOP))) 
(check-expect (move-ball BALL-AT-RIGHT)
              (make-ball (ball-im BALL-AT-RIGHT) (+ (ball-x BALL-AT-RIGHT) (ball-dx BALL-AT-RIGHT)) (+ (ball-y BALL-AT-RIGHT) (ball-dy BALL-AT-RIGHT)) (ball-dx BALL-AT-RIGHT) (ball-dy BALL-AT-RIGHT)))
             

; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond
    [(empty? lob) lob] 
    [(cons? lob) (cons (move-ball (bounce-up-down (bounce-left-right (first lob))))
                     (move-list-of-balls (rest lob)))]))

(check-expect (move-list-of-balls '()) '()) 
(check-expect (move-list-of-balls INIT-LOB)
              (cons (make-ball (circle (+ RADIUS 10) "solid" "cyan")
                               (+ (+ RADIUS 10) 13) (+ (/ HEIGHT 2) 13) 13 13)
                    (cons (make-ball (circle (+ RADIUS 25) "solid" "magenta")
                                     (+ (+ RADIUS 425) -3) (+ (/ HEIGHT 3) 3) -3 3)
                          (cons (make-ball (circle RADIUS "solid" "yellow")
                                           (+ (/ WIDTH 10) -7) (+ RADIUS 7) -7 7)
                                (cons (make-ball (circle (+ RADIUS 18) "solid" "green")
                                                 (+ (/ WIDTH 2) -20) (+ (- HEIGHT (+ RADIUS 18)) -20) -20 -20) '())))))

; ball image -> image 
; renders given ball b on given background bg
(define (render-ball b bg)
  (place-image (ball-im b) (ball-x b) (ball-y b) bg))

(check-expect (render-ball BALL-AT-TOP BACKGROUND)
              (place-image (ball-im BALL-AT-TOP) (ball-x BALL-AT-TOP) (ball-y BALL-AT-TOP) BACKGROUND))


; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))

(check-expect (render-balls '()) BACKGROUND)
(check-expect (render-balls (cons BALL-AT-RIGHT '()))
                            (place-image (ball-im BALL-AT-RIGHT) (ball-x BALL-AT-RIGHT) (ball-y BALL-AT-RIGHT) BACKGROUND)) 


; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(define (main w)
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls)))

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
(main INIT-LOB)