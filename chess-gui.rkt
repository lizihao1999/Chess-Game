#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "chess-logic.rkt")
(require "chess-graphics.rkt")
(require "../include/cs151-universe.rkt")

(define-struct Point
  ([x : Integer]
   [y : Integer]))

(define-struct ChessWorld
  ([running? : Boolean]
   [point : Point]
   [game : ChessGame]
   [flag : Boolean]
   [selected : (Optional Loc)]
   [evaluated : (Optional Loc)] 
   [length : Integer]
   [quit? : Boolean]))

(: text-under-raw : ChessGame -> Image)
;;text the coordinates of the mouse and the location
(define (text-under-raw c)
  (local
    {(: turn Player)
     (define turn (whoseturn c))}
  (cond
    [(checkmate? c) (text "Checkmate! You lose!" 30 "black")]
    [(stalemate? c) (text "Stalemate! Draw!" 30 "black")]
    [else (cond
            [(in-check? c) (above (showplayer turn) (text "You are in check!" 30 "black"))]
            [else (showplayer turn)])])))

(: showplayer : Player -> Image)
;;text a player's turn
(define (showplayer p)
  (match p
    ['White (text "White's turn" 20 "black")]
    ['Black (text "Black's turn" 20 "black")]))

(: hint Image)
;;this is a function to draw the hint of promotion under the chess board
(define hint
  (above (text "when your pawn reaches the boundry for promotion," 15 "black")
         (text "after making a move, press q(keyboard) for Queen,"  15 "black")
         (text "k for Knight, r for Rook, b for Bishop" 15 "black")))

(: text-under : ChessGame -> Image)
;;combine all the information and the hint under the board
(define (text-under c)
  (above (text-under-raw c) hint))

(: point->loc : Point Integer -> (Optional Loc))
;;this is a function to transform a given point to a Loc 
(define (point->loc point len)
  (match point
    [(Point x y)
     (cond
       [(or (>= x (* 8 len)) (>= y (* 8 len))) 'None]
       [else (Some (Loc (s-to-f (i-to-l (+ 1 (quotient x len)))) (i-to-r (- 9 (+ 1 (quotient y len))))))])]))
(check-expect (point->loc (Point 50 100) 60) (Some (Loc 'A 7)))
    
;;eyeball check
(: draw-raw : ChessWorld -> Image)
;;helper fucntion of draw the world without the coordiantes of mouse
(define (draw-raw w)
  (match w
    [(ChessWorld b1 point game flag se ev len b2)
     (local
       {(: x Integer)
        (define x (+ (* (quotient (Point-x point) len) len) (quotient len 2)))
        (: y Integer)
        (define y (+ (* (quotient (Point-y point) len) len) (quotient len 2)))}
       (match se
         ['None (place-image (square len "solid" (make-color 200 30 30 50)) x y (above (board->image (ChessGame-board game) len) (text-under game)))]
         [(Some (Loc file rank)) (place-image (square len "solid" (make-color 30 30 200 50)) (- (* (l-to-i file) len) (quotient len 2)) (- (* (- 9 rank) len) (quotient len 2)) (place-image (square len "solid" (make-color 200 30 30 50)) x y (above (board->image (ChessGame-board game) len) (text-under game))))]))]))

(: draw-chess-world : ChessWorld -> Image)
;;this is to draw the world, the outline red square is to remind the player the previous step
(define (draw-chess-world w)
  (match w
    [(ChessWorld b1 point game flag se ev len b2)
     (match ev
       ['None (draw-raw w)]
       [(Some (Loc file rank))
        (place-image (square len "outline" "red") (- (* (l-to-i file) len) (quotient len 2)) (- (* (- 9 rank) len) (quotient len 2)) (draw-raw w))])]))

;;eyeball check
(: handle-click : ChessWorld Integer Integer Mouse-Event -> ChessWorld)
;;usemouseevent to make world
(define (handle-click w x y z)
  (match w
    [(ChessWorld b1 point game flag se ev len b2)
     (cond
       [(or (checkmate? game) (stalemate? game)) (ChessWorld #f point game flag se ev len #t)]
       [else (match z
               ["move" (ChessWorld b1 (Point x y) game flag se ev len b2)]
               ["button-down"
                (local
                  {(: sloc (Optional Loc))
                   (define sloc (point->loc (Point x y) len))
                   (: b Board)
                   (define b (ChessGame-board game))}
                  (match sloc
                    ['None w]
                    [(Some (Loc x1 y1)) (match se
                                          ['None (ChessWorld b1 point game flag sloc ev len b2)]
                                          [else (cond
                                                  [(loc=? (Loc x1 y1) (val-of se)) (ChessWorld b1 point game flag 'None ev len b2)]
                                                  [else (local
                                                          {(: square1 Square)
                                                           (define square1 (board-ref b (val-of se)))
                                                           (: square2 Square)
                                                           (define square2 (board-ref b (Loc x1 y1)))}
                                                          (match square1
                                                            ['None (ChessWorld b1 point game flag sloc ev len b2)]
                                                            [(Some (Piece 'Pawn color))
                                                              (cond
                                                               [(legal-move? game (Move (val-of se) (Loc x1 y1) (val-of square1) (Some (Piece 'Pawn (reverse color))) 'None))
                                                                (ChessWorld b1 point (apply-move game (Move (val-of se) (Loc x1 y1) (val-of square1) (Some (Piece 'Pawn (reverse color))) 'None))
                                                                       flag 'None sloc len b2)]
                                                               [(legal-move? game (Move (val-of se) (Loc x1 y1) (val-of square1) square2 'None))
                                                                (cond
                                                                  [(or (= 1 y1) (= 8 y1)) (ChessWorld b1 point game (not flag) se sloc len b2)]
                                                                  [else (ChessWorld b1 point (apply-move game (Move (val-of se) (Loc x1 y1) (val-of square1) square2 'None))
                                                                               flag 'None sloc len b2)])]
                                                               [else
                                                                (ChessWorld b1 point game flag sloc ev len b2)])]
                                                            [_ (cond
                                                                 [(legal-move? game (Move (val-of se) (Loc x1 y1) (val-of square1) square2 'None))
                                                                  (ChessWorld b1 point (apply-move game (Move (val-of se) (Loc x1 y1) (val-of square1) square2 'None))
                                                                         flag 'None sloc len b2)]
                                                                 [else (ChessWorld b1 point game flag sloc ev len b2)])]))])])]))]
                                                            
               [_ w])])]))
                                                  
(: reverse : Player -> Player)
;;this is to reverse the color of player
(define (reverse p)
  (match p
    ['Black 'White]
    ['White 'Black]))
(check-expect (reverse 'Black) 'White)

(: key : ChessWorld String -> ChessWorld)
;;press "q" "k" "r" "b" for promotion
(define (key w string)
  (match w
    [(ChessWorld b1 point game #f se ev len b2) w]
    [(ChessWorld b1 point game #t se ev len b2)
     (local
       {(: square1 Square)
        (define square1 (board-ref (ChessGame-board game) (val-of se)))
        (: square2 Square)
        (define square2 (board-ref (ChessGame-board game) (val-of ev)))}
     (match string
       ["q" (ChessWorld b1 point (apply-move game (Move (val-of se) (val-of ev) (val-of square1) square2 (Some 'Queen))) #f 'None ev len b2)]
       ["k" (ChessWorld b1 point (apply-move game (Move (val-of se) (val-of ev) (val-of square1) square2 (Some 'Knight))) #f 'None ev len b2)]
       ["r" (ChessWorld b1 point (apply-move game (Move (val-of se) (val-of ev) (val-of square1) square2 (Some 'Rook))) #f 'None ev len b2)]
       ["b" (ChessWorld b1 point (apply-move game (Move (val-of se) (val-of ev) (val-of square1) square2 (Some 'Bishop))) #f 'None ev len b2)]
       [_ w]))]))
;;eyeball check
;     
;    
(: play-from  : ChessGame Integer -> ChessWorld)
;;run the world from a given game
(define (play-from  c len) (cond
                                  [(< len 30) (error "size too small!")]
                                  [(> len 80) (error "size too big!")]
                                  [else (big-bang (ChessWorld #t (Point 0 0) c #f 'None 'None len #f) : ChessWorld
                                                  [to-draw draw-chess-world]
                                                  [name "project2"]
                                                  [on-key key]
                                                  [on-mouse handle-click]
                                                  [stop-when ChessWorld-quit?])]))

(: play-new  : Integer -> ChessWorld)
;;start a new game
(define (play-new len)
  (play-from (ChessGame starting-board '()) len))

(: new-chess-world : Integer -> ChessWorld)
;;new world
(define (new-chess-world len)
  (world-from-game (ChessGame starting-board '()) len))

(: world-from-game : ChessGame Integer -> ChessWorld)
;;world from a given
(define (world-from-game c len)
  (cond
    [(< len 30) (error "size too small!")]
    [(> len 80) (error "size too big!")]
    [(ChessWorld #t (Point 0 0) c #f 'None 'None len #f)]))
  
  



;eyeball check
(test)