#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "chess-logic.rkt")

;; ==== ==== ==== ====
;; external interface
;; Adam Shaw, Oct 7 2017
(provide board->image)
(: board->image : Board Integer -> Image)
(define (board->image b length)
  (board-image-raw b 1 length))


(: board-image-raw : Board Integer Integer -> Image)
;;draw an image
(define (board-image-raw b i length)
  (match b
    ['() empty-image]
    [_ (local
         {(: l1 Board)
          (define l1 (take b 8))}
         (above (board-image-raw (drop b 8) (+ 1 i) length)
                (row l1 i length)))]))

(: row : (Listof Square) Integer Integer -> Image)
(define (row l i length)
  (row-raw l i 1 length))

(: row-raw : (Listof Square) Integer Integer Integer -> Image)
;;draw a row of board
(define (row-raw l i lie length)
  (match l
    ['() empty-image]
    [(cons first rest) (match first
                         ['None (beside (basic i lie length) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Pawn 'Black)) (beside (overlay (text  (string #\♟) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Rook 'Black)) (beside (overlay (text  (string #\♜) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Bishop 'Black)) (beside (overlay (text  (string #\♝) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Knight 'Black)) (beside (overlay (text  (string #\♞) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'King 'Black)) (beside (overlay (text  (string #\♚) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Queen 'Black))(beside (overlay (text  (string #\♛) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Pawn 'White)) (beside (overlay (text  (string #\♙) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Rook 'White)) (beside (overlay (text  (string #\♖) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Bishop 'White)) (beside (overlay (text (string #\♗) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Knight 'White)) (beside (overlay (text  (string #\♘) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'King 'White)) (beside (overlay (text (string #\♔) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))]
                         [(Some (Piece 'Queen 'White)) (beside (overlay (text  (string #\♕) 30 "black") (tosquare i lie length)) (row-raw rest i (+ 1 lie) length))])]))

(: basic : Integer Integer Integer -> Image)
(define (basic i1 i2 l)
  (overlay (text (tostring i1 i2) 20 "black")
           (tosquare i1 i2 l)))

(: tostring : Integer Integer -> String)
(define (tostring i1 i2)
  (string-append (toletter i2) (number->string i1)))

(: toletter : Integer -> String)
(define (toletter i)
  (match i
    [1 "a"]
    [2 "b"]
    [3 "c"]
    [4 "d"]
    [5 "e"]
    [6 "f"]
    [7 "g"]
    [8 "h"]))

(: tosquare : Integer Integer Integer -> Image)
(define (tosquare i1 i2 l)
  (cond
    [(odd? (+ i1 i2)) (overlay (square l "solid" "white") (square l "outline" "black"))]
    [else (overlay (square l "solid" "orange") (square l "outline" "black"))]))
                 

         


  
         