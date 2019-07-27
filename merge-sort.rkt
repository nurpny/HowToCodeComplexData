;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; merge sort

;; listofNumber -> listofNumber
;; produces sorted list in ascending order using merge sort (https://en.wikipedia.org/wiki/Merge_sort)

(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 2)) (list 2))
(check-expect (merge-sort (list 1 2)) (list 1 2))
(check-expect (merge-sort (list 4 3)) (list 3 4))
(check-expect (merge-sort (list 6 5 3 1 8 7 2 4)) (list 1 2 3 4 5 6 7 8))
              
;(define (merge-sort lon) lon)

;; template according to generative recursion
(define (merge-sort lon)
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) lon]
        [else
         (merge (merge-sort (take lon (quotient (length lon) 2)))      ;first length/2 elements
                (merge-sort (drop lon (quotient (length lon) 2))))]))  ;second length/2 elements



;; listofNumber, listofNumber -> listofNumber
;; given two sorted lists of numbers, produce a new sorted merged list

(check-expect (merge empty empty) empty)
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge empty (list 1)) (list 1))
(check-expect (merge (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (merge (list 1 4 5) (list 2 3 6)) (list 1 2 3 4 5 6))

;(define (merge lon1 lon2) lon1)

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (if (< (first lon1) (first lon2))
             (cons (first lon1) (merge (rest lon1) lon2))
             (cons (first lon2) (merge lon1 (rest lon2))))]))


;; listofNumber, Natural (1, lengthoflist] -> listofNumber
;; produce list of first n elements of lon / list after droping first n elements
;; assume: list length > 2

(check-expect (take (list 1 2) 1) (list 1))
(check-expect (take (list 1 2) 2) (list 1 2))
(check-expect (take (list 1 2 3 4) 2) (list 1 2))
(check-expect (drop (list 1 2) 1) (list 2))
(check-expect (drop (list 1 2) 2) empty)
(check-expect (drop (list 1 2 3 4) 2) (list 3 4))


(define (take lon n)
  (cond [(zero? n) empty]
        [else (cons (first lon) (take (rest lon) (sub1 n)))]))

(define (drop lon n)
  (cond [(= 1 n) (rest lon)]
        [else (drop (rest lon) (sub1 n))]))