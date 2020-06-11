#lang scheme


;You can replace #f's with your function definitions and define more helper functions as you need to use this template.

; Solver function
(define TENTS-SOLUTION (lambda (liste)(VAR(SOLUTION (car(cdr(cdr liste))) (car liste) (car(cdr liste)) '()))))





(define VAR(lambda (X) (if X X #f)))

(define SOLUTION(lambda(trees rows columns liste)(if(or (member -1 rows) (member -1 columns)) #f (if(null? trees)
                                                      (if(and (CHECK   rows columns) (CHECKADJACENT liste)) liste #f)
                                                     (VAR(RETURN-FIRST-NOT-FALSE (lambda (x)
                                                        (VAR(SOLUTION (cdr trees) (DECREASE-NTH rows (car x)) (DECREASE-NTH columns (car(cdr x))) (cons x liste)))) (NEIGHBOR-LIST (car trees))))))))
(define REPLACE-NTH (lambda (liste n number) (if(= n 1)  (cons number (cdr liste))
                                                (cons (car liste) (REPLACE-NTH (cdr liste) (- n 1) number)))))
; Helper functionsZ
(define VAR2(lambda( X func liste) (if X X (RETURN-FIRST-NOT-FALSE func (cdr liste)))))
(define RETURN-FIRST-NOT-FALSE (lambda( func liste) (if (null? liste)#f (VAR2 (func(car liste)) func liste))))

(define ADJACENT (lambda(list1 list2) (if (and ( < (-(car list1 ) (car list2)) 2) ( > (-(car list1) (car list2)) -2) )
                                        (if (and( < (-(car(cdr list1)) (car(cdr list2))) 2) ( > (-(car(cdr list1)) (car(cdr list2))) -2)) #t #f) #f)))
                                        
 
(define ADJACENT-WITH-LIST (lambda (list1 list2) (if( null? list2) #f
                                                    (if( ADJACENT  list1 (car list2) ) #t (ADJACENT-WITH-LIST list1 (cdr list2)) ))))


(define NEIGHBOR-LIST (lambda (list1)  (list
                                        (list (car list1)  (+(car(cdr list1)) 1))
                                        (list (+(car list1) 1) (car(cdr list1)))
                                        (list (car list1)  (-(car(cdr list1)) 1))
                                        (list (-(car list1) 1) (car(cdr list1))))))


(define DECREASE-NTH (lambda (liste n ) (if(null? liste) '(-1) (if(= n 1)  (cons (- (car liste) 1) (cdr liste))
                                                (cons (car liste) (DECREASE-NTH (cdr liste) (- n 1) ))))))
(define COUNTROWCOLUMN(lambda(liste  row column) (if(null? liste) (list row column)
                                               (COUNTROWCOLUMN (cdr liste)  (DECREASE-NTH row (car(car liste))) (DECREASE-NTH column  (car(cdr(car liste))))))))
(define CHECK(lambda (rows columns) (if(or (null? rows) (null? columns)) #t
                                (if(and (zero? (car rows)) (zero? (car columns))) (CHECK (cdr rows) (cdr columns)) #f))))

(define CHECKADJACENT( lambda(liste) (if(null? liste) #t (if (ADJACENT-WITH-LIST (car liste) (cdr liste)) #f (CHECKADJACENT (cdr liste))))))

      
