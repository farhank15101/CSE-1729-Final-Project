


(define (make-stack)
  (let (( S '()))
    (define (empty?)
      (null? S))
    (define (push x)
      (set! S (cons x S)))
    (define (pop)
      (let ((R (car S)))
      (begin (set! S (cdr S)) R)))
    (define (top)
      (car S))
    (lambda (method)
      (cond ((eq? method 'pop) pop)
            ((eq? method 'top) top)
            ((eq? method 'push) push)
            ((eq? method 'empty) empty?)))))


(define (make-queue)
  (let ((head '())
        (tail '()))
    (define (value n ) (car n))
    (define (next n) (cdr n))
    (define (empty?) (null?  head))
    (define (front) (value head))
    (define (enqueue x)
      (let ((new-val (cons x '())))
        (begin (if (empty?)
                   (set! head new-val)
                   (set-cdr! tail new-val))
               (set! tail new-val))))
    (define (dequeue)
      (let ((return (value head)))
        (if (eq? head tail)
            (begin (set! head '())
                   (set! tail '())
                   return)
            (begin (set! head (next head))
                   return))))
    (lambda (method)
      (cond ((eq? method 'empty) empty?)
            ((eq? method 'front) front)
            ((eq? method 'dequeue) dequeue)
            ((eq? method 'enqueue) enqueue)))))


(define (make-set is-equal? before?)
  (let ((empty-tree '()))
    (define (empty?)
      (null? empty-tree))
    (define (insert x)
      (define (helper x T)
        (if (null? T)
            (list x '() '())
            (if (is-equal? x (car T))
                T
                (if (before? x (car T))
                    (list (car T) (helper x (cadr T)) (caddr T))
                    (list (car T) (cadr T) (helper x (caddr T)))))))
      (set! empty-tree (helper x empty-tree)))
    (define (element? x)
      (define (helper x T)
        (if (null? T)
            #f
            (if (is-equal? x (car T))
                #t
                (if (before? x (car T))
                    (helper x (cadr T))
                    (helper x (caddr T))))))
      (helper x empty-tree))
    (define (get x)
        (define (helper x T)
          (if (null? T)
              #f
              (if (is-equal? x (car T))
                  (car T)
                  (if (before? x (car T))
                      (helper x (cadr T))
                       (helper x (caddr T))))))
      (helper x empty-tree))
                      
                      
                  
                  
              
    (define (list-elements)
      (define (helper T)
        (if (null? T)
            '()
            (append (helper (cadr T)) (list (car T)) (helper (caddr T)))))
      (helper empty-tree))
    (lambda (method)
      (cond ((eq? method 'insert) insert)
            ((eq? method 'element?) element?)
            ((eq? method 'get) get)
            ((eq? method 'list-elements) list-elements)
            ((eq? method 'empty) empty?)))))


(define (make-heap before?)
  (let ((empty-heap '()))
    (define (create-heap value left right)
      (list value left right))
    (define (heap-root) (car empty-heap))
    (define (left T) (cadr T))
    (define (right T) (caddr T))
    (define (empty?)
      (null? empty-heap))
    (define (heap-insert x)
      (define (helper before? x T)
        (define (value T) (car T))
        (if (null? T)
            (create-heap x '() '())
            (if (before? x (value T))
                   (create-heap x (right T) (helper before? (value T) (left T)))
                   (create-heap (value T) (right T) (helper before? x (left T))))))
      (set! empty-heap (helper before? x empty-heap)))
    (define (combine before? Ha Hb)
      (define (h-min H) (car H))
  (cond ((null? Ha) Hb)
        ((null? Hb) Ha)
        (( before? (h-min Ha) (h-min Hb)) (create-heap (h-min Ha) Hb (combine before? (left Ha) (right Ha))))
        (else (create-heap (h-min Hb) Ha (combine before? (left Hb) (right Hb ))))))
    (define (heap-remove)
      (let ((old-root (heap-root)))
      (if (null? empty-heap)
          '()
          (begin (set! empty-heap (combine before? (left empty-heap) (right empty-heap))) old-root))))
    (lambda (method)
      (cond ((eq? method 'insert) heap-insert)
            ((eq? method 'remove) heap-remove)
            ((eq? method 'empty) empty?)
            ((eq? method 'heap-root) heap-root)))))

(define (make-location n x y)

  (define (same-location? a b)
    (if (string=? (((car a) 'get-name)) (((car b) 'get-name)))
        #t
        #f))
    

  (define (location<? a b)
    (if (string<? (((car a) 'get-name)) (((car b) 'get-name))) 
        #t
        #f))
  
(let ((destinations (make-set same-location? location<?)))
    
(define (get-name)
    n)
  (define (get-position)
    (cons x y))
  (define (is-equal? l)
    (if (string=? ((l 'get-name)) n)
        #t
        #f))
  (define (add-route location distance)
    ((destinations 'insert) (cons location distance)))
  (define (get-routes)
    ((destinations 'list-elements)))
  (define (get-distance-to d)
   (cdr ((destinations 'get) (list d))))
  (lambda (method)
    (cond ((eq? method 'get-name) get-name)
          ((eq? method 'get-position) get-position)
          ((eq? method 'is-equal?) is-equal?)
          ((eq? method 'get-distance-to) get-distance-to)
          ((eq? method 'add-route) add-route)
          ((eq? method 'get-routes) get-routes)))))


(define (make-route r)
  (let ((route r))
    (define (add-stop s)
      (set! route (cons s route)))
    (define (get-last-stop)
      (car route))
    (define (get-route)
      (reverse route))
    (define (copy-route)
      (make-route route))
    (define (on-route? s)
      (define (helper s T)
        (if (null? T)
            #f
        (if ((s 'is-equal?) (car T))
            #t
            (helper s (cdr T)))))
     (helper s route))
    (define (get-distance)
      (define (helper r)
        (if (null? r)
            0
        
        (if (null? (cdr r))
            0
            (+ (((car r) 'get-distance-to) (cadr r)) (helper (cdr r))))))
      (helper route))
    (lambda (method)
      (cond ((eq? method 'add-stop) add-stop)
            ((eq? method 'get-route) get-route)
            ((eq? method 'on-route) on-route?)
            ((eq? method 'get-distance) get-distance)
            ((eq? method 'get-last-stop) get-last-stop)
            ((eq? method 'copy) copy-route)))))

(define (abstract-search container s d equal?)
  (define (process-routes stop locations)
    ;each stop is a location/route pair
    ;locations is a list of location/distance pairs
    (if (null? locations)
        locations
        (if (((cdr stop) 'on-route) (caar locations))
            (process-routes stop (cdr locations))
            (begin ((container 'add) (cons (caar locations)
                                           (((cdr stop) 'copy))))
                   (process-routes stop (cdr locations))))))
  (define (search)
    (if ((container 'empty))
        ;search failure, no solutions
        (list)
        ;Get next node to expand
        (let* ((next ((container 'remove)))
               (new-destinations (((car next) 'get-routes))))
          ;expand node
          (begin (((cdr next) 'add-stop) (car next))
                 (if (equal? (car next) d)
                     ;We reached the destination, show the route
                     (((cdr next) 'get-route))
                     ;Not there yet, keep searching
                     (begin (process-routes next new-destinations) 
                            (search)))))))
  
  (begin ((container 'add) (cons s (make-route (list))))
         (search)))

(define n (make-location "N" 0.2 1))
(define p (make-location "P" 1.35 4.25))
(define u (make-location "U" 2.15 0.875))  
(define e (make-location "E" 3.42 2.125))  
(define j (make-location "J" 3.8 4.575))  
(define m (make-location "M" 6.7 3.875))  
(define s (make-location "S" 6.7 1.875))  
(define v (make-location "V" 5.6 0.1))

((n 'add-route) p 3.5)
((n 'add-route) u 2)
((p 'add-route) n 3.5)
((p 'add-route) u 3.5)
((p 'add-route) e 3)
((p 'add-route) j 2.5)
((u 'add-route) n 2)
((u 'add-route) p 3.5)
((u 'add-route) e 1.8)
((e 'add-route) u 1.8)
((e 'add-route) p 3)
((e 'add-route) j 2.5)
((e 'add-route) v 1.8)
((j 'add-route) p 2.5)
((j 'add-route) e 2.5)
((j 'add-route) m 3)
((v 'add-route) e 3)
((v 'add-route) m 4)
((m 'add-route) j 3)
((m 'add-route) v 4)
((m 'add-route) s 2)

(define (make-queue-container)
  (let ((Q (make-queue)))
    (lambda (method)
      (cond ((eq? method 'add) (Q 'enqueue))
            ((eq? method 'remove) (Q 'dequeue))
            ((eq? method 'empty) (Q 'empty))))))

(define (get-directions lst)
  (if (null? lst)
      '()
      (cons (((car lst) 'get-name)) (get-directions (cdr lst)))))


  
 (define (same-location? a b)
    (if (string=? (( a 'get-name)) (( b 'get-name)))
        #t
        #f))  
  

(define (bfs s d)
  (get-directions (abstract-search (make-queue-container) s d same-location?)))

(bfs n s)

(define (make-stack-container)
  (let ((S (make-stack)))
    (lambda (method)
      (cond ((eq? method 'add) (S 'push))
            ((eq? method 'remove) (S 'pop))
            ((eq? method 'empty) (S 'empty))))))

(define (dfs s d)
  (get-directions (abstract-search (make-stack-container) s d same-location?)))

(define (make-pq-container f)
  (let ((H (make-heap f)))
    (lambda (method)
      (cond ((eq? method 'add) (H 'insert))
            ((eq? method 'remove) (H 'remove))
            ((eq? method 'empty) (H 'empty))))))
(define (A* s d)
  (define (build-heuristic)
    (define (distance x y)
      (let ((x-loc ((x 'get-position)))
            (y-loc ((y 'get-position))))
        (sqrt (+ (abs (- (car x-loc) (car y-loc)))
                 (abs (- (cdr x-loc) (cdr y-loc)))))))
    ;d is the desired final destination
    (lambda (a b)
      (< (+ (((cdr a) 'get-distance)) (distance (car a) d))
         (+ (((cdr b) 'get-distance)) (distance (car b) d)))))
  (get-directions (abstract-search (make-pq-container (build-heuristic)) s d same-location?)))


(A* n s)











  








  
      
      
    
      
          
      
      
    
      
    
      
    
      
    
    
  


  
  
  
    
    
    
    
  
    
  
    
          
      
        
    
            
            
            
    
    
          
      
    
    
      
      
    
    
        
    
   
    
    
                    
                
            



  

   
                  



                   
     
    
    
        





