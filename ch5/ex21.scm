;; Run as: scheme --load ./regsim-improved.scm --load ex21.scm


(define examples
  (list
   '()
   '1
   '(1 . 1)
   '(1 2 3)
   '((1 . 2) . 3)
   '(1 . (2 . 3))
   '((1 . 2) . (3 . 4))
   '(((1 2) . (3 4)) ((1 2) . (3 4)))
   ))


;; a)

(define count-leaves-machine
  (make-machine
   (list (list 'cons cons) (list 'cdr cdr) (list 'car car) (list 'pair? pair?)
         (list 'set-car! set-car!) (list 'set-cdr! set-cdr!)
         (list 'null? null?) (list '+ +))
   '(start
     (assign continue (label done))
     loop

     (test (op null?) (reg tree))
     (branch (label null-case))

     (test (op pair?) (reg tree))
     (branch (label pair-case))

     ;; base case
     (assign val (const 1))
     (goto (reg continue))

     pair-case
     (save stash)
     (save continue)
     (save tree)
     (assign continue (label after-recursion-1))
     (assign tree (op car) (reg tree))
     (goto (label loop))

     after-recursion-1
     (restore tree)
     (restore continue)
     (restore stash)
     (assign stash (reg val))
     (save stash)
     (save continue)
     (save tree)
     (assign continue (label after-recursion-2))
     (assign tree (op cdr) (reg tree))
     (goto (label loop))

     after-recursion-2
     (restore tree)
     (restore continue)
     (restore stash)
     (assign val (op +) (reg val) (reg stash))
     (goto (reg continue))

     null-case
     (assign val (const 0))
     (goto (reg continue))

     done)))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))


(define (run-count-leaves-machine tree)
  (set-register-contents! count-leaves-machine 'tree tree)
  ;; (count-leaves-machine 'enable-tracing)
  ;; (((count-leaves-machine 'get-register) 'tree) 'enable-tracing)
  ;; (((count-leaves-machine 'get-register) 'val) 'enable-tracing)
  ;; (((count-leaves-machine 'get-register) 'stash) 'enable-tracing)
  ;; (((count-leaves-machine 'get-register) 'continue) 'enable-tracing)

  (start count-leaves-machine)
  (get-register-contents count-leaves-machine 'val))

(pp "")
(pp (map count-leaves examples))
(pp (map run-count-leaves-machine examples))




;; b)

(define count-leaves-explicit-machine
  (make-machine
   (list (list 'cdr cdr) (list 'car car)
         (list 'pair? pair?) (list 'null? null?) (list '+ +))
   '(start
     (assign val (const 0))
     (assign continue (label end))

     loop
     (test (op null?) (reg tree))
     (branch (label null-case))

     (test (op pair?) (reg tree))
     (branch (label recursive-case))

     ;; else a single value
     (assign val (op +) (reg val) (const 1))
     (goto (reg continue))


     null-case
     ;; (assign val (reg val))
     (goto (reg continue))

     recursive-case
     (save continue)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label after-rec-1))
     (goto (label loop))

     after-rec-1
     ;; (restore continue)
     (restore tree) ; "peek" at tree, for use below
     (save tree)
     ;; (save continue)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-rec-2))
     ;; assign val val
     (goto (label loop))

     after-rec-2
     (restore tree)
     (restore continue)
     ;; assign val val
     (goto (reg continue))

     end)))


(define (run-count-leaves-explicit-machine tree)
  (set-register-contents! count-leaves-explicit-machine 'tree tree)
  ;; (count-leaves-explicit-machine 'enable-tracing)
  ;; (((count-leaves-explicit-machine 'get-register) 'tree) 'enable-tracing)
  ;; (((count-leaves-explicit-machine 'get-register) 'val) 'enable-tracing)
  ;; (((count-leaves-explicit-machine 'get-register) 'stash) 'enable-tracing)
  ;; (((count-leaves-explicit-machine 'get-register) 'continue) 'enable-tracing)

  (start count-leaves-explicit-machine)
  (get-register-contents count-leaves-explicit-machine 'val))

(define (count-leaves-explicit tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

(pp (map count-leaves-explicit examples))
(pp (map run-count-leaves-explicit-machine examples))
