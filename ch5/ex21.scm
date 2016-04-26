;; Run as: scheme --load ./regsim-improved.scm --load ex21.scm

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

(pp "")
(pp (map count-leaves examples))
(pp (map run-count-leaves-machine examples))
