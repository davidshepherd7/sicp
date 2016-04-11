
(display "\n")

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(start
        (assign continue (label fib-done))
     fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        ;; set up to compute Fib(n-1)
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)                           ; save old value of n
        (assign n (op -) (reg n) (const 1)); clobber n to n-1
        (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, val contains Fib(n-1)
        (restore n)
        (restore continue)
        ;; set up to compute Fib(n-2)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (label afterfib-n-2))
        (save val)                         ; save Fib(n-1)
        (goto (label fib-loop))
     afterfib-n-2                         ; upon return, val contains Fib(n-2)
        (assign n (reg val))               ; n now contains Fib(n-2)
        (restore val)                      ; val now contains Fib(n-1)
        (restore continue)
        (assign val                        ; Fib(n-1)+Fib(n-2)
                (op +) (reg val) (reg n))
        (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
        (assign val (reg n))               ; base case: Fib(n)=n
        (goto (reg continue))
     fib-done)))


(define (run-fib n)
  (set-register-contents! fib-machine 'n n)
  (start fib-machine)
  (get-register-contents fib-machine 'val))


(define args (list 0 1 2 3 4 5 6 7))
(pp (map fib args))
(pp (map run-fib args))


(define exp-rec-machine
  (make-machine
   (list 'b 'n 'val 'continue)
   (list (list '- -) (list '= =) (list '* *))
   '(start
        (assign continue (label done))
     exp-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-exp))
        (goto (label exp-loop))
    after-exp
        (restore n)
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
    base-case
        (assign val (const 1))
        (goto (reg continue))
    done)))

(define (run-exp-rec b n)
  (set-register-contents! exp-rec-machine 'n n)
  (set-register-contents! exp-rec-machine 'b b)
  (start exp-rec-machine)
  (get-register-contents exp-rec-machine 'val))


(define (run-exp-rec2 n) (run-exp-rec 2 n))
(define (exp2 n) (expt 2 n))

(pp (map exp2 args))
(pp (map run-exp-rec2 args))



(define exp-iter-machine
  (make-machine
   (list 'b 'product 'counter)
   (list (list '= =) (list '* *) (list '- -))
   '(start
         (assign product (const 1))
     exp-loop
         (test (op =) (reg counter) (const 0))
         (branch (label done))
         (assign counter (op -) (reg counter) (const 1))
         (assign product (op *) (reg b) (reg product))
         (goto (label exp-loop))
     done)))

(define (run-exp-iter b n)
  (set-register-contents! exp-iter-machine 'counter n)
  (set-register-contents! exp-iter-machine 'b b)
  (start exp-iter-machine)
  (get-register-contents exp-iter-machine 'product))


(define (run-exp-iter2 n) (run-exp-iter 2 n))
(define (exp2 n) (expt 2 n))

(pp (map exp2 args))
(pp (map run-exp-iter2 args))
