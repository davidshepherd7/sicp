;; Run as: scheme --load ./regsim-improved.scm --load ex16.scm

;; Also demos ex17

(define factorial-machine
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
   '(start
     (assign continue (label fact-done))     ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n-1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1!=1
     (goto (reg continue))                   ; return to caller
     fact-done)))


(define (run-factorial n)
  (if (< n 1) (error "Min value 1"))
  ((factorial-machine 'stack) 'initialize)
  (set-register-contents! factorial-machine 'n n)
  (factorial-machine 'enable-tracing)

  (start factorial-machine)

  (factorial-machine 'disable-tracing)
  (get-register-contents factorial-machine 'val))

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(define args (list 1 2 3 4 5 6 7 8 9 10 11 12 1))
(pp "")
(pp (map run-factorial args))
(pp (map factorial args))
