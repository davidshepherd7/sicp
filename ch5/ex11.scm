;; Modified version of stacks for one stack per register.

(define (make-stack)
  (let ((s '()))

    (define (push x reg)
      (let ((reg-stack (assoc reg s)))
        (if (not reg-stack)
            (set! s (cons (list reg x) s)) ; make a new stack for this register
            (set-cdr! reg-stack (cons x (cdr reg-stack)))))) ; prepend to the stack

    (define (pop reg)
      ;; (pp s)
      (let ((reg-stack (assoc reg s)))
        (if (or (null? reg-stack) (null? (cdr reg-stack)))
            (error "Empty stack -- POP")
            (let ((top (cadr reg-stack)))
              (set-cdr! reg-stack (cddr reg-stack))
              top))))

    (define (initialize)
      (set! s '())
      'done)

    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack reg)
  ((stack 'pop) reg))

(define (push stack value reg)
  ((stack 'push) value reg))

(let ((stack (make-stack)))
  (push stack 1 'a)
  (push stack 2 'b)
  (push stack 3 'b)

  ;; Should print 1 3 2
  (pp "")
  (pp (pop stack 'a))

  ;; ;; Should error:
  ;; (pop stack 'a)

  (pp (pop stack 'b))
  (pp (pop stack 'b))

  )
