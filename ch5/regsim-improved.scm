;; regsim with extensions from ex12, ex13, ex15

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing-enabled #f))

    (define (print-register-trace name value contents)
      (for-each display (list "Assigning to register: " name " " contents " -> " value))
      (newline))

    (define (set value)
      (if tracing-enabled (print-register-trace name value contents))
      (set! contents value))

    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) set)
            ((eq? message 'enable-tracing) (set! tracing-enabled #t))
            ((eq? message 'disable-tracing) (set! tracing-enabled #f))
            (else
             (error "Unknown request -- REGISTER" message))))

    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))



;; stack

;; monitored version from section 5.2.4
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))



;; Instruction counter

(define (make-inst-counter)
  (let ((n 0))

    (define (increment)
      (set! n (+ n 1)))

    (define (clear)
      (set! n 0))

    (define (print-statistics)
      (newline)
      (display (list 'number-of-instructions '= n)))

    (define (dispatch message)
      (cond ((eq? message 'increment) (increment))
            ((eq? message 'print-statistics) (print-statistics))
            ((eq? message 'clear) (clear))
            (else (error "Unknown request -- INSTRUCTION COUNTER" message))))

    dispatch))



(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-counter (make-inst-counter))
        (labels '())
        (break #f)
        (tracing-enabled #f))

    (let ((the-ops
           (list (list 'initialize-stack (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics (lambda () (stack 'print-statistics)))))
          (register-table (list (list 'pc pc) (list 'flag flag))))

      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)

      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin (allocate-register name)
                     (lookup-register name)))))

      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                (break 'break)
                (else
                 ((instruction-execution-proc (car insts)))
                 (execute)))))

      (define (instruction-list)
        (uniq (sort (map instruction-text the-instruction-sequence) less-than?)))

      (define (entry-point-registers)
        ;; I need threading macros so badly :(
        (map (lambda (exp) (register-exp-reg exp))
             (filter (lambda (exp) (register-exp? exp))
                     (map (lambda (inst) (car (instruction-execution-proc inst)))
                          (filter (lambda (inst) (equal? (instruction-text inst) 'goto))
                                  (instruction-list))))))

      (define (stack-registers)
        (uniq
         ((lambda (l) (sort l less-than?)) ;; swap arg order for consistent data flow
          (map (lambda (inst) (car (instruction-execution-proc inst)))
               (filter (lambda (inst) (or (equal? (instruction-text inst) 'save)
                                     (equal? (instruction-text inst) 'restore)))
                       (instruction-list))))))

      (define (register-sources)
        (group
         (map cdr
              (filter (lambda (inst) (equal? (instruction-text inst) 'assign))
                      (instruction-list)))))

      (define (set-breakpoint label n)
        (splice! (lookup-label labels label) n (make-breakpoint label n)))

      (define (cancel-breakpoint label n)
        (let ((bp-label (lookup-label labels label)))
          (if (not (breakpoint? (nth bp-label n)))
              (error "Can't cancel breakpoint because not a breakpoint" label n))
          (delete-nth! bp-label n)))

      (define (cancel-all-breakpoints)
        ;; Like a shitty mutating filter function
        (define (cab-rec insts)
          (if (null? insts)
              'done
              (begin
                (if (breakpoint? (car insts))
                    (delete-nth! insts 0))
                (cab-rec (cdr insts)))))

        (cab-rec the-instruction-sequence))

      (define (breakpoint? inst)
        (and (not (null? inst)) (eq? (car inst) 'break)))

      (define (make-breakpoint label n)
        (cons 'break
              (lambda ()
                (set! break #t)
                (map display (list "Hit breakpoint at label " label " with offset " n "\n"))
                (map display (list "Next instruction " (instruction-text (caddr (get-contents pc))) "\n"))
                (advance-pc pc))))

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'set-labels) (lambda (new-labels) (set! labels new-labels)))

              ((eq? message 'instruction-list) (instruction-list))
              ((eq? message 'entry-point-registers) (entry-point-registers))
              ((eq? message 'stack-registers) (stack-registers))
              ((eq? message 'register-sources) (register-sources))
              ((eq? message 'instruction-counter) instruction-counter)

              ((eq? message 'enable-tracing) (set! tracing-enabled #t))
              ((eq? message 'disable-tracing) (set! tracing-enabled #f))
              ((eq? message 'tracing-enabled) tracing-enabled)

              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              ((eq? message 'proceed) (set! break #f) (execute))

              (else (error "Unknown request -- MACHINE" message))))

      dispatch)))


(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    ((machine 'set-labels) labels)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (receive (cons (make-print-label next-inst) insts)
                                       (cons (make-label-entry next-inst insts) labels))
                              (receive (cons (make-instruction next-inst) insts)
                                       labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-print-label label)
  (list (cons 'print-label label)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (let ((exec
         (cond ((eq? (car inst) 'assign)
                (make-assign inst machine labels ops pc))
               ((eq? (car inst) 'test)
                (make-test inst machine labels ops flag pc))
               ((eq? (car inst) 'branch)
                (make-branch inst machine labels flag pc))
               ((eq? (car inst) 'goto)
                (make-goto inst machine labels pc))
               ((eq? (car inst) 'save)
                (make-save inst machine stack pc))
               ((eq? (car inst) 'restore)
                (make-restore inst machine stack pc))
               ((eq? (car inst) 'perform)
                (make-perform inst machine labels ops pc))
               ((eq? (car inst) 'print-label)
                (make-print-label-exec inst machine pc))
               (else (error "Unknown instruction type -- ASSEMBLE"
                            inst)))))

    (lambda ()
      ;; Tracing stuff shouldn't include the tracing instructions themselves
      (if (not (eq? (car inst) 'print-label))
          (begin
            ((machine 'instruction-counter) 'increment)
            (if (machine 'tracing-enabled) (pp inst))))
      (exec))))

(define (print-label-trace label)
  (for-each display (list ";" label "\n")))

(define (make-print-label-exec inst machine pc)
  (let ((label (cdr inst)))
    (lambda ()
      (if (machine 'tracing-enabled)
          (print-label-trace label))
      (advance-pc pc))))


(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (begin
                  (if (machine 'tracing-enabled) (print-label-trace (label-exp-label dest)))
                  (set-contents! pc insts))
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))



;; breakpoints

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))



;; from 4.1
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

'(REGISTER SIMULATOR LOADED)



;; helpers

(define (less-than? x y)

  (define (list<? x y)
    (if (not (list? x)) (error "Not a list" x))
    (if (not (list? y)) (error "Not a list" y))
    (cond
     ;; End of one of the lists
     ((and (null? x) (null? y)) #t)
     ((and (null? x) (not (null? y))) #t)
     ((and (not (null? x)) (null? y)) #f)

     ((less-than? (car x) (car y)) #t)
     ((equal? (car x) (car y)) (list<? (cdr x) (cdr y)))
     (#t #f)))

  (cond ((and (symbol? x) (symbol? y)) (symbol<? x y))
        ((and (string? x) (string? y)) (string<? x y))
        ((and (number? x) (number? y)) (< x y))
        ((and (list? x) (list? y)) (list<? x y))
        (#t (error "Unhandled types" x y))))

;; Given a sorted list, return the list with duplicates removed
(define (uniq l)
  (define (drop-if-equal x rest)
    (if (or (null? rest) (not (equal? x (car rest))))
        (cons x rest)
        rest))
  (fold-right drop-if-equal '() l))

;; (pp (uniq '(1 1 2 3 3)))

;; (pp (less-than? '(1 "a") '(1 "b"))) ; t
;; (pp (less-than? '(1 2) '(1 2 3))) ; t
;; (pp (less-than? '(1 2 3) '(1 2))) ; f
;; (pp (less-than? '(2) '(1))) ; f


(define (group lst_)
  (define (group-rec lst out)
    (if (null? lst)
        out
        (let ((x (car lst))
              (rest (cdr lst)))
          (let ((grp (assoc (car x) out)))
            (if grp
                (begin (set-cdr! grp (cons (cdr x) (cdr grp)))
                       (group-rec rest out))
                (group-rec rest (cons (cons (car x) (list (cdr x))) out)))))))

  (group-rec lst_ '()))

;; (pp (group '()))
;; (pp (group '((1 1) (1 2 3) (10 1 1))))

(define (nth-tail lst n)
  (if (eq? n 0)
      lst
      (nth-tail (cdr lst) (- n 1))))

(define (nth lst n)
  (car (nth-tail lst n)))

(define (splice! lst n x)
  (if (eq? n 0)
      (let ((front (car lst)))
        (set-car! lst x)
        (set-cdr! lst (cons front (cdr lst))))
      (let ((splice-point (nth-tail lst (- n 1))))
        (set-cdr! splice-point
                  (cons x (cdr splice-point))))))

;; (map (lambda (i)
;;        (let ((lst (list-copy '("a" "b" "c"))))
;;          (let ((other lst))
;;            (splice! lst i "X")
;;            (pp lst)
;;            (pp other))))
;;      '(0 1 2 3))

(define (delete-nth! lst n)
  (cond
   ;; Tricky case
   ((eq? n 0)
    (set-car! lst (cadr lst))
    (set-cdr! lst (cddr lst)))

   ;; Easy case
   ((eq? n 1) (set-cdr! lst (cddr lst)))

   ;; Reduce nth to easy case
   (else (delete-nth! (nth-tail lst (- n 1)) 1))))


;; (map (lambda (i)
;;        (let ((lst (list-copy '("a" "b" "c" "d"))))
;;          (let ((other lst))
;;            (delete-nth! lst i)
;;            (pp lst)
;;            (pp other))))
;;      '(0 1 2 3))




;; (define fib-machine
;;   (make-machine
;;    (list (list '< <) (list '- -) (list '+ +))
;;    '(start
;;      (assign continue (label fib-done))
;;      fib-loop
;;      (test (op <) (reg n) (const 2))
;;      (branch (label immediate-answer))
;;      ;; set up to compute Fib(n-1)
;;      (save continue)
;;      (assign continue (label afterfib-n-1))
;;      (save n)                           ; save old value of n
;;      (assign n (op -) (reg n) (const 1)); clobber n to n-1
;;      (goto (label fib-loop))            ; perform recursive call
;;      afterfib-n-1                         ; upon return, val contains Fib(n-1)
;;      (restore n)
;;      (restore continue)
;;      ;; set up to compute Fib(n-2)
;;      (assign n (op -) (reg n) (const 2))
;;      (save continue)
;;      (assign continue (label afterfib-n-2))
;;      (save val)                         ; save Fib(n-1)
;;      (goto (label fib-loop))
;;      afterfib-n-2                         ; upon return, val contains Fib(n-2)
;;      (assign n (reg val))               ; n now contains Fib(n-2)
;;      (restore val)                      ; val now contains Fib(n-1)
;;      (restore continue)
;;      (assign val                        ; Fib(n-1)+Fib(n-2)
;;              (op +) (reg val) (reg n))
;;      (goto (reg continue))              ; return to caller, answer is in val
;;      immediate-answer
;;      (assign val (reg n))               ; base case: Fib(n)=n
;;      (goto (reg continue))
;;      fib-done)))

;; (pp (fib-machine 'instruction-list))
;; (pp (fib-machine 'entry-point-registers))
;; (pp (fib-machine 'stack-registers))
;; (pp (fib-machine 'register-sources))

;; ;; Try it out!
;; (define (fib n)
;;   (if (< n 2)
;;       n
;;       (+ (fib (- n 1)) (fib (- n 2)))))

;; (define (run-fib n)
;;   (set-register-contents! fib-machine 'n n)
;;   (start fib-machine)
;;   ((fib-machine 'stack) 'print-statistics)
;;   (get-register-contents fib-machine 'val)
;;   )

;; (define args (list 0 1 2 3 4 5 6 7))
;; (pp (map run-fib args))
;; (pp (map fib args))
