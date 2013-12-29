#lang r5rs

; states of an NFA are represented as:
; ('nfa-state (list of e-transition) (associative list of transitions) final num)
; the associate list is a list of pairs (char (list of destinations))
; an NFA is a ('nfa initial_state (final_states))

(define (nfa? nfa)
  (and (pair? nfa) (eq? (car nfa) 'nfa)))

(define (nfa-state? nfa)
  (and (pair? nfa) (eq? (car nfa) 'nfa-state)))

(define (nfa-etrans n)
  (cadr n))

(define (nfa-trans n)
  (caddr n))

(define (nfa-final? n)
  (cadddr n))

(define (nfa-trans-to n c)
  (assq c (nfa-trans n)))

(define (nfa-set-etrans n trans)
  (set-car! (cdr n) trans))

(define (nfa-set-trans n trans)
  (set-car! (cddr n) trans))

(define (nfa-set-final n final?)
  (set-car! (cdddr n) final?))

(define (nfa-add-etrans n dst)
  (set-car! (cdr n) (cons dst (cadr n))))

(define (make-nfa-st)
  (list 'nfa-state (list) (list) #f 'na))

(define (nfa-initial nfa)
  (cadr nfa))

(define (nfa-finals nfa)
  (caddr nfa))

(define (nfa-set-num n num)
  (set-car! (cddddr n) num))

(define (nfa-num n)
  (car (cddddr n)))

(define (to-dot nfa)
  (let ((cur 0))
    (display "digraph automata {\n")
    (display "\trankdir = LR;\n")
    (display "\tsize = \"4,4\";\n")
    (display "\tnode [shape=box]; 0;\n")
    (display "\tnode [shape=circle];\n")
    (letrec ((iter
               (lambda (nfa)
                 (if (eq? (nfa-num nfa) 'na)
                   (let ((num cur))
                     (nfa-set-num nfa cur)
                     (set! cur (+ cur 1))
                     (map
                     (lambda (s)
                       (let ((n (iter s)))
                         (display "\t")
                         (display num)
                         (display " -> ")
                         (display n)
                         (display " [label=\"e\"]\n")))
                     (nfa-etrans nfa))
                     (map
                       (lambda (p)
                         (map
                           (lambda (s)
                           (let ((n (iter s)))
                             (display "\t")
                             (display num)
                             (display " -> ")
                             (display n)
                             (display " [label=\"")
                             (display (car p))
                             (display "\"]\n")))
                           (cadr p)))
                       (nfa-trans nfa))
                     (nfa-num nfa))
                   (nfa-num nfa)))))
      (iter (nfa-initial nfa)))
    (display "\tnode [shape=doublecircle];")
    (map
      (lambda (s)
        (display " ") (display (nfa-num s)))
      (nfa-finals nfa))
    (display ";\n")
    (display "}\n")))

(define (regex-to-nfa reg)
  (if (pair? reg)
    (let ((op (car reg))
          (args (cdr reg)))
      (case op
        (* (let ((sub (regex-to-nfa (car args)))
                    (new-init (make-nfa-st))
                    (new-final (make-nfa-st)))

                ; the new initial state transitions to the old initial state
                ; and to the new final state, to short-circuit the expression
                (nfa-set-etrans new-init (list (nfa-initial sub)
                                               new-final))

                ; the old final state is not final anymore and transitions to
                ; the new final state and the old initial state to allow repeat
                (let ((old-final (car (nfa-finals sub))))
                  (nfa-set-etrans old-final (list new-final (nfa-initial sub)))
                  (nfa-set-final old-final #f))

                ; the new states are the only initial and final states
                (nfa-set-final new-final #t)
                (list 'nfa new-init (list new-final))))

        (or (let ((subl (regex-to-nfa (car args)))
                     (subr (regex-to-nfa (cadr args)))
                     (new-init (make-nfa-st))
                     (new-final (make-nfa-st)))

                 ; the new initial transitions to both initial states of the sub
                 ; automatas
                 (nfa-set-etrans new-init (list (nfa-initial subl)
                                                (nfa-initial subr)))

                 ; the old final states transition to the new final state
                 (let ((old-final-l (car (nfa-finals subl)))
                       (old-final-r (car (nfa-finals subr))))
                   (nfa-set-etrans old-final-l (list new-final))
                   (nfa-set-etrans old-final-r (list new-final))
                   (nfa-set-final old-final-l #f)
                   (nfa-set-final old-final-r #f))

                 (nfa-set-final new-final #t)
                 (list 'nfa new-init (list new-final))))

        (else ; it's a regex, so a concatenation
          (letrec ((concat
                     (lambda (reg)
                       (cond
                         ((null? reg) reg) ; concat of nothing, should fail?
                         ((null? (cdr reg)) (regex-to-nfa (car reg)))
                         (else
                           (let ((subl (regex-to-nfa (car reg)))
                                 (subr (concat (cdr reg))))

                             ; concatenation of 2 automata. we remove the
                             ; initial state of the second automata and
                                   ; give its transitions to the final state of
                                   ; the first automate
                                   (let ((old-final (car (nfa-finals subl)))
                                         (new-final (car (nfa-finals subr)))
                                         (old-init (nfa-initial subr)))
                                     (nfa-set-trans old-final (nfa-trans old-init))
                                     (nfa-set-etrans old-final (nfa-etrans old-init))
                                     (nfa-set-final old-final #f)

                                     (list 'nfa (nfa-initial subl) (list new-final)))))))))
            (concat reg)))))

    ; it's just a char
    (let* ((init (make-nfa-st))
           (final (make-nfa-st))
           (trans (list (list reg (list final)))))
      (nfa-set-trans init trans)
      (nfa-set-final final #t)
      (list 'nfa init (list final)))))
