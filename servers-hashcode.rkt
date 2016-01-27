#lang racket
(define input (list (list 2 5 1 2 5)
                    (list 0 0)
                    (list 3 10) (list 3 10) (list 2 5) (list 1 5) (list 1 1)))


(define (empty-slot) '*)
(define rows (list))


(define (print-rows)
  (for ([row rows])
       (displayln row)))

(define (pair-set-cdr pair val)
  (cons (car pair) val)
  )

(define (get-unavailable-slots input)
  (let ([config (first input)])
    (let ([num-unavailable-slots (third config)])
      (for/list ([l (drop input 1)]
                 [i (range num-unavailable-slots)])
                l
                ))))

(define (get-servers-description input)
  (let ([config (first input)])
    (let ([num-unavailable-slots (third config)]
          [num-servers (fifth config)])
      (drop input (+ 1 num-unavailable-slots)))))

(define (with-servers-id servers)
  (for/list ([i (range (length servers))]
             [server servers])
            (append (list i) server)
            ))

(define (make-rows num-rows num-slots)
  (for/list ([i (range num-rows)])
            (cons (make-vector num-slots (empty-slot)) num-slots)
            )
  )

(define (mark-unavailable-slots unavailable-slots rows)
  (set! rows (car
              (for/list ([unavailable-slot unavailable-slots])
                        (vector-set! (car (list-ref rows (first unavailable-slot))) (second unavailable-slot) 'x)
                        (list-set
                         rows
                         (first unavailable-slot)
                         (pair-set-cdr (list-ref rows (first unavailable-slot)) (- (cdr (list-ref rows (first unavailable-slot))) 1)))))))

(define (row-capacity row)
  (cdr row))

(define (place-server-aux server row row-n)
  (let ([id (first server)]
        [size (second server)]
        [row-v (car row)])
    ;(displayln size)
    (for ([i (range (vector-length row-v))]
          #:when (eq? (vector-ref row-v i) (empty-slot))
          #:break (eq? size 0)
          )
         (set! size (- size 1))
         (vector-set! row-v i id)
         )
    (set! rows (list-set rows row-n (cons row-v (- (row-capacity row) (second server)))))
    ))


(define (place-server server rows-i row-n)
  ;(displayln rows)
  (let ([size (second server)])
    (when (not (empty? rows-i))
      (if (>= (row-capacity (car rows-i)) size)
          (place-server-aux server (car rows-i) row-n)
          (place-server server (cdr rows-i) (+ row-n 1))
          ))))


#;(define (place-servers servers num-pools)
    (let ([sorted-servers (reverse (sort servers < #:key (lambda (x) (third x))))]
          )
      (displayln sorted-servers)
      ;(set! sorted-servers (sort sorted-servers < #:key (lambda (x) (first x))))
      ;(displayln sorted-servers)
      (for ([server sorted-servers])
           (place-server server rows 0)
           )))

(define start-rows (list))

(define (place-servers servers num-pools)
  (let ([hips (list)]
        [perms (permutations servers)])
    (for ([sorted-servers perms])
         (set! rows start-rows)
         (displayln sorted-servers)
         (displayln start-rows)
         (for ([server sorted-servers])
              (place-server server rows 0))
         (set! hips (append hips (list rows)))
         )
    hips
    ))



(define (eval-hip hip servers)
  (define (eval-row row)
    (let ([inc-servs (filter number? (remove-duplicates (vector->list (car row))))])
      (for/fold ([score 0])
                ([i inc-servs])
                (+ score (third (list-ref servers i)))
    )))
  (let ([rows-scores (map eval-row hip)])
    (remove (max (car rows-scores)) rows-scores)))

(define (eval-hips hips servers)
  (for/list ([hip hips])
       (eval-hip hip servers)))
       

(let ([config (first input)]
      [unavailable-slots (get-unavailable-slots input)]
      [servers (with-servers-id (get-servers-description input))])
  (let ([num-rows (first config)]
        [num-slots (second config)]
        [num-unavailable-slots (third config)]
        [num-pools (fourth config)]
        [num-servers (fifth config)])
    (set! rows (make-rows num-rows num-slots))
    (displayln servers)
    (mark-unavailable-slots unavailable-slots rows)
    (set! start-rows (make-rows num-rows num-slots))
    (mark-unavailable-slots unavailable-slots start-rows)
    (displayln start-rows)    
    (let ([hips (list)]
          [perms (permutations servers)])
      (for ([sorted-servers perms])
           (set! rows (make-rows num-rows num-slots))
           (mark-unavailable-slots unavailable-slots rows)
           (for ([server sorted-servers])
                (place-server server rows 0))
           (set! hips (append hips (list rows)))
           )
      (eval-hips hips servers)
      )
    ))







