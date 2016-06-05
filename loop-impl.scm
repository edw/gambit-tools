(define (bindings->pairs bindings)
  (let loop ((bs bindings) (pairs '()))
    (cond ((null? bs)
           (reverse pairs))
          ((null? (cdr bs))
           (error "Odd number of values in bindings"))
          (else
           (loop (cddr bs) (cons (list (car bs) (cadr bs)) pairs))))))

(define (for-impl bindings form iter in out)
  (cond ((null? bindings)
         `(,iter (cursor-next! ,in) (cons ,form ,out)))
        (else
         (let ((key (caar bindings))
               (val (cadar bindings))
               (vals (cdar bindings))
               (rest (cdr bindings)))
           (cond ((eq? key while:)
                  `(if ,val
                       ,(for-impl rest form iter in out)
                       (reverse ,out)))
                 ((eq? key when:)
                  `(if ,val
                       ,(for-impl rest form iter in out)
                       (,iter (cursor-next! ,in) ,out)))
                 ((eq? key let:)
                  `(let ,val
                     ,(for-impl rest form iter in out)))
                 ((eq? key let*:)
                  `(let* ,val
                     ,(for-impl rest form iter in out)))
                 (else
                  (let ((new-iter (gensym 'for-iter))
                        (new-in (gensym 'for-in))
                        (new-out (gensym 'for-out)))
                    `(let ,new-iter ((,new-in (cursor ,@vals))
                                     (,new-out ,out))
                          (if (cursor-null? ,new-in)
                              (,iter (cursor-next! ,in) ,new-out)
                              (let ((,key (cursor-value ,new-in)))
                                ,(for-impl rest form
                                           new-iter new-in new-out)))))))))))

(define (loop-impl bindings forms iter in)
  (cond ((null? bindings)
         `(begin
            ,@forms
            (,iter (cursor-next! ,in))))
        (else
         (let ((key (caar bindings))
               (val (cadar bindings))
               (vals (cdar bindings))
               (rest (cdr bindings)))
           (cond
            ((eq? key while:)
             `(if ,val
                  ,(loop-impl rest forms iter in)))
            ((eq? key when:)
             `(if ,val
                  ,(loop-impl rest forms iter in)
                  (,iter (cursor-next! ,in))))
            ((eq? key let:)
             `(let ,val
                ,(loop-impl rest forms iter in)))
            ((eq? key let*:)
             `(let* ,val
                ,(loop-impl rest forms iter in)))
            (else
             (let ((new-iter (gensym 'for-iter))
                   (new-in (gensym 'for-in)))
               `(let ,new-iter ((,new-in (cursor ,@vals)))
                     (if (cursor-null? ,new-in)
                         (,iter (cursor-next! ,in))
                         (let ((,key (cursor-value ,new-in)))
                           ,(loop-impl rest forms new-iter new-in)))))))))))
