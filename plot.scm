(define (plot-function)
  (define width 60)
  (define height 20)

  (define (to-screen-x x x-min x-max)
    (inexact->exact
     (round (* (/ (- x x-min) (- x-max x-min)) (- width 1)))))

  (define (to-screen-y y y-min y-max)
    (inexact->exact
     (round (* (/ (- y-max y) (- y-max y-min)) (- height 1)))))

  (define (prompt-number msg)
    (display msg)
    (flush-output)
    (read))

  (define (plot-loop)
    (display "\nEnter function (e.g. (lambda (x) (* x x))) or 'quit' to exit: ")
    (flush-output)
    (let ((input (read)))
      (when (not (eq? input 'quit))
        (let* ((x-min (prompt-number "Enter x-min: "))
               (x-max (prompt-number "Enter x-max: "))
               (y-min (prompt-number "Enter y-min: "))
               (y-max (prompt-number "Enter y-max: "))
               (func (eval input)))

          (let loop-y ((y 0))
            (when (< y height)
              (let loop-x ((x 0))
                (when (< x width)
                  (let* ((real-x (+ x-min (* (/ x width) (- x-max x-min))))
                         (real-y (func real-x))
                         (screen-y (to-screen-y real-y y-min y-max)))
                    (display
                     (cond
                       ((= x (to-screen-x 0 x-min x-max)) "|")
                       ((= y (to-screen-y 0 y-min y-max)) "-")
                       ((= y screen-y) "*")
                       (else " "))))
                  (loop-x (+ x 1))))
              (newline)
              (loop-y (+ y 1))))
          (plot-loop)))))

  (plot-loop))

(plot-function)
