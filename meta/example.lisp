;;;; Common Lisp Examples.

(defun hello-world ()
  "Print 'hello, world' message."
  (format t "hello, world~%"))

(defun factorial (n)
  "Compute factorial of n."
  (if (zerop n)
      1
      (* n (factorial (- n 1)))))

(defun fibonacci (n)
  "Compute nth Fibonacci number."
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
