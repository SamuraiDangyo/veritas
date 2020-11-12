; Floyd+Pascals's triangles
; (C) 2020 Toni Helminen <GPLv3>

; Print Floyds triangle
(defun floyds-triangle (depth)
  (setq counter 1)
  (loop for x from 0 to depth do
    (progn
      (loop for y from 0 to x do
        (progn
          (format t "|~D" counter)
          (setq counter (+ counter 1))))
      (format t "|~%"))))

; Print Pascal's triangle
(defun pascals-triangle (size)
  (setq myarray (make-array '(100 100))) ; TODO wtf?
  (dotimes (y size) (dotimes (x size) (setf (aref myarray y x) 0)))
  (dotimes (y size)
    (dotimes (x size)
      (setq num 0)
      (if (or (= x 0 ) (= x y)) (setq num 1)) ; Reset
      (if (and (> x 0 ) (< x y) (> y 0)) (setq num (+ (aref myarray (- y 1) (- x 1)) (aref myarray (- y 1) (+ x 0))))) ; Calc
      (setf (aref myarray y x) num)))
  (dotimes (y size) ; Print
    (progn
      (loop for x from 0 to y do
        (format t "|~D" (aref myarray y x))))
        (format t "|~%")))

; Is prime number 
(defun is-prime-number (num)
  (if (< num 2) (return-from is-prime-number NIL))
  (loop for i from 2 to (/ num 2) do
    (progn
      (if (= 0 (mod num i)) (return-from is-prime-number NIL)))) 
  (return-from is-prime-number T))

(defun pascals-triangle-program ()
  (format t "Enter Pascal depth? ")
  (setq depth (read))
  (pascals-triangle depth))

(defun floyds-triangle-program ()
  (format t "Enter Floyds's Triangle depth? ")
  (setq depth (read))
  (floyds-triangle depth))

(defun is-prime-number-program ()
  (format t "Is Prime number? ")
  (setq prime (read))
  (if (is-prime-number prime) (format t "Yes~%") (format t "No~%")))

(defun main ()
  (format t "Salve Omnetes!~%")
  (format t "1. Floyd's triangle~%2. Pascal's triangle~%3. Is prime number~%> ")
  (case (read)
    (1 (floyds-triangle-program))
    (2 (pascals-triangle-program))
    (3 (is-prime-number-program))))

; In vivo veritas
(main)
