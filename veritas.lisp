; Floyd+Pascals's triangles
; (C) 2020 Toni Helminen <GPLv3>

; Floyds triangle
(defun floyds-triangle (depth)
  (setq counter 1)
  (loop for a from 0 to depth do
    (progn
      (loop for b from 0 to a do
        (progn
          (format t "|~D" counter)
          (setq counter (+ counter 1))))
      (format t "|~%"))))

; Pascal's triangle
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

(defun pascals-triangle-program ()
  (format t "Enter Pascal depth? ")
  (setq depth (read))
  (pascals-triangle depth))

(defun floyds-triangle-program ()
  (format t "Enter Floyds's Triangle depth? ")
  (setq depth (read))
  (floyds-triangle depth))

(defun main ()
  (format t "Salve Omnetes!~%")
  (format t "1. Floyd's triangle~%2. Pascal's triangle~%> ")
  (case (read)
    (1 (floyds-triangle-program))
    (2 (pascals-triangle-program))))

; In vivo veritas
(main)
