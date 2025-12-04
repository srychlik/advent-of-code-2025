(defparameter *min-dial* 0)
(defparameter *max-dial* 99)

(defun rotate-dial (start direction distance)
  "Rotate the dial of the lock <direction L or R> <distance spaces> starting at <start>. Return the new value"
  (cond ((equal direction "R") (mod (+ start distance) (+ 1 *max-dial*)))
	((equal direction "L") (mod (- start distance) (+ 1 *max-dial*)))
	(t (error "Invalid direction"))
	)
  )
  
(assert (equal (rotate-dial 11 "R" 8) 19))
(assert (equal (rotate-dial 19 "L" 19) 0))

(assert (equal (rotate-dial 5 "L" 10) 95))
(assert (equal (rotate-dial 95 "R" 5) 0))

(defparameter *start-dial* 50)

(defun parse-rotation (string)
  ;; Not happy that this is a list, but multiple value bind doesn't work in a loop easily
  ;; and I don't care to make it fancier. A list of 2 values is fine for direction and distance
  (list (subseq string 0 1) (parse-integer (subseq string 1))))
    
(defun get-password (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  for firstp = t then nil
	  for instruction-parts = (parse-rotation line)
	  for dial = (if firstp (rotate-dial *start-dial* (first instruction-parts) (second instruction-parts)) (rotate-dial dial (first instruction-parts) (second instruction-parts)))
	  count (= 0 dial))
    ))
