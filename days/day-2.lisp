(ql:quickload "split-sequence")

(defun invalid-id-p (id)
  "Invalid IDs consist only of a repeated sequence e.g. 123123"
  (let* ((id-string (write-to-string id))
	(id-length (length id-string)))
    (equal (subseq id-string 0 (floor id-length 2))
	   (subseq id-string (floor id-length 2)))))

(assert (invalid-id-p 55))
(assert (invalid-id-p 6464))
(assert (invalid-id-p 123123))
(assert (not (invalid-id-p 101)))

(defun invalid-in-range (start end)
  (loop for n from start to end
	if (invalid-id-p n) collect n))

(defun sum-invalid-in-range (start end)
  ;; This is probably more efficient than accumulating the list and then reducing
  (loop for n from start to end
	if (invalid-id-p n) sum n))

(assert (equal `(11 22) (invalid-in-range 11 22)))
(assert (equal `(99) (invalid-in-range 95 115)))
(assert (equal `(1010) (invalid-in-range 998 1012)))
(assert (equal `(1188511885) (invalid-in-range 1188511880 1188511890)))
(assert (equal `(222222) (invalid-in-range 222220 222224)))
(assert (equal `() (invalid-in-range 1698522 1698528)))
(assert (equal `(446446) (invalid-in-range 446443 446449)))
(assert (equal `(38593859) (invalid-in-range 38593856 38593862)))

(defun split-into-ranges (input)
  (split-sequence::split-sequence #\, input))

(assert (equal `("12-34" "56-78") (split-into-ranges "12-34,56-78")))

(defun part-1 (filename)
  ;; File isn't that big. Just load it and split it
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      ;; Keeping list of ranges in memory is fine
      (let ((ranges (split-into-ranges contents)))
	(loop for range in ranges
	      for start-end = (split-sequence::split-sequence #\- range)
	      sum (sum-invalid-in-range (parse-integer (first start-end)) (parse-integer (second start-end))))))))

