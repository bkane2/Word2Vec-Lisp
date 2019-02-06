;; Word2Vec LISP Implementation
;; ----------------------------
;; Ben Kane
;; bkane2@u.rochester.edu

(ql:quickload "split-sequence")
(load "matrix.lisp")

;; Tokenize string to list of symbols
(defun tokenize (str)
  (mapcar (lambda (x) (intern (string-downcase x))) (split-sequence:SPLIT-SEQUENCE #\Space str)))

;; Creates vocabulary of unique symbols from list of symbols
;; NOTE: in future, can be modified to pick out most common words if we want a
;; vocabulary size smaller than the number of unique words in a sentence/corpus.
(defun create-vocabulary (tokens)
  (remove-duplicates tokens))

;; Creates one-hot encoding in the form of a hash map from words to one-hot vectors
;; (prepended by vocabulary length)
(defun create-one-hot (tokens)
  (let*
    ((vocabulary (create-vocabulary tokens))
    (v (length vocabulary))
    (encoding (make-hash-table :test #'equalp)))
  (labels ((one-hot-rec (words i)
    (if (null words) nil
    (let ((li (make-list v :initial-element 0))) (setf (nth i li) 1)
      (setf (gethash (car words) encoding) li)
      ;; (setf (gethash li encoding) (car words))
      (one-hot-rec (cdr words) (+ i 1))))))
  (one-hot-rec vocabulary 0)
  (list v encoding))))

;; Converts input to training data, given an encoding and skip-gram window
(defun get-training (tokens encoding window-dim)
  (let* ((len (length tokens)))
    (loop for i from 0 to (- len 1)
      collect (list (mat-t (list (gethash (nth i tokens) encoding)))
              (remove nil (loop for j from (- i window-dim) to (+ i window-dim)
              collect (if (or (< j 0) (>= j len) (= j i)) nil (mat-t (list (gethash (nth j tokens) encoding))))))))))

;; Initializes random model
(defun init-model (vocab-dim embedding-dim)
  (let
  ((w1 (mat-init-random vocab-dim embedding-dim))
  (w2 (mat-init-random embedding-dim vocab-dim)))
  (list w1 w2)))

;; Forward pass through network
(defun forward-pass (model x)
  (let*
    ((w1 (first model))
    (w2 (second model))
    (h (mat-dot (mat-t w1) x))
    (u (mat-dot (mat-t w2) h))
    (yp (softmax u)))
  (list h u yp)))

;; Backprop and update weights
(defun backprop (model lr e h x)
  (let*
    ((w1 (first model))
    (w2 (second model))
    (dl_dw2 (mat-outer h e))
    (dl_dw1 (mat-outer x (mat-dot w2 (mat-t e)))))
  (setf w1 (mat-pairwise #'- w1 (mat-unary (lambda (x) (* lr x)) dl_dw1)))
  (setf w2 (mat-pairwise #'- w2 (mat-unary (lambda (x) (* lr x)) dl_dw2)))
  (list w1 w2)))


;; Trains the word2vec embedding
(defun train (n-iterations vocab-dim embedding-dim window-dim learn-rate training-data)
  (let* ((loss 0) (model (init-model vocab-dim embedding-dim)))
    ;; Loop through each iteration
    (loop
      for i from 0 to n-iterations
      do (progn
          ;; Reset loss
          (setf loss 0)
          ;; Loop for each data point in the training set (i.e. a word and its context)
          (mapcar (lambda (x)
            (let* ((xn (first x)) (yn (second x))
              ;; Complete forward pass and get updated parameters
              (fp (forward-pass model xn))
              (h (first fp)) (u (second fp)) (yp (third fp))
              ;; Calculate error
              (err (mat-sum (flatten (mapcar (lambda (yc) (mat-pairwise #'- (mat-t yp) (mat-t yc))) yn)) 0)))
            ;; Backprop error and update model
            (setf model (backprop model learn-rate err h xn))
            ;; Update loss
            (setf loss (+
                loss
                (- 0 (reduce #'+ (mapcar (lambda (x) (car (nth (position 1 (flatten x)) u))) yn)))
                (* (length yn) (log (reduce #'+ (first (mat-unary (lambda (x) (exp x)) (mat-t u))))))))))
          training-data)
          ;; Print loss when iteration is done
          (format t "Iteration: ~a, Loss: ~a~%" i loss)))
    model))

;; Train word2vec model, return trained model as well as word encoding hash table
(defun word2vec (input n-iterations embedding-dim window-dim learn-rate)
  (let*
    ((tokens (tokenize input))
    (one-hot (create-one-hot tokens)) (vocab-dim (first one-hot)) (encoding (second one-hot))
    (training-data (get-training tokens encoding window-dim))
    (model (train n-iterations vocab-dim embedding-dim window-dim learn-rate training-data)))
  (list encoding model)))

;; Get vector representation of given word
(defun get-vec (word w2v)
  (let ((encoding (gethash (intern (string-downcase word)) (first w2v))))
    (if encoding (list (nth (position 1 encoding) (first (second w2v)))) (error "word does not exist in encoding"))))

;; Get n most similar words to given vector
(defun vec-sim (vec n w2v)
  (subseq (sort (mapcar (lambda (w)
    (let*
      ((v (get-vec w w2v))
      (cos-num (car (car (mat-dot vec (mat-t v)))))
      (cos-den (* (vec-norm vec) (vec-norm v)))
      (cos-dist (/ (* cos-num 1.0) cos-den)))
    (list w cos-dist)))
  (loop for key being the hash-keys of (first w2v) collect key)) #'>= :key #'second) 0 n))

;; Get n most similar words to given word
(defun word-sim (word n w2v)
  (vec-sim (get-vec word w2v) n w2v))




;; (----------------- EXAMPLE USEAGE ----------------------------)

(setq example "babies do not start intellectually as tabulae rasae they rapidly build abstract knowledge and
concepts and learn language with relatively little input at the symbolic ie linguistic level to build
machines with such abilities we need to equip them with the symbolic machinery that can represent the kinds
of abstract knowledge to be learned ways of using that knowledge for inference of various sorts and ways
of deriving useful abstract patterns of behavior events and relationships from linguistic input")
(setq embed-size 10)
(setq window-size 2)
(setq lr 0.01)
(setq num-iterations 50)

(setq w2v (word2vec example num-iterations embed-size window-size lr))

(format t "~a~%" (word-sim "tabulae" 3 w2v))




