Word2Vec prototype for Lisp

Requires [Allegro Common Lisp](https://franz.com/products/allegro-common-lisp/) and [Quicklisp](https://www.quicklisp.org/beta/)


Example useage:
```lisp
;; For best results, preproccess input to remove punctuation and special characters (and optionally, stopwords)
(setq example "babies do not start intellectually as tabulae rasae they rapidly build abstract knowledge and
concepts and learn language with relatively little input at the symbolic ie linguistic level to build
machines with such abilities we need to equip them with the symbolic machinery that can represent the kinds
of abstract knowledge to be learned ways of using that knowledge for inference of various sorts and ways
of deriving useful abstract patterns of behavior events and relationships from linguistic input")

(setq embed-size 10) ;; Size of embedding
(setq window-size 2) ;; Size of skip-gram window
(setq lr 0.01) ;; Learning rate
(setq num-iterations 50) ;; Number of iterations

;; Create and train word2vec model
;; Returns pair (encoding, model), where encoding is a hash table mapping words to one-hot encodings,
;; and model is the pair of weights in the trained model, (w1, w2).
(setq w2v (word2vec example num-iterations embed-size window-size lr))

;; Get 5 most similar words to "tabulae"
(word-sim "tabulae" 5 w2v)
```
