;; Word2Vec
;; Packaged on 2019-02-25

(in-package :cl-user)

(defpackage :word2vec
  (:use :cl :util :cl-strings :inferior-shell :drakma :cl-json)
  (:export word2vec))

