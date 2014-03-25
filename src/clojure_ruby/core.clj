(ns clojure-ruby.core
  (:require [instaparse.core :as insta]))

(def ruby-parser
  (insta/parser (clojure.java.io/resource "ruby.ebnf")))

(def variables (atom {}))

(defmulti evaluate first)

(defmethod evaluate :method_call_infix [stmt]
  (let [[_ obj method arg] stmt
        arg (evaluate arg)]
    (assert (= :var_ref (first obj)))
    (case method
      "=" (swap! variables assoc (second obj) arg))))

(defmethod evaluate :literal [stmt]
  (let [[_ val] stmt
        [type val] val]
    (case type
      :number (Long. val))))
