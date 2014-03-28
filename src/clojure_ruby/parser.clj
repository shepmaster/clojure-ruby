(ns clojure-ruby.parser
  (:require [instaparse.core :as insta]))

(def ruby-parser
  (insta/parser (clojure.java.io/resource "ruby.ebnf")))

(defn mutating-method->real-method [meth-name]
  (subs meth-name 0 1))

(defn rewrite-assignment-mutate [var-name meth val]
  (let [real-meth (mutating-method->real-method meth)]
    [:assignment var-name
     [:method-call
      [:var-ref var-name]
      real-meth
      val]]))

(defn rewrite-assignment-bracket-mutate [obj & idxs-meth-and-val]
  (let [rev-tail (reverse idxs-meth-and-val)
        [val meth & rev-idxs] rev-tail
        idxs (reverse rev-idxs)
        real-meth (mutating-method->real-method meth)]
    `[:method-call ~obj "[]=" ~@idxs
      [:method-call
       [:method-call ~obj "[]" ~@idxs]
       ~real-meth
       ~val]]))

(defn rename-infix [obj meth arg]
  [:method-call obj meth arg])

(defn rename-bracket [obj & idxs]
  `[:method-call ~obj "[]" ~@idxs])

(defn rename-assignment-bracket [obj & idxs-and-val]
  `[:method-call ~obj "[]=" ~@idxs-and-val])

(def cleaning-map
  {:assignment-mutate rewrite-assignment-mutate
   :assignment-bracket rename-assignment-bracket
   :assignment-bracket-mutate rewrite-assignment-bracket-mutate
   :method-call-logic rename-infix
   :method-call-relop rename-infix
   :method-call-bracket rename-bracket})

(defn clean-parse-tree [tree]
  (insta/transform cleaning-map tree))
