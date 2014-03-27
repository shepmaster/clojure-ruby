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

(defn rewrite-assignment-bracket-mutate [obj idx meth val]
  (let [real-meth (mutating-method->real-method meth)]
    [:method-call obj "[]=" idx
     [:method-call
      [:method-call obj "[]" idx]
      real-meth
      val]]))

(defn rename-infix [obj meth arg]
  [:method-call obj meth arg])

(defn rename-bracket [obj idx]
  [:method-call obj "[]" idx])

(defn rename-assignment-bracket [obj idx val]
  [:method-call obj "[]=" idx val])

(def cleaning-map
  {:assignment-mutate rewrite-assignment-mutate
   :assignment-bracket rename-assignment-bracket
   :assignment-bracket-mutate rewrite-assignment-bracket-mutate
   :method-call-logic rename-infix
   :method-call-relop rename-infix
   :method-call-bracket rename-bracket})

(defn clean-parse-tree [tree]
  (insta/transform cleaning-map tree))
