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

(defn rewrite-bracket-assignment-mutate [obj idx meth val]
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

(defn rename-bracket-assignment [obj idx val]
  [:method-call obj "[]=" idx val])

(def cleaning-map
  {:assignment-mutate rewrite-assignment-mutate
   :method-call-bracket-assignment-mutate rewrite-bracket-assignment-mutate
   :method-call-logic rename-infix
   :method-call-relop rename-infix
   :method-call-bracket rename-bracket
   :method-call-bracket-assignment rename-bracket-assignment})

(defn clean-parse-tree [tree]
  (insta/transform cleaning-map tree))