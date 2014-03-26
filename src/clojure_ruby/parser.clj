(ns clojure-ruby.parser
  (:require [instaparse.core :as insta]))

(def ruby-parser
  (insta/parser (clojure.java.io/resource "ruby.ebnf")))

(defn mutating-method->real-method [meth-name]
  (subs meth-name 0 1))

(defn rewrite-assignment-mutate [var-name meth val]
  (let [real-meth (mutating-method->real-method meth)]
    [:assignment var-name
     [:method_call
      [:var_ref var-name]
      real-meth
      val]]))

(defn rewrite-bracket-assignment-mutate [obj idx meth val]
  (let [real-meth (mutating-method->real-method meth)]
    [:method_call obj "[]=" idx
     [:method_call
      [:method_call obj "[]" idx]
      real-meth
      val]]))

(defn rename-infix [obj meth arg]
  [:method_call obj meth arg])

(defn rename-bracket [obj idx]
  [:method_call obj "[]" idx])

(defn rename-bracket-assignment [obj idx val]
  [:method_call obj "[]=" idx val])

(def cleaning-map
  {:assignment_mutate rewrite-assignment-mutate
   :method_call_bracket_assignment_mutate rewrite-bracket-assignment-mutate
   :method_call_logic rename-infix
   :method_call_relop rename-infix
   :method_call_bracket rename-bracket
   :method_call_bracket_assignment rename-bracket-assignment})

(defn clean-parse-tree [tree]
  (insta/transform cleaning-map tree))
