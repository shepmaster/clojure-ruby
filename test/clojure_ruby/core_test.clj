(ns clojure-ruby.core-test
  (:require [clojure.test :refer :all]
            [clojure-ruby.core :refer :all]
            [instaparse.core :as insta]))

(defn unambigous? [code]
  (= 1 (count (insta/parses ruby-parser code))))

(deftest assignment
  (is (unambigous? "a = 1"))
  (is (= (ruby-parser "a = 1")
         [[:assignment
           "a"
           [:number "1"]]]))
  (is (unambigous? "a = b"))
  (is (= (ruby-parser "a = b")
         [[:assignment
           "a"
           [:var_ref "b"]]])))

(deftest assignment-and-method-call
  (is (unambigous? "a = o.m()"))
  (is (= (ruby-parser "a = o.m()")
         [[:assignment
           "a"
           [:method_call
            [:var_ref "o"]
            "m"]]])))

(deftest method-calls
  (is (unambigous? "alpha.beta()"))
  (is (= (ruby-parser "alpha.beta()")
         [[:method_call
           [:var_ref "alpha"]
           "beta"]]))
  (is (unambigous? "alpha.beta(1)"))
  (is (= (ruby-parser "alpha.beta(1)")
         [[:method_call
           [:var_ref "alpha"]
           "beta"
           [:number "1"]]]))
  (is (unambigous? "alpha.beta(1, 2)"))
  (is (= (ruby-parser "alpha.beta(1, 2)")
         [[:method_call
           [:var_ref "alpha"]
           "beta"
           [:number "1"]
           [:number "2"]]]))
  (is (unambigous? "alpha[1]"))
  (is (= (ruby-parser "alpha[1]")
         [[:method_call_bracket
           [:var_ref "alpha"]
           [:number "1"]]]))
  (is (unambigous? "alpha[1] = 2"))
  (is (= (ruby-parser "alpha[1] = 2")
         [[:method_call_bracket_assignment
           [:var_ref "alpha"]
           [:number "1"]
           [:number "2"]]]))
  (is (unambigous? "alpha < 4"))
  (is (= (ruby-parser "alpha < 4")
         [[:method_call_infix
           [:var_ref "alpha"]
           "<"
           [:number "4"]]])))

(deftest infix-and-naked
  (is (unambigous? "alpha.beta() < 4"))
  (is (= (ruby-parser "alpha.beta() < 4")
         [[:method_call_infix
           [:method_call
            [:var_ref "alpha"]
            "beta"]
           "<"
           [:number "4"]]]))
  (is (unambigous? "1 == gamma.delta()"))
  (is (= (ruby-parser "1 == gamma.delta()")
         [[:method_call_infix
           [:number "1"]
           "=="
           [:method_call
            [:var_ref "gamma"]
            "delta"]]]))
  (is (unambigous? "a.b() && x.y()"))
  (is (= (ruby-parser "a.b() && x.y()")
         [[:method_call_infix
           [:method_call
            [:var_ref "a"]
            "b"]
           "&&"
           [:method_call
            [:var_ref "x"]
            "y"]]]))
  (is (unambigous? "a.b(1) && x.y(1)"))
  (is (= (ruby-parser "a.b(1) && x.y(1)")
         [[:method_call_infix
           [:method_call
            [:var_ref "a"]
            "b"
            [:number "1"]]
           "&&"
           [:method_call
            [:var_ref "x"]
            "y"
            [:number "1"]]]])))
