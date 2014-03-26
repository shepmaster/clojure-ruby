(ns clojure-ruby.parser-test
  (:require [clojure.test :refer :all]
            [clojure-ruby.parser :refer :all]
            [instaparse.core :as insta]))

(defn unambigous? [code]
  (= 1 (count (insta/parses ruby-parser code))))

(deftest extra-space
  (is (unambigous? "a"))
  (is (unambigous? " a"))
  (is (unambigous? "a "))
  (is (unambigous? " a "))
  (is (unambigous? "\n\t a\n\t "))
  (is (unambigous? " \t\na \t\n "))
  (is (= (ruby-parser "a")
         (ruby-parser " a")
         (ruby-parser "a ")
         (ruby-parser " a ")
         (ruby-parser "\n\t a\n\t ")
         (ruby-parser " \t\na \t\n ")
         [[:var_ref "a"]])))

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

(deftest mutating-assignment
  (is (unambigous?    "a += 1"))
  (is (= (ruby-parser "a += 1")
         [[:assignment_mutate "a" "+=" [:number "1"]]]))
  (is (unambigous?    "a -= 1"))
  (is (= (ruby-parser "a -= 1")
         [[:assignment_mutate "a" "-=" [:number "1"]]])))

(deftest assignment-and-method-call
  (is (unambigous? "a = o.m"))
  (is (= (ruby-parser "a = o.m")
         [[:assignment
           "a"
           [:method_call
            [:var_ref "o"]
            "m"]]])))

(deftest method-calls
  (is (unambigous? "alpha.beta"))
  (is (unambigous? "alpha.beta()"))
  (is (= (ruby-parser "alpha.beta")
         (ruby-parser "alpha.beta()")
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
  (is (unambigous?    "alpha[1] += 2"))
  (is (= (ruby-parser "alpha[1] += 2")
         [[:method_call_bracket_assignment_mutate
           [:var_ref "alpha"]
           [:number "1"]
           "+="
           [:number "2"]]]))
  (is (unambigous? "alpha < 4"))
  (is (= (ruby-parser "alpha < 4")
         [[:method_call_relop
           [:var_ref "alpha"]
           "<"
           [:number "4"]]])))

(deftest relational-operator-has-higher-precedence-than-logic-operator
  (is (unambigous?    "a < b && x == y"))
  (is (= (ruby-parser "a < b && x == y")
         [[:method_call_logic
           [:method_call_relop [:var_ref "a"] "<" [:var_ref "b"]]
           "&&"
           [:method_call_relop [:var_ref "x"] "==" [:var_ref "y"]]]])))


(deftest method-call-has-higher-precedence-than-relational-operator
  (is (unambigous? "alpha.beta < 4"))
  (is (= (ruby-parser "alpha.beta < 4")
         [[:method_call_relop
           [:method_call
            [:var_ref "alpha"]
            "beta"]
           "<"
           [:number "4"]]]))
  (is (unambigous? "1 == gamma.delta"))
  (is (= (ruby-parser "1 == gamma.delta")
         [[:method_call_relop
           [:number "1"]
           "=="
           [:method_call
            [:var_ref "gamma"]
            "delta"]]])))

(deftest flow-if
  (is (unambigous?    "if 1; o.m; 10; end"))
  (is (= (ruby-parser "if 1; o.m; 10; end")
         [[:if
           [:if_branch [:number "1"]
            [:method_call [:var_ref "o"] "m"]
            [:number "10"]]]]))
  (is (unambigous?    "if 1; 2; elsif 3; 4; end"))
  (is (= (ruby-parser "if 1; 2; elsif 3; 4; end")
         [[:if
           [:if_branch [:number "1"]
            [:number "2"]]
           [:if_branch [:number "3"]
            [:number "4"]]]])))

(deftest flow-while
  (is (unambigous? "while 1; o.m; 10; end"))
  (is (= (ruby-parser "while 1; o.m; 10; end")
         [[:while [:number "1"]
           [:method_call [:var_ref "o"] "m"]
           [:number "10"]]])))

(deftest flow-until
  (is (unambigous? "until 1; o.m; 10; end"))
  (is (= (ruby-parser "until 1; o.m; 10; end")
         [[:until [:number "1"]
           [:method_call [:var_ref "o"] "m"]
           [:number "10"]]])))

(deftest flow-case
  (is (unambigous? "case 1; when 1; 2; end"))
  (is (= (ruby-parser "case 1; when 1; 2; end")
         [[:case [:number "1"]
           [:when
            [:number "1"]
            [:number "2"]]]])))

(deftest multiple-statements
  (is (unambigous? "a;b"))
  (is (unambigous? "a\nb"))
  (is (unambigous? " a \n b "))
  (is (= (ruby-parser "a;b")
         (ruby-parser "a\nb")
         (ruby-parser "a \n b")
         [[:var_ref "a"]
          [:var_ref "b"]])))

(deftest rewrite-mutating-assignment
  (is (= (rewrite-assignment-mutate :var-name "+=" :arg)
         [:assignment :var-name [:method_call [:var_ref :var-name] "+" :arg]])))

(deftest rewrite-mutating-bracket-assignment
  (is (= (rewrite-bracket-assignment-mutate :obj :idx "-=" :val)
         [:method_call :obj "[]=" :idx
          [:method_call
           [:method_call :obj "[]" :idx]
           "-"
           :val]])))