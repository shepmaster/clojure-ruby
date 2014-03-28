(ns clojure-ruby.parser-test
  (:require [clojure.test :refer :all]
            [clojure-ruby.parser :refer :all]
            [instaparse.core :as insta]))

(defn unambigous? [code]
  (= 1 (count (insta/parses ruby-parser code))))

(deftest the-vast-emptiness
  (is (unambigous? ""))
  (is (unambigous? " "))
  (is (unambigous? "\t"))
  (is (unambigous? "\n"))
  (is (unambigous? " \t\n"))
  (is (unambigous? "\n\n"))
  (is (unambigous? ";;;"))
  (is (=  (ruby-parser "")
          (ruby-parser " ")
          (ruby-parser "\t")
          (ruby-parser "\n")
          (ruby-parser " \t\n")
          (ruby-parser "\n\n")
          (ruby-parser ";;;")
          [])))

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
         [[:var-ref "a"]])))

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
           [:var-ref "b"]]]))
  (is (unambigous? "alpha[1] = 2"))
  (is (= (ruby-parser "alpha[1] = 2")
         [[:assignment-bracket
           [:var-ref "alpha"]
           [:number "1"]
           [:number "2"]]])))

(deftest chained-assignment
  (is (unambigous?    "a = b = 1"))
  (is (= (ruby-parser "a = b = 1")
         [[:assignment
           "a"
           [:assignment
            "b"
            [:number "1"]]]])))

(deftest mutating-assignment
  (is (unambigous?    "a += 1"))
  (is (= (ruby-parser "a += 1")
         [[:assignment-mutate "a" "+=" [:number "1"]]]))
  (is (unambigous?    "a -= 1"))
  (is (= (ruby-parser "a -= 1")
         [[:assignment-mutate "a" "-=" [:number "1"]]]))
  (is (unambigous?    "alpha[1] += 2"))
  (is (= (ruby-parser "alpha[1] += 2")
         [[:assignment-bracket-mutate
           [:var-ref "alpha"]
           [:number "1"]
           "+="
           [:number "2"]]])))

(deftest chained-mutating-assignment
  (is (unambigous?    "a += b -= 1"))
  (is (= (ruby-parser "a += b -= 1")
         [[:assignment-mutate
           "a"
           "+="
           [:assignment-mutate
            "b"
            "-="
            [:number "1"]]]])))

(deftest method-calls-without-parens
  (is (unambigous?    "alpha.beta 1"))
  (is (= (ruby-parser "alpha.beta 1")
         [[:method-call-no-parens
           [:var-ref "alpha"]
           "beta"
           [:number "1"]]]))
  (is (unambigous?    "alpha.beta 1, 2"))
  (is (= (ruby-parser "alpha.beta 1, 2")
         [[:method-call-no-parens
           [:var-ref "alpha"]
           "beta"
           [:number "1"]
           [:number "2"]]])))

(deftest method-calls
  (is (unambigous?    "alpha.beta"))
  (is (unambigous?    "alpha.beta()"))
  (is (= (ruby-parser "alpha.beta")
         (ruby-parser "alpha.beta()")
         [[:method-call
           [:var-ref "alpha"]
           "beta"]]))
  (is (unambigous?    "alpha.beta(1)"))
  (is (= (ruby-parser "alpha.beta(1)")
         [[:method-call
           [:var-ref "alpha"]
           "beta"
           [:number "1"]]]))
  (is (unambigous?    "alpha.beta(1, 2)"))
  (is (unambigous?    "alpha.beta(\n 1 \n , \n 2 \n)"))
  (is (= (ruby-parser "alpha.beta(1, 2)")
         (ruby-parser "alpha.beta(\n 1 \n , \n 2 \n)")
         [[:method-call
           [:var-ref "alpha"]
           "beta"
           [:number "1"]
           [:number "2"]]]))
  (is (unambigous?    "alpha[]"))
  (is (= (ruby-parser "alpha[]")
         [[:method-call-bracket
           [:var-ref "alpha"]]]))
  (is (unambigous?    "alpha[1]"))
  (is (= (ruby-parser "alpha[1]")
         [[:method-call-bracket
           [:var-ref "alpha"]
           [:number "1"]]]))
  (is (unambigous?    "alpha[1, 2]"))
  (is (= (ruby-parser "alpha[1, 2]")
         [[:method-call-bracket
           [:var-ref "alpha"]
           [:number "1"]
           [:number "2"]]]))
  (is (unambigous? "alpha < 4"))
  (is (= (ruby-parser "alpha < 4")
         [[:method-call-relop
           [:var-ref "alpha"]
           "<"
           [:number "4"]]])))

(deftest logic-has-higher-precedence-than-assignment
  (is (unambigous?    "a = b && c"))
  (is (= (ruby-parser "a = b && c")
         [[:assignment
           "a"
           [:method-call-logic
            [:var-ref "b"]
            "&&"
            [:var-ref "c"]]]]))
  (is (unambigous?    "a += b && c"))
  (is (= (ruby-parser "a += b && c")
         [[:assignment-mutate
           "a"
           "+="
           [:method-call-logic
            [:var-ref "b"]
            "&&"
            [:var-ref "c"]]]]))
  (is (unambigous?    "a[b] = c && d"))
  (is (= (ruby-parser "a[b] = c && d")
         [[:assignment-bracket
           [:var-ref "a"]
           [:var-ref "b"]
           [:method-call-logic
            [:var-ref "c"]
            "&&"
            [:var-ref "d"]]]]))
  (is (unambigous?    "a[b] += c && d"))
  (is (= (ruby-parser "a[b] += c && d")
         [[:assignment-bracket-mutate
           [:var-ref "a"]
           [:var-ref "b"]
           "+="
           [:method-call-logic
            [:var-ref "c"]
            "&&"
            [:var-ref "d"]]]])))

(deftest relational-operator-has-higher-precedence-than-logic-operator
  (is (unambigous?    "a < b && x == y"))
  (is (= (ruby-parser "a < b && x == y")
         [[:method-call-logic
           [:method-call-relop [:var-ref "a"] "<" [:var-ref "b"]]
           "&&"
           [:method-call-relop [:var-ref "x"] "==" [:var-ref "y"]]]])))

(deftest method-call-has-higher-precedence-than-relational-operator
  (is (unambigous? "alpha.beta < 4"))
  (is (= (ruby-parser "alpha.beta < 4")
         [[:method-call-relop
           [:method-call
            [:var-ref "alpha"]
            "beta"]
           "<"
           [:number "4"]]]))
  (is (unambigous? "1 == gamma.delta"))
  (is (= (ruby-parser "1 == gamma.delta")
         [[:method-call-relop
           [:number "1"]
           "=="
           [:method-call
            [:var-ref "gamma"]
            "delta"]]])))

(deftest method-call-has-higher-precedence-than-method-call-with-no-parens
  (is (unambigous?    "a.b c[d]"))
  (is (= (ruby-parser "a.b c[d]")
         [[:method-call-no-parens
           [:var-ref "a"]
           "b"
           [:method-call-bracket
            [:var-ref "c"]
            [:var-ref "d"]]]])))

(deftest flow-if
  (is (unambigous?    "if 1; o.m; 10; end"))
  (is (= (ruby-parser "if 1; o.m; 10; end")
         [[:if
           [:if-branch [:number "1"]
            [:method-call [:var-ref "o"] "m"]
            [:number "10"]]]]))
  (is (unambigous?    "if 1; 2; elsif 3; 4; end"))
  (is (= (ruby-parser "if 1; 2; elsif 3; 4; end")
         [[:if
           [:if-branch [:number "1"]
            [:number "2"]]
           [:if-branch [:number "3"]
            [:number "4"]]]])))

(deftest flow-while
  (is (unambigous? "while 1; o.m; 10; end"))
  (is (= (ruby-parser "while 1; o.m; 10; end")
         [[:while [:number "1"]
           [:method-call [:var-ref "o"] "m"]
           [:number "10"]]])))

(deftest flow-until
  (is (unambigous? "until 1; o.m; 10; end"))
  (is (= (ruby-parser "until 1; o.m; 10; end")
         [[:until [:number "1"]
           [:method-call [:var-ref "o"] "m"]
           [:number "10"]]])))

(deftest flow-case
  (is (unambigous? "case 1; when 1; 2; end"))
  (is (unambigous? "case 1 ;;; when 1 ;;; 2 ;;; end"))
  (is (= (ruby-parser "case 1; when 1; 2; end")
         (ruby-parser "case 1 ;;; when 1 ;;; 2 ;;; end")
         [[:case [:number "1"]
           [:when
            [:number "1"]
            [:number "2"]]]])))

(deftest method-definition
  (is (unambigous?    "def foo; end"))
  (is (= (ruby-parser "def foo; end")
         [[:method-def "foo"]]))
  (is (unambigous?    "def foo; a; end"))
  (is (= (ruby-parser "def foo; a; end")
         [[:method-def "foo"
           [:var-ref "a"]]])))

(deftest multiple-statements
  (is (unambigous? "a;b"))
  (is (unambigous? "a\nb"))
  (is (unambigous? " a \n b "))
  (is (= (ruby-parser "a;b")
         (ruby-parser "a\nb")
         (ruby-parser "a \n b")
         [[:var-ref "a"]
          [:var-ref "b"]])))

(deftest rewrite-bracket
  (is (= (rename-bracket :obj)
         [:method-call :obj "[]"]))
  (is (= (rename-bracket :obj :idx)
         [:method-call :obj "[]" :idx]))
  (is (= (rename-bracket :obj :idx1 :idx2)
         [:method-call :obj "[]" :idx1 :idx2])))

(deftest rewrite-bracket-assignment
  (is (= (rename-assignment-bracket :obj :val)
         [:method-call :obj "[]=" :val]))
  (is (= (rename-assignment-bracket :obj :idx :val)
         [:method-call :obj "[]=" :idx :val]))
  (is (= (rename-assignment-bracket :obj :idx1 :idx2 :val)
         [:method-call :obj "[]=" :idx1 :idx2 :val])))

(deftest rewrite-mutating-assignment
  (is (= (rewrite-assignment-mutate :var-name "+=" :arg)
         [:assignment :var-name [:method-call [:var-ref :var-name] "+" :arg]])))

(deftest rewrite-mutating-bracket-assignment
  (is (= (rewrite-assignment-bracket-mutate :obj "-=" :val)
         [:method-call :obj "[]="
          [:method-call
           [:method-call :obj "[]"]
           "-"
           :val]]))
  (is (= (rewrite-assignment-bracket-mutate :obj :idx "-=" :val)
         [:method-call :obj "[]=" :idx
          [:method-call
           [:method-call :obj "[]" :idx]
           "-"
           :val]]))
  (is (= (rewrite-assignment-bracket-mutate :obj :idx1 :idx2 "-=" :val)
         [:method-call :obj "[]=" :idx1 :idx2
          [:method-call
           [:method-call :obj "[]" :idx1 :idx2]
           "-"
           :val]])))
