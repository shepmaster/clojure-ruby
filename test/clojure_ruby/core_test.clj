(ns clojure-ruby.core-test
  (:require [clojure.test :refer :all]
            [clojure-ruby.core :refer :all]))

(defn stdout-of [ruby-str]
  (with-out-str (full-eval ruby-str)))

(deftest full-stack
  (is (= (stdout-of "STDOUT.putc 65")
         "A"))
  (is (= (stdout-of "if 1 < 2; STDOUT.putc 66; end")
         "B"))
  (is (= (stdout-of "def foo; STDOUT.putc \"C\"; end; foo")
         "C")))
