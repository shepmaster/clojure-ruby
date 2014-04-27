(ns clojure-ruby.core-test
  (:require [clojure.test :refer :all]
            [clojure-ruby.core :refer :all]
            [clojure-ruby.corelib :as lib]))

(defn stdout-of [ruby-str]
  (with-out-str (full-eval ruby-str)))

(defn number-of [ruby-str]
  (let [retval (full-eval ruby-str)]
    (lib/host-msg retval :number)))

(defn string-of [ruby-str]
  (let [retval (full-eval ruby-str)]
    (lib/host-msg retval :string)))

(deftest literals
  (testing "integral"
    (is (= (number-of "65")
           65)))
  (testing "string"
    (is (= (string-of "'Hello'")
           "Hello"))))

(deftest defining-self-method
  (is (= (string-of "def foo; 'C'; end; foo")
         "C")))

(deftest defining-self-method-with-args
  (is (= (number-of "def foo x; x + 1; end; foo 1")
         2)))

(deftest defining-class
  (is (= (string-of "class Foo; def bar; 'F'; end; end
                     f = Foo.new
                     f.bar")
         "F"))
  (testing "multiple methods"
    (is (= (number-of "class Foo; def bar; 2; end; end
                       class Foo; def baz; 7; end; end
                       f = Foo.new
                       f.bar + f.baz")
           9)))
  (testing "methods calling other methods"
    (is (= (string-of "class Foo; def bar; 'I'; end; end
                       class Foo; def baz; bar; end; end
                       f = Foo.new
                       f.baz")
           "I"))))

(deftest flow-if
  (is (= (number-of "if 1 < 2; 66; end")
         66)))

(deftest flow-while
  (is (= (number-of "i = 0
                     val = 0
                     while i < 5
                       val += i
                       i += 1
                     end
                     val")
         10)))

(deftest flow-until
  (is (= (number-of "i = 0
                     val = 0
                     until 5 < i
                       val += i
                       i += 1
                     end
                     val")
         15)))

(deftest flow-case
  (is (= (string-of "i = 'b'
                     case i
                     when 'a'
                       'A'
                     when 'b'
                       'B'
                     when 'c'
                       'C'
                     end")
         "B")))

(deftest integer-math
  (is (= (number-of "100 + 1")
         101))
  (is (= (number-of "100 - 1")
         99)))

(deftest stdout
  (testing "putc"
    (is (= (stdout-of "STDOUT.putc 65")
           "A"))
    (is (= (stdout-of "STDOUT.putc 'C'")
           "C"))))
