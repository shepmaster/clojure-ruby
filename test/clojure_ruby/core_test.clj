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
  (is (= (stdout-of "def foo; STDOUT.putc 'C'; end; foo")
         "C"))
  (is (= (stdout-of "def foo x; STDOUT.putc x; end; foo 'D'")
         "D"))
  (is (= (stdout-of "def foo; 'E'; end; STDOUT.putc foo")
         "E"))
  (is (= (stdout-of "class Foo; def bar; 'F'; end; end; f = Foo.new; STDOUT.putc f.bar")
         "F"))
  (is (= (stdout-of "class Foo; def bar; 'G'; end; end
                     class Foo; def baz; 'H'; end; end
                     f = Foo.new
                     STDOUT.putc f.bar
                     STDOUT.putc f.baz")
         "GH"))
  (is (= (stdout-of "class Foo; def bar; 'I'; end; end
                     class Foo; def baz; bar; end; end
                     f = Foo.new
                     STDOUT.putc f.baz")
         "I"))
)
