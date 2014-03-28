(ns clojure-ruby.core
  (:require [clojure-ruby.parser :as parse]
            [clojure-ruby.evaluate :as eval]))

(defn full-eval [ruby-str]
  (-> ruby-str parse/ruby-parser parse/clean-parse-tree eval/evaluate-all))

(defn -main [& args]
  (let [filename (or (first args) "example.rb")
        ruby-code (slurp filename)]
    (full-eval ruby-code)))
