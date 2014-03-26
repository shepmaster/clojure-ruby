(ns clojure-ruby.core
  (:require [clojure-ruby.parser :as parse]
            [clojure-ruby.evaluate :as eval]))

(defn -main [& args]
  (let [filename (or (first args) "example.rb")
        ruby-code (slurp filename)]
    (-> ruby-code parse/ruby-parser parse/clean-parse-tree eval/evaluate-all)))
