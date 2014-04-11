(ns clojure-ruby.core
  (:require [clojure-ruby.parser :as parse]
            [clojure-ruby.evaluate :as eval]
            [clojure-ruby.corelib :as lib]))

(defn full-eval [ruby-str]
  (->> ruby-str
       parse/ruby-parser
       parse/clean-parse-tree
       (eval/evaluate-all lib/create-string lib/create-number lib/as-host-boolean lib/global-variables)))

(defn -main [& args]
  (let [filename (or (first args) "example.rb")
        ruby-code (slurp filename)]
    (full-eval ruby-code)))
