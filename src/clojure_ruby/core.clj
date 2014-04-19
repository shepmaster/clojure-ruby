(ns clojure-ruby.core
  (:require [clojure-ruby.parser :as parse]
            [clojure-ruby.evaluate :as eval]
            [clojure-ruby.corelib :as lib]))

(defn blank-system []
  (eval/create-system lib/create-string
                      lib/create-number
                      lib/as-host-boolean
                      lib/global-variables))

(defn parse-eval [system ruby-str]
  (->> ruby-str
       parse/parse
       (eval/evaluate-all system)))

(def corelib-file
  "src/ruby/ruby_io.rb")

(defn initial-system []
  (let [system (blank-system)
        _ (parse-eval system (slurp corelib-file))]
    system))

(defn full-eval [ruby-str]
  (parse-eval (initial-system) ruby-str))

(defn -main [& args]
  (let [filename (or (first args) "example.rb")
        ruby-code (slurp filename)]
    (full-eval ruby-code)))
