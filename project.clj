(defproject clojure-ruby "0.1.0-SNAPSHOT"
  :description "An extremely bare-bones Ruby interpreter"
  :url "https://github.com/shepmaster/clojure-ruby"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.3.2"]]
  :profiles {:dev {:source-paths ["dev"]}}
  :main clojure-ruby.core)
