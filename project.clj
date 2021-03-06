(defproject sicp "0.1.0-SNAPSHOT"
  :description "SICP"
  :url "http://github.com/ericdwhite/sicp"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"] 
                 ]
  :repl-options {
                 :init (use 'sicp.core 'sicp.core-test)
                 })
