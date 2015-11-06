(defproject schedule "0.0.0-SNAPSHOT"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.145"]
                 [crate "0.2.1"]
                 [jayq "2.5.4"]]
  :plugins [[lein-cljsbuild "1.1.0"]]
  :cljsbuild {
    :builds {
      :main {
        :source-paths ["src"]
        :compiler {
          :output-dir "target/cljsbuild/main"
          :output-to "target/site/js/app.js"
          :optimizations :whitespace ; :advanced => error
          }}
      :dev {
        :source-paths ["src"]
        :compiler {
          :output-dir "target/cljsbuild/dev"
          :output-to "target/cljsbuild/dev.js"
          :optimizations :none}}}})
