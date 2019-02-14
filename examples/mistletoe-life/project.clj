(defproject mistletoe-life "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}

  :repositories [["jitpack" "https://jitpack.io"]]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.516"]
                 [com.github.nilern/mistletoe "eeb77ed"]]

  :plugins [[lein-cljsbuild "1.1.7"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {:builds [{:id           "dev"
                        :source-paths ["src"]
                        :compiler     {:output-to  "resources/public/js/main.js"
                                       :output-dir "resources/public/js"
                                       :main       mistletoe-life.core
                                       :asset-path "js"}}]})
