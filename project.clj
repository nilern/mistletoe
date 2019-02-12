(defproject mistletoe "0.1.0-SNAPSHOT"
  :description "Dom magic box"
  :url "http://github.com/nilern/mistletoe"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}


  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.516"]]
                 
  :plugins [[lein-cljsbuild "1.1.7"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :output-dir "resources/public/js"
                                   :main mistletoe.core
                                   :asset-path "js"}}]})
