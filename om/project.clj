(defproject quantum-ttt-om "0.1.0-SNAPSHOT"
  :description "Quantum Tic Tac Toe"
  :dependencies [[org.clojure/clojure "1.7.0-alpha6"]
                 [org.clojure/clojurescript "0.0-3126"]
                 [org.omcljs/om "0.8.8":exclusions [cljsjs/react]]
                 [cljsjs/react-with-addons "0.12.2-4"]]
  :plugins [[lein-cljsbuild "1.0.5"]]
  :clean-targets ^{:protect false} [:target-path :compile-path "resources/public/gen"]
  :cljsbuild {:builds {:dev {:source-paths ["src"]
                               :compiler {:main quantum-ttt.om
                                          :output-to "resources/public/gen/dev/main.js"
                                          :output-dir "resources/public/gen/dev"
                                          :asset-path "gen/dev"
                                          :optimizations :none
                                          :source-map true}}
                       :prod {:source-paths ["src"]
                              :compiler {:main quantum-ttt.om
                                         :output-to "resources/public/gen/main.js"
                                         :optimizations :advanced
                                         :pretty-print false}}}})
