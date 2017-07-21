(defproject viz "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "2.6.0"]
                 ;[gil "1.0.0-SNAPSHOT"]
                 [org.clojure/clojurescript "1.9.473"]
                 ;[lein-cljsbuild "1.1.6"]
                 ]

  :plugins [[lein-cljsbuild "1.1.5"]
            ]

  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.2"]
                                  [org.clojure/tools.nrepl "0.2.10"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   }}

  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds [{:source-paths ["src"]
             :compiler
             {:output-to "js/main.js"
              :output-dir "out"
              :main "viz.core"
              ;:externs ["externs/processing.js"]
              :preamble ["processing.min.js"]
              ;:optimizations :advanced
              :optimizations :none
              :pretty-print false}}
            ]
   }

  )
