(defproject org.clojars.vladimirmarkovic86/utils-lib "0.4.7"
  :description "Utils library"
  :url "http://github.com/VladimirMarkovic86/utils-lib"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 ]

  :min-lein-version "2.0.0"
  
  :source-paths ["src/clj" "src/cljc"]
  :test-paths ["test/clj" "test/cljc"]

  :plugins [[lein-cljsbuild  "1.1.7"]
            [lein-doo "0.1.11"]
            ]

  :cljsbuild
    {:builds
      {:test
        {:source-paths ["src/cljc" "test/cljc" "test/cljs"]
         :compiler     {:main utils-lib.test-runner
                        :optimizations :whitespace
                        :output-dir "resources/public/assets/js/out/test"
                        :output-to "resources/public/assets/js/test.js"}}
       }}
  )

