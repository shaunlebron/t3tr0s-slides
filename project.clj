(defproject t3tr0s-slides "0.1.0-SNAPSHOT"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [org.clojure/core.async "0.1.319.0-6b1aca-alpha"]
                 [sablono "0.2.21"]
                 [om "0.7.1"]
                 [prismatic/om-tools "0.3.2"]]

  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild { 
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {
                :output-to "public/t3tr0s_slides.js"
                :output-dir "out-dev"
                :optimizations :whitespace}}
             {:id "min"
              :source-paths ["src"]
              :compiler {
                :externs ["externs/react.js"
                          "externs/jquery.js"]
                :output-to "public/t3tr0s_slides.min.js"
                :output-dir "out-min"
                :optimizations :advanced}}]})
