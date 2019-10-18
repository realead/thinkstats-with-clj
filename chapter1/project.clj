(defproject chapter1 "0.1.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter/incanter   "1.5.5"]]
  :resource-paths ["data"]
  :main ^:skip-aot chapter1.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
