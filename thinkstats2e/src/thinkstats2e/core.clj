(ns thinkstats2e.core
  (:require [thinkstats2e.data :refer :all]
            [thinkstats2e.chapter1 :as ch1]
            [thinkstats2e.chapter2 :as ch2]
            [thinkstats2e.chapter3 :as ch3]
            [thinkstats2e.chapter4 :as ch4]
  )
  (:gen-class)
)

(def ch1-descr
     {:no-data [
                 ch1/ex-1-2a
                 ch1/ex-1-2b
                 ch1/ex-1-2c 
                 ch1/ex-1-3a
                 ch1/ex-1-3b
               ]
     :with-data [
               ch1/view-data
               ch1/check-outcome
               ch1/check-weight
               ch1/check-weight2
               ch1/caseid-10229
               ch1/ex-1-1a
               ch1/ex-1-1b
               ch1/ex-1-1c
               ]
     :data-loader #(load-clean-dataset :FemPreg2002)
     }
)


(def ch2-descr
     {:no-data [
                  ch2/ex-2-2                 
                  ch2/ex-2-3
               ]
     :with-data [
                  ch2/hists-of-data
                  ch2/hists-of-filtered-prglngth
                  ch2/smallest-biggest-of-filtered-prglngth
                  ch2/first-vs-later
                  ch2/first-vs-later-stats
                  ch2/ex-2-4
               ]
     :data-loader #(load-clean-dataset :FemPreg2002)
     }
)


(def ch3-descr
     {:no-data [
                  ch3/test-pmf
                  ch3/students-and-profs
                  ch3/bias-unbias
                  ch3/work-dataframes
                  ch3/ex-3-1
                  ch3/ex-3-2
                  ch3/ex-3-4
               ]
     :with-data [
                  ch3/first-vs-later-density
                  ch3/first-vs-later-pmf-diff
                  ch3/ex-3-3
               ]
     :data-loader #(load-clean-dataset :FemPreg2002)
     }
)

(def ch4-descr
     {:no-data [
                  ch4/check-percentiles
                  ch4/calc-cdf
                  ch4/test-eval-cdf
                  ch4/plot-cdf
                  ch4/test-value-cdf
                  ch4/ex-4-2
               ]
     :with-data [
                  ch4/plot-prglngth
                  ch4/plot-weight-comparison
                  ch4/weight-quantils
                  ch4/show-percentil-cdf
                  ch4/show-random
                  ch4/ex-4-1
               ]
     :data-loader #(load-clean-dataset :FemPreg2002)
     }
)


(def tasks { 
           :ch1 ch1-descr
           :ch2 ch2-descr
           :ch3 ch3-descr
           :ch4 ch4-descr
           }
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run 
   [{:keys [no-data with-data data-loader]}]
   (doseq  [f no-data] 
      (-> (f)
          (pr-str)
          (println)
      )
  )

  (let [dataset (data-loader)]
      (doseq  [f with-data] 
          (-> (f dataset)
              (pr-str)
              (println)
          )
      )
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [to-do (if (nil? args) (keys tasks) (map keyword args))]
    (doseq  [ch to-do] 
           (run (ch tasks))
    )
  )
)
