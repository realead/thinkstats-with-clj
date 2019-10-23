(ns thinkstats2e.core
  (:require [thinkstats2e.data :refer :all]
            [thinkstats2e.chapter1 :as ch1]
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
  (run ch1-descr)
)
