(ns chapter1.core
  (:require [chapter1.data :refer :all]
            [chapter1.examples :refer :all]
  )
  (:gen-class)
)

(def examples [
                 ex-1-2a
                 ex-1-2b
                 ex-1-2c 
                 ex-1-3a
                 ex-1-3b
              ]
)

(def examples-with-preg-data [
               view-data
               check-outcome
               check-weight
               check-weight2
               caseid-10229
               ex-1-1a
               ex-1-1b
               ex-1-1c
            ]
)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (doseq  [f examples] 
      (-> (f)
          (pr-str)
          (println)
      )
  )

  (let [dataset (load-clean-dataset :FemPreg2002)]
      (doseq  [f examples-with-preg-data] 
          (-> (f dataset)
              (pr-str)
              (println)
          )
      )
  )
)
