(ns chapter1.core
  (:require [chapter1.data :refer :all]
            [chapter1.examples :refer :all]
  )
  (:gen-class)
)

(def examples [
                   
              ]
)

(def examples-with-data [
                       view-data
                       check-outcome
                       check-weight
                       check-weight2
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

  (let [data (load-clean-dataset)]
      (doseq  [f examples-with-data] 
          (-> (f data)
              (pr-str)
              (println)
          )
      )
  )
)
