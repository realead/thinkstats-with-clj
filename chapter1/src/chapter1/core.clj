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

  (let [data (read-dat-file)]
      (doseq  [f examples-with-data] 
          (-> (f data)
              (pr-str)
              (println)
          )
      )
  )
)
