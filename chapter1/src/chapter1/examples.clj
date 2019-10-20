(ns chapter1.examples
  (:require [chapter1.data :refer :all]
            [incanter.core :as i]
  )
)

(defn view-data
  [data]
  (i/view data)
)

(defn check-outcome
  [data]
  (->> data
       (i/$ :outcome)
       (frequencies)
       (into (sorted-map))
  )
)

(defn check-weight
  [data]
  (->> data
       (select-filtered :birthwgt_lb)
       (frequencies)
       (into (sorted-map))
  )
)


(defn check-weight2
  [data]
  (->> data
       (select-filtered :birthwgt_lb)
       (every? #(< % 21))
  )
)

(defn caseid-10229
  [data]
  (->> data
       (i/$where {:caseid 10229})
       (i/$ :outcome)
  )
)
