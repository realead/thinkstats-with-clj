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

(defn ex-1-1a
  [data]
  (let [age (->> data
                 (i/$where {:caseid 1})
                 (i/$ :agepreg)
                 (apply max)
            )
        ]
        ; actually ager "AGE AT INTERVIEW" is probably  the right column
        ; but one could also understand as maximal of :agepreg
        (println "age for case 1:", age)
  )
)

(defn ex-1-1b
  [data]
  (let [durations (->> data
                    (i/$where {:caseid 2298})
                    (i/$ :prglngth)
            )
        ]
        (println "lengths of pregnancies for case 2298:", durations)
  )
)

(defn ex-1-1c
  [data]
  (let [weight (->>  data
                     (i/$where {:caseid 5012 :birthord 1})
                     (i/$ :totalwgt_kg)      
              )
        ]
        (println "weight for first live birth:", weight, "kg")
  )
)
