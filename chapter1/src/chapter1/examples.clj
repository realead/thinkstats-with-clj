(ns chapter1.examples
  (:require [chapter1.data :refer :all]
            [incanter.core :as i]
  )
)


;; chapter 1

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



;exercise 1.1

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


;exercise 1.2

(defn ex-1-2a
  []
  (let [data_resp (load-clean-dataset :FemResp2002)]
       (->> data_resp
            (i/$rollup :count :count :pregnum)
            (i/$order :pregnum :asc)
       )
  )
)

;print distribution from fem_preg
(defn ex-1-2b
  []
  (let [data_preg (load-clean-dataset :FemPreg2002)]
       (->> data_preg
            (i/$rollup :count :pregnum :caseid)
            (i/$rollup :count :count :pregnum)
            (i/$order :pregnum :asc)
       )
  )
)

;sanity check for datasets
(defn ex-1-2c
  []
  (let [data_resp (->> (load-clean-dataset :FemResp2002)
                       (i/$where {:pregnum {:$gt 0}})
                       (i/$order :caseid :asc)
                  )
        data_preg (->> (load-clean-dataset :FemPreg2002)
                       (i/$rollup :count :pregnum :caseid)
                       (i/$order :caseid :asc)
                  )
        ;same_ids (= (i/$ :caseid data_resp) (i/$ :caseid data_preg))
        ;same_nums (= (i/$ :pregnum data_resp) (i/$ :pregnum data_preg))
       ]
       (println "same" (= data_resp data_preg))
  )
)

