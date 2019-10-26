(ns thinkstats2e.chapter3
  (:require [thinkstats2e.data :refer :all]
            [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as s]
  )
)


;; chapter 3

; 3.1

(defn normalize-frequencies
  [freqs]
  (let [norm (apply + (vals freqs))]
      (->> (for [[k v] freqs] 
                 [k (float (/  v norm))])
           (into (sorted-map))
      ) 
  )
)

(defn mean-of-pmf
     [pmf]
     (reduce +
         (for [[k v] pmf]
              (* k v)
         )
     )
)

(defn pmf
   [series]
   (-> series
       (frequencies)
       (normalize-frequencies)
   )
)

(defn test-pmf
   []
   (let [series  [1 1 1 2 2 3]]
       (println "input" series)
       (println "pmf" (pmf series))
   )
)


; 3.2  histogram with density=true

(defn first-vs-later-density
   [data]
   (let [first-children (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :prglngth)
                        )
         later-children (->> data 
                              (i/$where {:outcome 1 :birthord {:$ne 1} :prglngth {:gt 27}})
                              (select-filtered :prglngth)
                         )
        chart (-> (c/histogram  first-children 
                                :nbins 20         
                                :density true 
                                :title "length of livebirth-pregnancies"
                                :series-label "first child"
                                :legend true
                  )
                  (c/add-histogram later-children
                             :nbins 20
                             :series-label "second and later children"
                  )
              )
       
        chart (do 
                 (.setSeriesOutlinePaint (-> chart .getPlot .getRenderer) 0 java.awt.Color/red) 
                 (.setSeriesOutlinePaint (-> chart .getPlot .getRenderer) 1 java.awt.Color/blue)

                 chart
              )
        ]
        (-> (c/set-stroke-color chart java.awt.Color/black :series 0)
            (i/view)
        )
    )
)

; 3.3

(defn pmf-diff
   [a b r]
   (let [pmfa (pmf a)
         pmfb (pmf b)
        ]
        (for [x r
             :let [pa (get pmfa x 0)
                   pb (get pmfb x 0)]
             ]
             (* 100 (- pa pb))
        )
    )
)

(defn first-vs-later-pmf-diff
   [data]
   (let [first-children (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :prglngth)
                        )
         later-children (->> data 
                              (i/$where {:outcome 1 :birthord {:$ne 1} :prglngth {:gt 27}})
                              (select-filtered :prglngth)
                         )
          x (range 30 50)
          y (pmf-diff first-children later-children x)
        ]
        (-> (c/bar-chart x y 
                        :title "pmf difference first (positive) vs latter (negative)"
                        :x-label "pregnancy length in weeks"
                        :y-label "in %")
            (i/view)
        )
    )
)

; 3.4

(defn bias-pmf
   [pmf]
   (->>(for [[k x] pmf]
            [k (* k x)])
       (into {})
       (normalize-frequencies)
   )
)

(defn students-and-profs
   []
   (let [ pmf (normalize-frequencies {7 8,   12 8, 17 14, 22 4,
                                    27 6, 32 12,  37 8, 42 3, 47 2}
              )
          biased-pmf (bias-pmf pmf)
        ]

        (println "mean pmf"        (mean-of-pmf pmf))
        (println "mean biased-pmf" (mean-of-pmf biased-pmf))
        (-> (c/bar-chart (keys pmf) (vals pmf) 
                        :title "pmf vs biased-pmf"
                        :x-label "class size"
                        :y-label "in %"
                        :legend true
                        :series-label "original pmf")
            (c/add-categories (keys biased-pmf) (vals biased-pmf)
                        :series-label "biased pmf")
            (i/view)
        )

   )
)

(defn unbias-pmf
   [pmf]
   (->>(for [[k x] pmf]
            [k (/ x k)])
       (into {})
       (normalize-frequencies)
   )
)

(defn bias-unbias
   []
   (let [ pmf (normalize-frequencies {7 8,   12 8, 17 14, 22 4,
                                    27 6, 32 12,  37 8, 42 3, 47 2}
              )
          biased-pmf (bias-pmf pmf)
          unbiased-pmf (unbias-pmf biased-pmf)
        ]
        (println pmf)
        (println unbiased-pmf)
        (println "maximal difference:"
           (apply max (for [[k v] pmf
                              :let [o (get unbiased-pmf k 0)]
                           ]
                           (i/abs (- v o))
                      )
           )
       )
   )
)




