(ns thinkstats2e.chapter2
  (:require [thinkstats2e.data :refer :all]
            [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as s]
  )
)


;; chapter 2

(defn hists-of-data
   [data]
   (let [keys [:birthwgt_oz :totalwgt_kg :agepreg :prglngth]]
        (doseq  [key keys] 
          (-> (select-filtered key data)
              (c/histogram  :nbins 100         
                            :density false  
                            :title (str key)
              )
              (i/view)
          )
       )
   )
)


(defn hists-of-filtered-prglngth
   [data]
   (let [x (->> (i/$where {:outcome 4} data)
                (select-filtered :prglngth)
           )
        ]
        (-> (c/histogram  x :nbins 100         
                            :density false  
                            :title "distribution week miscarriage"
            )
            (i/view)
        )
   )
)


(defn tails-of-hist-values
  [n x]
  (let [freqs (into (sorted-map) (frequencies x))]
       [(take n freqs)
        (take-last n freqs)]
  )
)

(defn smallest-biggest-of-filtered-prglngth
   [data]
   (let [[shortest, longest] (->> data
                                  (i/$where {:outcome 1})
                                  (select-filtered :prglngth)
                                  (tails-of-hist-values 10)   
                             )
        ]
        (println "shortest livebirth pregs:" shortest)
        (println "longest livebirth pregs:" longest)
   )
)

(defn first-vs-later
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
                                :density false  
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
                 ; changes color of all histograms
                 ;(.setPaint (-> chart .getPlot .getRenderer) java.awt.Color/blue)
                 ;only one having an effec
                 (.setSeriesOutlinePaint (-> chart .getPlot .getRenderer) 0 java.awt.Color/red) 
                 ;(.setSeriesFillPaint    (-> chart .getPlot .getRenderer) 0 java.awt.Color/red)
                 ;(.setSeriesPaint         (-> chart .getPlot .getRenderer) 0 java.awt.Color/red)
                 (.setSeriesOutlinePaint (-> chart .getPlot .getRenderer) 1 java.awt.Color/blue)

                 chart
              )
        ]
        (-> (c/set-stroke-color chart java.awt.Color/black :series 0)
            (i/view)
        )
    )
)

(defn get-series-stats
   [series]
   [(s/mean series) (s/variance series) (s/sd series)]
)

(defn first-vs-later-stats
   [data]
   (let [first-children (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :prglngth)
                        )
         later-children (->> data 
                              (i/$where {:outcome 1 :birthord {:$ne 1} :prglngth {:gt 27}})
                              (select-filtered :prglngth)
                         )
        ]
        (println "mean/var/sd of first children" (get-series-stats first-children))
        (println "mean/var/sd of later children" (get-series-stats later-children))    
    )
)

(defn cohen-effect-size
  [a b]
  (let [var1 (s/variance a)
        var2 (s/variance b)
        n1 (count a)
        n2 (count b)
        pooled_var (/ (+ (* n1 var1)  (* n2 var2))
                      (+ n1 n2) 
                   )
        diff (- (s/mean a) (s/mean b))
       ]
       (/ diff (i/sqrt pooled_var))
   )
)

(defn first-vs-later-stats
   [data]
   (let [first-children (->> data
                             (i/$where {:birthord 1 :prglngth {:gt 27}})
                             (select-filtered :prglngth)
                        )
         later-children (->> data 
                              (i/$where {:outcome 1 :birthord {:$ne 1} :prglngth {:gt 27}})
                              (select-filtered :prglngth)
                         )
        ]
        (println "mean/var/sd of first children" (get-series-stats first-children))
        (println "mean/var/sd of later children" (get-series-stats later-children)) 
        (println "cohen effect size" (cohen-effect-size first-children later-children))    
    )
)



