(ns thinkstats2e.chapter2
  (:require [thinkstats2e.data :refer :all]
            [incanter.core :as i]
            [incanter.charts :as c]
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

