(ns thinkstats2e.utils

)

;;;; pmf-functionality

(defn normalize-frequencies
  [freqs]
  (let [norm (apply + (vals freqs))]
      (->> (for [[k v] freqs] 
                 [k (double (/  v norm))])
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

(defn variance-of-pmf
     [pmf]
     ;no so good: traverses series twice
     (let [m (mean-of-pmf pmf)]
           (reduce +   
               (for [[k p] pmf
                    :let [d (- k m)]
                   ]
                   (* p d d)
               )
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

