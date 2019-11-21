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


(defn cdf
   [series]
   (let [p (pmf series)
         m (apply min (keys p)) ]
        (->> (reductions (fn [f s] [(first s) (+ (second f) (second s))]) [(dec m) 0] p)
             (into (sorted-map))
        )

   )
)

(defn cdf
   [series]
   (let [p (pmf series)
         m (apply min (keys p)) ]
        (->> (reductions (fn [f s] [(first s) (+ (second f) (second s))]) [(dec m) 0] p)
             (into (sorted-map))
        )

   )
)

(defn eval-cdf
   [x cdf]
   (let [s (rsubseq cdf <= x)]
        (if (empty? s)
            0.0
            (second (first s))
         )
   )
)

; not fastest but works for now:
(defn value-cdf
   [x cdf]
   (-> (drop-while #(< (second %) x) cdf)
       (first)
       (first)
   )
)


 
