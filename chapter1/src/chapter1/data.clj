(ns chapter1.data
  (:require [clojure.java.io :as io]
            [incanter.core :as i]
            [incanter.io :as iio]

  )
)


(defn clean-preg-dataset
   [dataset]
   (-> dataset
       (i/transform-col :agepreg #(if (some? %) (/ % 100.0) Double/NaN))
       (i/transform-col :birthwgt_lb #(if (contains? #{97 98 99 51 nil} %) Double/NaN %))
       (i/transform-col :birthwgt_oz #(if (contains? #{97 98 99 nil} %) Double/NaN %))
   )
)

(defn postload-preg-dataset
   [dataset]
   (->> (clean-preg-dataset dataset)
        (i/add-derived-column :totalwgt_lb [:birthwgt_lb :birthwgt_oz] #(+ %1 (/ %2 16.0)))
        (i/add-derived-column :totalwgt_kg [:totalwgt_lb] #(* % 0.453592))
   )
)

(def dataset-infos {:FemPreg2002  {:dct-file "data/2002FemPreg.dct"
                                   :dat-file "data/2002FemPreg.dat"
                                   :features #{:caseid :prglngth :outcome 
                                              :pregordr :birthord :birthwgt_lb
                                              :birthwgt_oz :agepreg :finalwgt}
                                   :postload  postload-preg-dataset
                                  }
                   :FemResp2002   {:dct-file "data/2002FemResp.dct"
                                   :dat-file "data/2002FemResp.dat"
                                   :features #{:caseid :pregnum}
                                   :postload identity 
                                  }
                  }
)

(defn open-dct-file
   [dct-file]
   (->> (io/reader dct-file)
        (line-seq)
        (rest)
        (butlast)
   )
)

(def type2parser
   {"byte" #(Integer/parseInt %)
    "int"  #(Integer/parseInt %)
    "str12"  #(Long/parseLong %);
    "float" #(Double/parseDouble %)
    "double" #(Double/parseDouble %)
   }
)

(defn parse-dct-line
  [line]
  (let [words   (clojure.string/split line #" +")
        wstart  (nth words 1)
        start   (Integer/parseInt (subs wstart 8 (dec (count wstart))))
        type    (nth words 2)
        wlength (nth words 4)
        length  (Integer/parseInt (subs wlength 1 (dec (count wlength))))]
     {:typeparser (get type2parser type str)
      :name (keyword (nth words 3))
      :start (dec start) ;start 1-base
      :end (+ (dec start) length) ;start 1-based
     } 
  )
)


(defn create-dat-line-parser-from-dct
  [dct-file]
  (map parse-dct-line (open-dct-file dct-file))
)


(defn extract-entry-from-line
    [line extractor parsed]
    (let [s (->> (subs line (:start extractor) (:end extractor))
                 (clojure.string/trim)
            )
          val (if (empty? s) 
                  nil 
                  ((:typeparser extractor) s)
              )
         ]
         (conj parsed val)
    )
)

(defn parse-dat-line
  [line dat-line-parser]
  (reduce #(extract-entry-from-line line %2 %1) [] dat-line-parser)
)


(defn open-dat-file
   [dat-file]
   (->> (io/reader dat-file)
        (line-seq)
   )
)

(defn read-dat-file
  [dataname]
  (let [info (dataname dataset-infos)
        parser (create-dat-line-parser-from-dct (:dct-file info))
        data (open-dat-file (:dat-file info))]
       (i/dataset (map :name parser) (map #(parse-dat-line % parser) data))
  )    
)

(defn read-dat-file-filtered
  [{:keys [features dct-file  dat-file]}]
  (let [parser (create-dat-line-parser-from-dct dct-file)
        filtered_parser (filter #(contains? features (:name %)) parser)
        data (open-dat-file dat-file)]
       (i/dataset (map #(:name %) filtered_parser) 
                  (map #(parse-dat-line % filtered_parser) data)
       )
    )   
)


(defn load-clean-dataset
   [dataname]
   (let [info (dataname dataset-infos)
         postload (:postload info)]
        (postload (read-dat-file-filtered info))
   )
)


(defn select-filtered
   [col-name dataset]
   (->> dataset
        (i/$where {col-name {:$fn #(Double/isFinite %)}}) 
        (i/$ col-name)
   )
)


(defn get-nth-row-element
   [column-name row-id dataset]
   (let [selected (i/$ column-name dataset)]
        (if (sequential? selected)
            (nth selected row-id)
            selected
        )
   )
)


   

