(ns dat.malli
  (:require
   [hyperfiddle.rcf :as rcf]
   [malli.core :as m]
   [malli.registry :as mr]
   [dat.util :as util]))

;; https://docs.datomic.com/schema/schema-reference.html
;; https://github.com/metosin/malli?tab=readme-ov-file#built-in-schemas
(def datalog-type->malli-type
  {:db.type/uuid :uuid
   :db.type/long :int
   :db.type/string :string
   :db.type/float :float
   :db.type/keyword :keyword
   :db.type/boolean :boolean
   :db.type/instant :inst
   :db.type/bytes :bytes})

(defn bytes-value?
  [value]
  #?(:clj (bytes? value)
     :cljs (or (instance? js/Uint8Array value)
               (instance? js/Int8Array value))))

(defn ->malli-spec
  [{:dat/keys [type spec]}]
  (let [base-type (datalog-type->malli-type type)]
    (if spec
      [:and base-type spec]
      base-type)))

(defn ->malli-registry
  [schema]
  (-> (merge (mr/schemas m/default-registry)
             {:neg-int (m/-simple-schema {:type :neg-int :pred neg-int?})
              :whole-int (m/-simple-schema {:type :whole-int :pred #(or (zero? %)
                                                                        (pos-int? %))})
              :pos-int (m/-simple-schema {:type :pos-int :pred pos-int?})
              :bytes (m/-simple-schema {:type :bytes :pred bytes-value?})}
             ;; entities
             (->> schema
                  (map (fn [[k vs]]
                         [k (into [:map]
                                  (->> vs
                                       (map (fn [[k v]]
                                              ;; TODO rel types
                                              [k (->malli-spec v)]))
                                       (into {})
                                       util/remove-nil-vals))]))
                  (into {}))
             ;; attrs
             (->> schema
                  (mapcat val)
                  (map (fn [[k v]]
                         ;; TODO rel-types
                         [k (->malli-spec v)]))
                  (into {})
                  util/remove-nil-vals))))

(rcf/tests
 (let [registry (->malli-registry {:entity/file {:file/data {:dat/type :db.type/bytes}}})
       validate (fn [value]
                  (m/validate :file/data value {:registry registry}))]
   [(validate #?(:clj (byte-array [1 2 3])
                 :cljs (js/Uint8Array. #js [1 2 3])))
    (validate "not-bytes")])
 := [true false])
