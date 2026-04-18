(ns dat.malli
  (:require
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
   :db.type/instant :inst})

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
              :pos-int (m/-simple-schema {:type :pos-int :pred pos-int?})}
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
