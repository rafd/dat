(ns dat.schema
  (:require
   [malli.core :as m]
   [hyperfiddle.rcf :as rcf]
   [com.rpl.specter :as x]
   [dat.test-schema :as ts]
   [dat.malli :as dm]
   [dat.util :as util]))

(def Schema
  [:map-of
   :keyword
   [:map-of
    :keyword
    [:map
     [:dat/type
      {:optional true}
      (into [:enum] (keys dm/datalog-type->malli-type))]
     [:dat/unique {:optional true}
      [:enum :dat.unique/identity]]
     [:dat/no-history {:optional true} :boolean]
     [:dat/rel {:optional true}
      [:tuple
       [:enum :dat.rel/one :dat.rel/many]
       :keyword
       :keyword]]]]])

(defn by-key [schema]
  (->> schema
       vals
       (apply concat)
       (into {})))

(rcf/tests
 (by-key ts/test-schema)
 :=
 {:user/id _
  :post/id _
  :post/user _})

(defn attrs [schema]
  (->> schema
       (x/select [x/MAP-VALS x/MAP-KEYS])))

(rcf/tests
 (attrs ts/test-schema)
 := [:user/id :post/id :post/user])

(defn direct-attrs
  "Returns non-rel attrs for a given entity-type"
  [schema entity-type]
  (->> (get schema entity-type)
       (keep (fn [[attr opts]]
                 (when-not (:dat/rel opts)
                   attr)))))

(rcf/tests
 (direct-attrs ts/test-schema :entity/post)
 := [:post/id])

(defn id-key [schema entity-type]
  (->> (get schema entity-type)
       (some (fn [[attr opts]]
               (when (and (= :dat.unique/identity (:dat/unique opts))
                          ;; requiring that every entity has a /id attr
                          (= "id" (name attr)))
                 attr)))))
(rcf/tests
 (id-key ts/test-schema :entity/post)
 := :post/id)

(defn ->db-schema
  [db-type schema]
  {:pre [(m/validate [:enum
                      :dat.db/datomic
                      :dat.db/datelevin
                      :dat.db/datascript] db-type)
         (m/validate Schema schema)]}
  (->> schema
       by-key
       (map (fn [[k o]]
              (-> {:db/ident k
                   :db/unique (case (:dat/unique o)
                                :dat.unique/identity :db.unique/identity
                                nil)
                   :db/valueType (or (when (#{:dat.db/datalevin
                                              :dat.db/datomic} db-type)
                                       (:dat/type o))
                                     (when (:dat/rel o)
                                       :db.type/ref))
                   :db/cardinality (if-let [[cardinality _ _] (:dat/rel o)]
                                     (case cardinality
                                       :dat.rel/one
                                       :db.cardinality/one
                                       :dat.rel/many
                                       :db.cardinality/many
                                       nil)
                                     :db.cardinality/one)
                   :db/noHistory (when (and (= db-type :dat.db/datomic)
                                            (:dat/no-history o))
                                   true)}
                   util/remove-nil-vals)))
       ((fn [vals]
          (case db-type
            :dat.db/datomic vals
            :dat.db/datascript (zipmap (map :db/ident vals)
                                       (map #(dissoc % :db/ident) vals)))))))

