(ns dat.api
  (:require
   [clojure.java.io :as io]
   [malli.core :as m]
   [malli.registry :as mr]
   [dat.uuid :as uuid]
   [dat.malli :as dm]
   [dat.schema :as schema]
   [datascript.core :as d]))

(defn init!
  [db-type schema db-opts]
  {:pre [(m/validate schema/Schema schema)]}
  (mr/set-default-registry! (dm/->malli-registry schema))
  (atom
   {::db-type db-type
    ::db-opts db-opts
    ::schema schema
    ::conn
    (if (and (:file-path db-opts)
             (.exists (io/file (:file-path db-opts))))
      (let [c (d/restore-conn (d/file-storage (:file-path db-opts)))]
        (datascript.core/reset-schema! c (schema/->db-schema db-type schema))
        c)
      (d/create-conn (schema/->db-schema db-type schema)
                     {:storage (d/file-storage (:file-path db-opts))}))}))

(defn close!
  [db]
  (let [{::keys [db-type conn]} @db]
    (case db-type
      :dat.db/datalevin ((requiring-resolve 'datalevin.core/close) conn)
      nil)))

(defn clear!
  [db]
  (let [{::keys [db-type schema db-opts]} @db]
    (swap! db assoc ::conn (d/create-conn
                            (schema/->db-schema db-type schema)
                            {:storage (d/file-storage (:file-path db-opts))})))
  ;; datalevin
  #_(d/clear conn))

(defn transact!
  [db txs]
  (d/transact! (::conn @db) txs))

(defn q
  [query derefed-db & args]
  (apply d/q query @(::conn derefed-db) args))

(defn pull
  [derefed-db selector eid]
  (d/pull @(::conn derefed-db) selector eid))

(defn uuid []
  (uuid/random))


