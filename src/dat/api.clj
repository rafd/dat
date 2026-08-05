(ns dat.api
  (:require
   [clojure.java.io :as io]
   [malli.core :as m]
   [malli.registry :as mr]
   [dat.uuid :as uuid]
   [dat.malli :as dm]
   [dat.schema :as schema]
   [datascript.core :as d]))

(defn- init-conn!
  [db-type schema db-opts]
  (case db-type
    :dat.db/datalevin
    ((requiring-resolve 'datalevin.core/get-conn)
     (:dir db-opts)
     (schema/->db-schema db-type schema))

    :dat.db/datascript
    (if (and (:file-path db-opts)
             (.exists (io/file (:file-path db-opts))))
      (let [c (d/restore-conn (d/file-storage (:file-path db-opts)))]
        (d/reset-schema! c (schema/->db-schema db-type schema))
        c)
      (d/create-conn (schema/->db-schema db-type schema)
                     {:storage (d/file-storage (:file-path db-opts))}))))

(defn init!
  [db-type schema db-opts]
  {:pre [(m/validate schema/Schema schema)]}
  (mr/set-default-registry! (dm/->malli-registry schema))
  (atom
   {::db-type db-type
    ::db-opts db-opts
    ::schema schema
    ::conn (init-conn! db-type schema db-opts)}))

(defn conn
  "Returns the underlying backend connection (a datalevin.core or
  datascript.core conn), for backend-specific operations dat.api doesn't
  wrap, e.g. datalevin.core/listen! or datalevin.core/entity."
  [db]
  (::conn @db))

(defn close!
  [db]
  (let [{::keys [db-type conn]} @db]
    (case db-type
      :dat.db/datalevin ((requiring-resolve 'datalevin.core/close) conn)
      nil)))

(defn clear!
  [db]
  (let [{::keys [db-type schema db-opts conn]} @db]
    (case db-type
      :dat.db/datalevin ((requiring-resolve 'datalevin.core/clear) conn)
      nil)
    (swap! db assoc ::conn (init-conn! db-type schema db-opts))))

(defn transact!
  [db txs]
  (case (::db-type @db)
    :dat.db/datalevin ((requiring-resolve 'datalevin.core/transact!) (::conn @db) txs)
    :dat.db/datascript (d/transact! (::conn @db) txs)))

(defn q
  [query derefed-db & args]
  (apply (case (::db-type derefed-db)
           :dat.db/datalevin (requiring-resolve 'datalevin.core/q)
           :dat.db/datascript d/q)
         query @(::conn derefed-db) args))

(defn pull
  [derefed-db selector eid]
  ((case (::db-type derefed-db)
     :dat.db/datalevin (requiring-resolve 'datalevin.core/pull)
     :dat.db/datascript d/pull)
   @(::conn derefed-db) selector eid))

(defn register-fn!
  "Registers a transaction function `f` under `ident`, so that tx-data
  entries of the form `[ident & args]` invoke it as a transaction function.

  For :dat.db/datalevin, `f` must be built with `datalevin.interpret/inter-fn`
  at the call site (it needs to see the literal `fn` form to make it
  serializable), and its args are `[db & args]`."
  [db ident f]
  (transact! db [{:db/ident ident :db/fn f}]))

(defn uuid []
  (uuid/random))


