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
    ;; without a :file-path, the db is in-memory only
    (let [db-schema (schema/->db-schema db-type schema)
          file-path (:file-path db-opts)]
      (cond
        (and file-path
             (.exists (io/file file-path)))
        (doto (d/restore-conn (d/file-storage file-path))
          (d/reset-schema! db-schema))

        file-path
        (d/create-conn db-schema {:storage (d/file-storage file-path)})

        :else
        (d/create-conn db-schema)))))

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

(defn- delete-dir!
  [path]
  (doseq [file (reverse (file-seq (io/file path)))]
    (io/delete-file file true)))

(defn clear!
  [db]
  (let [{::keys [db-type schema db-opts conn]} @db]
    (case db-type
      :dat.db/datalevin ((requiring-resolve 'datalevin.core/clear) conn)
      :dat.db/datascript (when-let [file-path (:file-path db-opts)]
                           (delete-dir! file-path))
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


