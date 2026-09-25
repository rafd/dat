(ns dat.api
  {:clj-kondo/config '{:lint-as {dat.api/with-transaction clojure.core/let}}}
  (:require
   [clojure.java.io :as io]
   [hyperfiddle.rcf :as rcf]
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

(defn with-transaction-fn
  [db f]
  (let [{::keys [db-type conn]} @db]
    (case db-type
      :dat.db/datalevin
      ((requiring-resolve 'datalevin.core/with-transaction-fn)
       conn
       (reify java.util.function.Function
         (apply [_ tx-conn]
           (f (atom (assoc @db ::conn tx-conn))))))

      :dat.db/datascript
      ;; datascript has no transactions; serialize blocks and roll back on throw
      (locking conn
        (let [db-before @conn]
          (try
            (f db)
            (catch Throwable error
              (d/reset-conn! conn db-before)
              (throw error))))))))

(defmacro with-transaction
  "Runs `body` as a single atomic transaction. `tx-db` is a db to be used
  in place of `db` for `transact!`, `q` and `pull` within `body`.
  If `body` throws, all its writes are rolled back."
  [[tx-db db] & body]
  `(with-transaction-fn ~db (fn [~tx-db] ~@body)))

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

(rcf/tests
 "with-transaction commits the body's writes"
 (let [db (init! :dat.db/datascript
                 {:entity/user {:user/id {:dat/type :db.type/uuid
                                          :dat/unique :dat.unique/identity}
                                :user/name {:dat/type :db.type/string}}}
                 {})
       user-id (uuid)]
   (with-transaction [tx-db db]
     (transact! tx-db [{:user/id user-id
                        :user/name "Ada"}])
     (transact! tx-db [{:user/id user-id
                        :user/name "Ada L."}]))
   (q '[:find [?name ...] :where [_ :user/name ?name]] @db))
 := ["Ada L."])

(rcf/tests
 "with-transaction rolls back the body's writes when it throws"
 (let [db (init! :dat.db/datascript
                 {:entity/user {:user/name {:dat/type :db.type/string}}}
                 {})]
   (transact! db [{:user/name "Ada"}])
   [(try
      (with-transaction [tx-db db]
        (transact! tx-db [{:user/name "Grace"}])
        (throw (ex-info "boom" {})))
      (catch clojure.lang.ExceptionInfo _error
        :thrown))
    (q '[:find [?name ...] :where [_ :user/name ?name]] @db)])
 := [:thrown ["Ada"]])
