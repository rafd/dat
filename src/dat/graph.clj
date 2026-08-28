(ns dat.graph
  (:require
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.connect.planner :as pcp]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [dat.schema :as schema]))

(defn blanks-for [ks]
  (zipmap ks (repeat nil)))

(defn expected-shape
  "What the running pathom node wants from this resolver, as `{attr {}}`.
   `join-key` is the attr the resolver returns entities under, for resolvers
   with a nested output. Nil when the resolver runs outside of a pathom plan."
  ([env]
   (-> env ::pcp/node ::pcp/expects))
  ([env join-key]
   (-> env ::pcp/node ::pcp/expects (get join-key))))

(defn requested-attrs
  "The subset of `attrs` that the running query asks for.
   Falls back to all of them when pathom tells us nothing, so that a resolver
   called outside of a plan still returns a whole entity."
  [shape attrs]
  (let [requested (when shape
                    (filterv shape attrs))]
    (if (seq requested)
      requested
      (vec attrs))))

(defn attrs-cache-key
  "Pathom keys the resolver cache on op-name+input+params; entities pulled with
   different attrs must not collide on it."
  [op-name attrs-for-env]
  (fn [env input]
    [op-name input (pco/params env) (attrs-for-env env)]))

(defn ->resolvers
  [schema]
  (->> schema
       (mapcat (fn [[entity-type attrs]]
                 (let [source-id-key (schema/id-key schema entity-type)]
                   (concat

                    [
                     ;; widgets - _ => [{widget/*}, ...]
                     (let [all-attrs (vec (schema/direct-attrs schema entity-type))
                           op-name (symbol (str "all-" entity-type))
                           attrs-for-env (fn [env]
                                           (requested-attrs (expected-shape env entity-type) all-attrs))]
                       (pco/resolver
                        op-name
                        {::pco/input []
                         ::pco/output [{entity-type all-attrs}]
                         ::pco/cache-key (attrs-cache-key op-name attrs-for-env)}
                        (fn [{::keys [q ->db conn] :as env} _in]
                          (let [additional-where (:where (pco/params env))
                                attrs (attrs-for-env env)
                                blank (blanks-for attrs)]
                            {entity-type
                             (->> (q (concat [:find [(list 'pull '?e attrs) '...]
                                              :where
                                              ['?e source-id-key '_]]
                                             additional-where)
                                     (->db conn))
                                  (map (fn [e]
                                         (merge blank e))))}))))
                     ]

                    ;; (unique-key)->widget => {widget/*}
                    (->> attrs
                         (keep (fn [[attr-key opts]]
                                 (when (:dat/unique opts)
                                   (let [all-attrs (vec (schema/direct-attrs schema entity-type))
                                         op-name (symbol (str attr-key "->" entity-type))
                                         attrs-for-env (fn [env]
                                                         (requested-attrs (expected-shape env) all-attrs))]
                                     (pco/resolver
                                      op-name
                                      {::pco/input [attr-key]
                                       ::pco/output all-attrs
                                       ::pco/cache-key (attrs-cache-key op-name attrs-for-env)}
                                      (fn [{::keys [q ->db conn] :as env} in]
                                        ;; TODO could access indexes directly?
                                        (let [attrs (attrs-for-env env)]
                                          (merge
                                           (blanks-for attrs)
                                           (q [:find (list 'pull '?e attrs) '.
                                               :in '$ '?value
                                               :where
                                               ['?e attr-key '?value]]
                                              (->db conn)
                                              (attr-key in)))))))))))

                    ;; widget-sprocket - widget-id => [sprocket/id, ...]
                    ;; sprocket-widgets - sprocket-id => [widget/id, ...]
                    (->> attrs
                         (keep (fn [[attr-key opts]]
                                 (when-let [[cardinality target-entity-type target-id-key] (:dat/rel opts)]
                                   (let []
                                     [;; normal direction: widget - sprocket(s)
                                      (let [in-key source-id-key
                                            out-key attr-key
                                            out-ids-key target-id-key
                                            count-key (keyword
                                                       (namespace attr-key)
                                                       (str (name attr-key) "-count"))]
                                        (case cardinality
                                          :dat.rel/one
                                          (pco/resolver
                                           (symbol (str entity-type "[" attr-key "]->" target-entity-type))
                                           {::pco/input [in-key]
                                            ::pco/output [{out-key
                                                           [target-id-key]}
                                                          out-ids-key]}
                                           (fn [{::keys [q ->db conn]} in]
                                             (let [id (q [:find '?target-id '.
                                                          :in '$ '?id
                                                          :where
                                                          ['?e source-id-key '?id]
                                                          ['?e attr-key '?target-e]
                                                          ['?target-e target-id-key '?target-id]]
                                                         (->db conn)
                                                         (in-key in))]
                                               {out-ids-key id
                                                out-key (when id
                                                          {target-id-key id})})))

                                          :dat.rel/many
                                          (pco/resolver
                                           (symbol (str entity-type "[" attr-key "]->" target-entity-type "+"))
                                           {::pco/input [in-key]
                                            ::pco/output [{out-key
                                                           [target-id-key]}
                                                          out-ids-key
                                                          count-key]}
                                           (fn [{::keys [q ->db conn]} in]
                                             (let [ids (q [:find ['?target-id '...]
                                                           :in '$ '?id
                                                           :where
                                                           ['?e source-id-key '?id]
                                                           ['?e attr-key '?target-e]
                                                           ['?target-e target-id-key '?target-id]]
                                                          (->db conn)
                                                          (in-key in))]
                                               {out-ids-key ids
                                                out-key (mapv (fn [x]
                                                                {target-id-key x})
                                                              ids)
                                                count-key (count ids)})))))

                                      ;; reverse direction
                                      (let [in-key target-id-key
                                            ;; datomic _underscore naming convention
                                            out-key (keyword
                                                     (namespace attr-key)
                                                     (str "_" (name attr-key)))
                                            count-key (keyword
                                                       (namespace attr-key)
                                                       (str "_" (name attr-key) "-count"))]
                                        (pco/resolver
                                         (symbol (str target-entity-type
                                                      "[" out-key "]"
                                                      "->"
                                                      entity-type
                                                      ;; always many on the reverse
                                                      "+"))
                                         {::pco/input [in-key]
                                          ::pco/output [{out-key
                                                         [source-id-key]}
                                                        count-key]}
                                         (fn [{::keys [q ->db conn]} in]
                                           (let [ids (q [:find ['?id '...]
                                                         :in '$ '?target-id
                                                         :where
                                                         ['?target-e target-id-key '?target-id]
                                                         ['?e attr-key '?target-e]
                                                         ['?e source-id-key '?id]]
                                                        (->db conn)
                                                        (in-key in))]
                                             {out-key (mapv (fn [x]
                                                              {source-id-key x})
                                                            ids)
                                              count-key (count ids)}))))]))))
                         (apply concat))))))))

(defn ->dynamic-resolvers->resolvers
  [dynamic-resolvers]
  (->> dynamic-resolvers
       (map (fn [{:dat.resolver/keys [id in out f]}]
              (pco/resolver
               (symbol id)
               {::pco/input (vec in)
                ::pco/output (vec out)}
               (fn [{::keys [q ->db conn]} in]
                 (f in)))))))

(defn make-pull [schema {:keys [q pull conn ->db dynamic-resolvers]}]
  (let [env (-> (concat (->resolvers schema)
                        (->dynamic-resolvers->resolvers dynamic-resolvers))
                pci/register
                (assoc ::q q
                       ::pull pull
                       ::conn conn
                       ::->db ->db))]
    (fn pull [input output]
      (p.eql/process env input output))))
