(ns dat.graph
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [dat.schema :as schema]))

(defn blanks-for [ks]
  (zipmap ks (repeat nil)))

(defn ->resolvers
  [schema]
  (->> schema
       (mapcat (fn [[entity-type attrs]]
                 (let [source-id-key (schema/id-key schema entity-type)]
                   (concat

                    [
                     ;; widgets - _ => [{widget/*}, ...]
                     (let [attrs (schema/direct-attrs schema entity-type)]
                       (pco/resolver
                        (symbol (str "all-" entity-type))
                        {::pco/input []
                         ::pco/output [{entity-type (vec attrs)}]}
                        (fn [{::keys [q ->db conn] :as env} _in]
                          (let [additional-where (:where (pco/params env))]
                            {entity-type
                             ;; TODO could pull *just the attributes that are asked for
                             (let [blank (blanks-for attrs)]
                               (->> (q (concat [:find [(list 'pull '?e attrs) '...]
                                                :where
                                                ['?e source-id-key '_]]
                                               additional-where)
                                       (->db conn))
                                    (map (fn [e]
                                           (merge blank e)))))}))))
                     ]

                    ;; (unique-key)->widget => {widget/*}
                    (->> attrs
                         (keep (fn [[attr-key opts]]
                                 (when (:dat/unique opts)
                                   (let [attrs (schema/direct-attrs schema entity-type)]
                                     (pco/resolver
                                      (symbol (str attr-key "->" entity-type))
                                      {::pco/input [attr-key]
                                       ::pco/output (vec attrs)}
                                      (fn [{::keys [q ->db conn]} in]
                                        ;; TODO could pull *just* the attributes that are asked for?
                                        ;; TODO could access indexes directly?
                                        (merge
                                         (blanks-for attrs)
                                         (q [:find (list 'pull '?e attrs) '.
                                             :in '$ '?value
                                             :where
                                             ['?e attr-key '?value]]
                                            (->db conn)
                                            (attr-key in))))))))))

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
