(ns dat.e2e
  "End-to-end tests: the pathom graph over a real datascript db."
  (:require
   [hyperfiddle.rcf :as rcf]
   [dat.api :as api]
   [dat.graph :as graph]))

(def e2e-schema
  {:entity/user
   {:user/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :user/name {:dat/type :db.type/string}
    :user/email {:dat/type :db.type/string}
    :user/bio {:dat/type :db.type/string}}
   :entity/post
   {:post/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :post/title {:dat/type :db.type/string}
    :post/body {:dat/type :db.type/string}
    :post/user {:dat/rel [:dat.rel/one :entity/user :user/id]}}
   :entity/comment
   {:comment/id {:dat/type :db.type/uuid
                 :dat/unique :dat.unique/identity}
    :comment/text {:dat/type :db.type/string}
    :comment/user {:dat/rel [:dat.rel/one :entity/user :user/id]}}})

(def ada-id #uuid "00000000-0000-0000-0000-0000000000a1")
(def first-post-id #uuid "00000000-0000-0000-0000-0000000000b1")
(def second-post-id #uuid "00000000-0000-0000-0000-0000000000b2")
(def comment-id #uuid "00000000-0000-0000-0000-0000000000c1")

(defn make-graph!
  "A fresh db holding one user with two posts and one comment.
   Returns the pathom pull fn, plus an atom of the datalog queries it issues."
  []
  (let [db (api/init! :dat.db/datascript e2e-schema {})
        queries (atom [])]
    (api/transact! db
                   [{:user/id ada-id
                     :user/name "Ada"
                     :user/email "ada@example.com"
                     :user/bio "Analyst"}
                    {:post/id first-post-id
                     :post/title "T1"
                     :post/body "B1"
                     :post/user [:user/id ada-id]}
                    {:post/id second-post-id
                     :post/title "T2"
                     :post/body "B2"
                     :post/user [:user/id ada-id]}
                    {:comment/id comment-id
                     :comment/text "C1"
                     :comment/user [:user/id ada-id]}])
    {:queries queries
     :graph-pull (graph/make-pull e2e-schema
                                  {:q (fn [query derefed-db & args]
                                        (swap! queries conj query)
                                        (apply api/q query derefed-db args))
                                   :pull api/pull
                                   :conn db
                                   :->db deref})}))

(defn find-clause [query]
  (->> query
       (drop-while (fn [form]
                     (not= :find form)))
       second))

(defn pull-specs
  "The pull patterns of the datalog queries issued, in order.
   Queries that find ids rather than entities are left out."
  [queries]
  (->> queries
       (keep (fn [query]
               (let [found (find-clause query)]
                 (cond
                   ;; [:find (pull ?e attrs) .]
                   (seq? found)
                   (nth found 2)
                   ;; [:find [(pull ?e attrs) ...]]
                   (and (vector? found)
                        (seq? (first found)))
                   (nth (first found) 2)))))))

(rcf/tests
 "resolvers pull only the attrs that the query asks for"

 (let [{:keys [graph-pull queries]} (make-graph!)
       result (graph-pull {} [{:entity/user [:user/name]}])]
   [result (pull-specs @queries)])
 := [{:entity/user [{:user/name "Ada"}]}
     [[:user/name]]]

 ;; the related posts are pulled one at a time, and :user/id is only pulled
 ;; because the reverse-rel resolver takes it as input
 (let [{:keys [graph-pull queries]} (make-graph!)
       result (graph-pull {} [{:entity/user [:user/name
                                             {:post/_user [:post/title]}]}])]
   [(->> result :entity/user first :post/_user (map :post/title) set)
    (pull-specs @queries)])
 := [#{"T1" "T2"}
     [[:user/id :user/name] [:post/title] [:post/title]]]

 "a count does not pull the related entities at all"

 (let [{:keys [graph-pull queries]} (make-graph!)
       result (graph-pull {} [{:entity/user [:user/name :post/_user-count]}])]
   [result (pull-specs @queries)])
 := [{:entity/user [{:user/name "Ada"
                     :post/_user-count 2}]}
     [[:user/id :user/name]]]

 "an entity reached twice with different attrs is not served the narrower cached pull"

 (let [{:keys [graph-pull]} (make-graph!)]
   (graph-pull {} [{:entity/post [{:post/user [:user/name]}]}
                   {:entity/comment [{:comment/user [:user/email]}]}]))
 := {:entity/post [{:post/user {:user/name "Ada"}}
                   {:post/user {:user/name "Ada"}}]
     :entity/comment [{:comment/user {:user/email "ada@example.com"}}]})
