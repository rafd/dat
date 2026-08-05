(ns dat.test-schema)

(def test-schema
  {:entity/user
   {:user/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}}
   :entity/post
   {:post/id {:dat/type :db.type/uuid
              :dat/unique :dat.unique/identity}
    :post/user {:dat/rel [:dat.rel/one :entity/user :user/id]}}})
