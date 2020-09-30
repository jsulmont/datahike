(ns datahike.test.tuples-test
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [datahike.core :as d]
   [datahike.api :as da]
   [datahike.db :as db]))



(deftest test-schema-declaration
  (testing "composite tuple"
    (is (d/empty-db {:reg/semester+course+student {:db/valueType   :db.type/tuple
                                                   :db/tupleAttrs  [:reg/course :reg/semester :reg/student]}})))

  (testing "heterogeneous tuples"
    (is (d/empty-db  {:player/location {:db/valueType :db.type/tuple
                                        :db/tupleTypes [:db.type/long :db.type/long]}})))
  
  (testing "homogeneous tuples"
    (is (d/empty-db  {:db/tupleAttrs {:db/valueType :db.type/tuple
                                      :db/tupleType :db.type/keyword}}))))


(def reg-schema [{:db/ident       :reg/course
                  :db/valueType   :db.type/string
                  :db/cardinality :db.cardinality/one}
                 {:db/ident       :reg/semester
                  :db/valueType   :db.type/string
                  :db/cardinality :db.cardinality/one}
                 {:db/ident       :reg/student
                  :db/valueType   :db.type/string
                  :db/cardinality :db.cardinality/one}])


(deftest test-transaction
  (testing "homogeneous tuple"
    (let [_    (da/delete-database)
          _    (da/create-database)
          conn (da/connect)]
      (d/transact conn [{:db/ident       :db/reg
                         :db/valueType   :db.type/tuple
                         :db/tupleType   :db.type/keyword
                         :db/cardinality :db.cardinality/one}])
      (d/transact conn [{:db/reg [:reg/course :reg/semester :reg/student]}])))


  #_(testing "composite tuple"
      (let [_    (da/delete-database)
            db   (da/create-database)
            conn (da/connect)]
      (d/transact conn reg-schema)
      (d/transact conn [{:reg/course   "BIO-101"
                         :reg/semester "2018-fall"
                         :reg/student  "johndoe@university.edu"}])))
    )
