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


(defn connect
  []
  (da/delete-database)
  (da/create-database)
  (da/connect))

(deftest test-transaction
  ;; TODO: how to ensure that there are only 2 to 8 elements in the vector?
  (testing "homogeneous tuple"
    (let [conn (connect)]
      (is (d/transact conn [{:db/ident       :db/reg
                             :db/valueType   :db.type/tuple
                             :db/tupleType   :db.type/keyword
                             :db/cardinality :db.cardinality/one}]))
      (d/transact conn [{:db/reg [:reg/course :reg/semester :reg/student]}])))

  (testing "heterogeneous tuple"
    (let [conn (connect)]
      (d/transact conn [{:db/ident       :db/coord
                         :db/valueType   :db.type/tuple
                         :db/tupleTypes  [:db.type/long :db.type/keyword]
                         :db/cardinality :db.cardinality/one}])
      (d/transact conn [{:db/coord [100 :coord/west]}])))

  (testing "composite tuple"
    (let [conn (connect)
          reg-schema [{:db/ident       :reg/course
                       :db/valueType   :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident       :reg/semester
                       :db/valueType   :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident       :reg/student
                       :db/valueType   :db.type/string
                       :db/cardinality :db.cardinality/one}]]
      (d/transact conn reg-schema)
      (is (d/transact conn [{:db/ident       :reg/semester+course+student
                             :db/valueType   :db.type/tuple
                             :db/tupleAttrs  [:reg/course :reg/semester :reg/student]
                             :db/cardinality :db.cardinality/one}]))
      (d/transact conn [{:reg/course   "BIO-101"
                         :reg/semester "2018-fall"
                         :reg/student  "johndoe@university.edu"}]))))


(deftest test-saving-vector
  (testing "heterogeneous"
    (let [conn (connect)]
      (d/transact conn [{:db/ident       :db/coord
                         :db/valueType   :db.type/tuple
                         :db/tupleTypes  [:db.type/long :db.type/keyword]
                         :db/cardinality :db.cardinality/one}])
      (d/transact conn [[:db/add 100 :db/coord [100 :coord/west]]])
      (is (= #{[[100 :coord/west]]}
            (d/q '[:find ?v
                   :where [_ :db/coord ?v]]
              @conn)))))
  (testing "homogeneous"
    (let [conn (connect)]
      (d/transact conn [{:db/ident       :db/coord
                         :db/valueType   :db.type/tuple
                         :db/tupleType   :db.type/long
                         :db/cardinality :db.cardinality/one}])
      (d/transact conn [[:db/add 100 :db/coord [100 200 300]]])
      (is (= #{[[100 200 300]]}
            (d/q '[:find ?v
                   :where [_ :db/coord ?v]]
              @conn))))))
