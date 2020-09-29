(ns datahike.test.tuples-test
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [datahike.core :as d]
   [datahike.db :as db]))



(deftest test-schema
  (testing "composite tuple"
    (is (d/empty-db {:reg/semester+course+student {:db/valueType   :db.type/tuple
                                                   :db/tupleAttrs  [:reg/course :reg/semester :reg/student]}})))

  (testing "heterogeneous tuples"
    (is (d/empty-db  {:player/location {:db/valueType :db.type/tuple
                                        :db/tupleTypes [:db.type/long :db.type/long]}})))
  
  (testing "homogeneous tuples"
    (is (d/empty-db  {:db/tupleAttrs {:db/valueType :db.type/tuple
                                      :db/tupleType :db.type/keyword}}))))
