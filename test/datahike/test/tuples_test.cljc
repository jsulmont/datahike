(ns datahike.test.tuples-test
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [datahike.core :as d]
   [datahike.db :as db]))



(deftest test-schema
  (testing "composite tuple"
    (is (d/empty-db {:reg/semester+course+student {:db/valueType   :db.type/tuple
                                                   :db/tupleAttrs  [:reg/course :reg/semester :reg/student]
                                                   :db/cardinality :db.cardinality/one
                                                   :db/unique      :db.unique/identity}}))))
