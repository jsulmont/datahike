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
  (da/create-database {:schema-flexibility :write})
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


(deftest test-transacting
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


(deftest test-transacting-composite
  (testing ""
    (let [conn (connect)]
      (d/transact conn [{:db/ident       :a
                         :db/valueType   :db.type/long
                         :db/cardinality :db.cardinality/one}
                        ;; {:db/ident       :test/b}
                        {:db/ident       :a+b+c
                         :db/valueType   :db.type/tuple
                         :db/tupleAttrs  [:a :b :c]
                         :db/cardinality :db.cardinality/one}])
      (is (d/transact conn [[:db/add 100 :a 123]]))
      (is (= #{[123]}
            (d/q '[:find ?v
                   :where [100 :a ?v]]
              @conn)))
      (is (= #{[100 [123 nil nil]]}
            (d/q '[:find ?e ?v
                   :where [?e :a+b+c ?v]]
              @conn)))
      ;; TODO: weird, when we specify the 'e' it does not work where the previous query shows that it should work.
      #_(is (= #{[[123 nil nil]]}
            (d/q '[:find ?v
                   :where [100 :a+b+c ?v]]
              @conn)))
      )))

(defn all-datoms [db eid]
  (into #{} (map (juxt :e :a :v)) (d/datoms db :eavt eid)))


(deftest test-tx
  (let [conn (connect)
        e    100]
    (d/transact conn [{:db/ident :a
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :b
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :c
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :d
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :a+b
                       :db/valueType :db.type/tuple
                       :db/tupleAttrs [:a :b]
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :a+c+d
                       :db/valueType :db.type/tuple
                       :db/tupleAttrs [:a :c :d]
                       :db/cardinality :db.cardinality/one}])
    (are [tx datoms] (= datoms (all-datoms (:db-after (d/transact! conn tx)) e))
      [[:db/add e :a "a"]]
      #{[e :a     "a"]
        [e :a+b   ["a" nil]]
        [e :a+c+d ["a" nil nil]]}

      [[:db/add e :b "b"]]
      #{[e :a     "a"]
        [e :b     "b"]
        [e :a+b   ["a" "b"]]
        [e :a+c+d ["a" nil nil]]}

      [[:db/add e :a "A"]]
      #{[e :a     "A"]
        [e :b     "b"]
        [e :a+b   ["A" "b"]]
        [e :a+c+d ["A" nil nil]]}

      [[:db/add e :c "c"]
       [:db/add e :d "d"]]
      #{[e :a     "A"]
        [e :b     "b"]
        [e :a+b   ["A" "b"]]
        [e :c     "c"]
        [e :d     "d"]
        [e :a+c+d ["A" "c" "d"]]}

      [[:db/add e :a "a"]]
      #{[e :a     "a"]
        [e :b     "b"]
        [e :a+b   ["a" "b"]]
        [e :c     "c"]
        [e :d     "d"]
        [e :a+c+d ["a" "c" "d"]]}

      [[:db/add e :a "A"]
       [:db/add e :b "B"]
       [:db/add e :c "C"]
       [:db/add e :d "D"]]
      #{[e :a     "A"]
        [e :b     "B"]
        [e :a+b   ["A" "B"]]
        [e :c     "C"]
        [e :d     "D"]
        [e :a+c+d ["A" "C" "D"]]}

      [[:db/retract e :a "A"]]
      #{[e :b     "B"]
        [e :a+b   [nil "B"]]
        [e :c     "C"]
        [e :d     "D"]
        [e :a+c+d [nil "C" "D"]]}

      [[:db/retract e :b "B"]]
      #{[e :c     "C"]
        [e :d     "D"]
        [e :a+c+d [nil "C" "D"]]}
      )

    ;; TODO: Fix this test
    #_(is (thrown-msg? "Can’t modify tuple attrs directly: [:db/add 1 :a+b [\"A\" \"B\"]]"
          (d/transact! conn [{:db/id 1 :a+b ["A" "B"]}])))))


(deftest test-queries
  (let [conn (connect)]
    (d/transact conn [{:db/ident :a
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :b
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :a+b
                       :db/valueType :db.type/tuple
                       :db/tupleAttrs [:a :b]
                       :db/cardinality :db.cardinality/one}])

    (d/transact conn [{:db/id 1 :a "A" :b "B"}
                      {:db/id 2 :a "A" :b "b"}
                      {:db/id 3 :a "a" :b "B"}
                      {:db/id 4 :a "a" :b "b"}])

    ;; TODO: fix this test
    #_(is (= #{[3]}
          (d/q '[:find ?e
                 :where [?e :a+b ["a" "B"]]] @conn)))

    ;; TODO: fix this test
    ;; (is (= #{[["a" "B"]]}
    ;;       (d/q '[:find ?a+b
    ;;              :where [[:a+b ["a" "B"]] :a+b ?a+b]] db)))

    (is (= #{[["A" "B"]] [["A" "b"]] [["a" "B"]] [["a" "b"]]}
          (d/q '[:find ?a+b
                 :where [?e :a ?a]
                 [?e :b ?b]
                 [(tuple ?a ?b) ?a+b]] @conn)))

    (is (= #{["A" "B"] ["A" "b"] ["a" "B"] ["a" "b"]}
          (d/q '[:find ?a ?b
                 :where [?e :a+b ?a+b]
                 [(untuple ?a+b) [?a ?b]]] @conn)))))
