(ns datahike.test.tuples-test
  (:require
   #?(:cljs [cljs.test    :as t :refer-macros [is are deftest testing]]
      :clj  [clojure.test :as t :refer        [is are deftest testing]])
   [datahike.core :as d]
   [datahike.api :as da]
   [datahike.db :as db])
  #?(:clj
      (:import [clojure.lang ExceptionInfo])))



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
  (da/delete-database) ;; deletes the 'default' db
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
      (is (d/transact conn [{:reg/course   "BIO-101"
                             :reg/semester "2018-fall"
                             :reg/student  "johndoe@university.edu"}])))))


(deftest test-transact-and-query-non-composite
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
  (let [conn (connect)]
    (d/transact conn [{:db/ident       :a
                       :db/valueType   :db.type/long
                       :db/cardinality :db.cardinality/one}
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
    (is (= #{[[123 nil nil]]}
          (d/q '[:find ?v
                 :where [100 :a+b+c ?v]]
            @conn)))))


(defn some-datoms
  [db es]
  (into #{} (map (juxt :e :a :v)) (mapcat #(d/datoms db :eavt %) es)))


(deftest test-more-composite-transaction
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
    (are [tx datoms] (= datoms (some-datoms (:db-after (d/transact! conn tx)) [e]))
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
        [e :a+c+d [nil "C" "D"]]})

    (is (thrown-with-msg? ExceptionInfo #"Can’t modify tuple attrs directly:.*"
          (d/transact! conn [{:db/id 100 :a+b ["A" "B"]}])))))


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
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/value}])

    (d/transact conn [{:db/id 1 :a "A" :b "B"}
                      {:db/id 2 :a "A" :b "b"}
                      {:db/id 3 :a "a" :b "B"}
                      {:db/id 4 :a "a" :b "b"}])

    (is (= #{[3]}
          (d/q '[:find ?e
                 :where [?e :a+b ["a" "B"]]] @conn)))

    (is (= #{[["a" "B"]]}
          (d/q '[:find ?a+b
                 :where [[:a+b ["a" "B"]] :a+b ?a+b]] @conn)))

    (is (= #{[["A" "B"]] [["A" "b"]] [["a" "B"]] [["a" "b"]]}
          (d/q '[:find ?a+b
                 :where [?e :a ?a]
                 [?e :b ?b]
                 [(tuple ?a ?b) ?a+b]] @conn)))

    (is (= #{["A" "B"] ["A" "b"] ["a" "B"] ["a" "b"]}
          (d/q '[:find ?a ?b
                 :where [?e :a+b ?a+b]
                 [(untuple ?a+b) [?a ?b]]] @conn)))))


(deftest test-lookup-refs
  (let [conn (connect)]
    (d/transact conn [{:db/ident :a
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :b
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one}
                      {:db/ident :c
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/identity}
                      {:db/ident :d
                       :db/valueType :db.type/string
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/identity}
                      {:db/ident :a+b
                       :db/valueType :db.type/tuple
                       :db/tupleAttrs [:a :b]
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/identity}])

    (d/transact! conn
      [{:db/id 100 :a "A" :b "B"}
       {:db/id 200 :a "a" :b "b"}])

    (d/transact! conn [[:db/add [:a+b ["A" "B"]] :c "C"]
                       {:db/id [:a+b ["a" "b"]] :c "c"}])
    (is (= #{[100 :a "A"]
             [100 :b "B"]
             [100 :a+b ["A" "B"]]
             [100 :c "C"]
             [200 :a "a"]
             [200 :b "b"]
             [200 :a+b ["a" "b"]]
             [200 :c "c"]}
          (some-datoms (d/db conn) [100 200])))

    (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
          (d/transact! conn [[:db/add [:a+b ["A" "B"]] :c "c"]])))

    (is (thrown-with-msg? ExceptionInfo #".*Conflicting upsert: \[\:c \"c\"] .*"
          (d/transact! conn [{:db/id [:a+b ["A" "B"]] :c "c"}])))

    ;; change tuple + upsert
    (d/transact! conn
      [{:db/id [:a+b ["A" "B"]]
        :b "b"
        :d "D"}])

    (is (= #{[100 :a "A"]
             [100 :b "b"]
             [100 :a+b ["A" "b"]]
             [100 :c "C"]
             [100 :d "D"]
             [200 :a "a"]
             [200 :b "b"]
             [200 :a+b ["a" "b"]]
             [200 :c "c"]}
          (some-datoms (d/db conn) [100 200])))

    (is (= {:db/id 200
            :a     "a"
            :b     "b"
            :a+b   ["a" "b"]
            :c     "c"}
          (d/pull (d/db conn) '[*] [:a+b ["a" "b"]])))))

(deftest test-unique
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
                       :db/cardinality :db.cardinality/one
                       :db/unique :db.unique/identity}])

    (d/transact! conn [[:db/add 100 :a "a"]])
    (d/transact! conn [[:db/add 200 :a "A"]])
    (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
          (d/transact! conn [[:db/add 100 :a "A"]])))

    (d/transact! conn [[:db/add 100 :b "b"]
                       [:db/add 200 :b "b"]
                       {:db/id 300 :a "a" :b "B"}])

    (is (= #{[100 :a "a"]
             [100 :b "b"]
             [100 :a+b ["a" "b"]]
             [200 :a "A"]
             [200 :b "b"]
             [200 :a+b ["A" "b"]]
             [300 :a "a"]
             [300 :b "B"]
             [300 :a+b ["a" "B"]]}
          (some-datoms (d/db conn) [100 200 300])))

    (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
          (d/transact! conn [[:db/add 100 :a "A"]])))
    (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
          (d/transact! conn [[:db/add 100 :b "B"]])))
    (is (thrown-with-msg? ExceptionInfo #"Cannot add .* because of unique constraint: .*"
          (d/transact! conn [[:db/add 100 :a "A"]
                             [:db/add 100 :b "B"]])))

    (testing "multiple tuple updates"
      ;; changing both tuple components in a single operation
      (d/transact! conn [{:db/id 100 :a "A" :b "B"}])
      (is (= {:db/id 100 :a "A" :b "B" :a+b ["A" "B"]}
            (d/pull (d/db conn) '[*] 100)))

      ;; adding entity with two tuple components in a single operation
      (d/transact! conn [{:db/id 4 :a "a" :b "b"}])
      (is (= {:db/id 4 :a "a" :b "b" :a+b ["a" "b"]}
            (d/pull (d/db conn) '[*] 4))))))

(deftest test-validation
  (let [db (d/empty-db {:a+b {:db/valueType :db.type/tuple
                           :db/tupleAttrs [:a :b]}})
        db1 (d/db-with db [[:db/add 100 :a "a"]])
        err-msg #"Can’t modify tuple attrs directly:.*"]
    (is (thrown-with-msg? ExceptionInfo err-msg
          (d/db-with db [[:db/add 100 :a+b [nil nil]]])))
    (is (thrown-with-msg? ExceptionInfo err-msg
          (d/db-with db1 [[:db/add 100 :a+b ["a" nil]]])))
    (is (thrown-with-msg? ExceptionInfo err-msg
          (d/db-with db [[:db/add 100 :a "a"]
                         [:db/add 100 :a+b ["a" nil]]])))
    (is (thrown-with-msg? ExceptionInfo err-msg
          (d/db-with db1 [[:db/retract 100 :a+b ["a" nil]]])))))

(deftest test-indexes
  (let [db (-> (d/empty-db {:a+b+c {:db/tupleAttrs [:a :b :c]
                                    :db/valueType :db.type/tuple
                                    :db/index true}})
             (d/db-with
               [{:db/id 1 :a "a" :b "b" :c "c"}
                {:db/id 2 :a "A" :b "b" :c "c"}
                {:db/id 3 :a "a" :b "B" :c "c"}
                {:db/id 4 :a "A" :b "B" :c "c"}
                {:db/id 5 :a "a" :b "b" :c "C"}
                {:db/id 6 :a "A" :b "b" :c "C"}
                {:db/id 7 :a "a" :b "B" :c "C"}
                {:db/id 8 :a "A" :b "B" :c "C"}]))]
    (is (= [6]
          (mapv :e (d/datoms db :avet :a+b+c ["A" "b" "C"]))))
    (is (= []
          (mapv :e (d/datoms db :avet :a+b+c ["A" "b" nil]))))
    (is (= [8 4 6 2]
          (mapv :e (d/index-range db :a+b+c ["A" "B" "C"] ["A" "b" "c"]))))
    (is (= [8 4]
          (mapv :e (d/index-range db :a+b+c ["A" "B" nil] ["A" "b" nil]))))))
