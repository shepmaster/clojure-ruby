(ns clojure-ruby.variables-test
  (:require [clojure.test :refer :all]
            [clojure-ruby.variables :refer :all]))

(deftest basic-setting-and-getting
  (is (= 5 (-> (create-vars)
               (add-binding :a 5)
               (get-binding :a)))))

(deftest adding-a-method
  (is (= :body (-> (create-vars)
                   (add-method "object" "method" :body)
                   (get-binding "object")
                   (get-in [:instance-methods "method"])))))

(deftest shadowing
  (is (= 3 (-> (create-vars)
               (add-binding :a 5)
               (push-bindings)
               (add-binding :a 3)
               (get-binding :a)))))

(deftest unshadowing
  (is (= 5 (-> (create-vars)
               (add-binding :a 5)
               (push-bindings)
               (add-binding :a 3)
               (pop-bindings)
               (get-binding :a)))))

(deftest hierarchy-lookup
  (is (= 5 (-> (create-vars)
               (add-binding :a 5)
               (push-bindings)
               (get-binding :a)))))
