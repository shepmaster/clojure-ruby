(ns clojure-ruby.evaluate
  (:require [clojure-ruby.messaging :as msg]
            [clojure-ruby.corelib :as core]))

(declare evaluate)
(defmulti evaluate-one (fn [vars stmt] (first stmt)))

(defmethod evaluate-one :assignment [variables stmt]
  (let [[_ name val] stmt
        val (evaluate variables val)]
    (swap! variables assoc name val)))

(defmethod evaluate-one :var-ref [variables stmt]
  (let [[_ name] stmt]
    (if-let [var (get @variables name)]
      var
      (throw (ex-info "Cannot find variable" {:name name})))))

(defmethod evaluate-one :method-call [variables stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate variables obj)
        args (map (partial evaluate variables) args)]
    (msg/ruby obj method args)))

(defmethod evaluate-one :number [variables stmt]
  (let [[_ val] stmt]
    (core/create-number (Long. val))))

(defmethod evaluate-one :string [variables stmt]
  (let [[_ val] stmt]
    (core/create-string val)))

(defmethod evaluate-one :if [variables stmt]
 (let [[_ & branches] stmt]
   (loop [branches branches]
     (if-let [[branch & branches] branches]
       (let [[_ predicate & body] branch]
         (if (msg/host (evaluate variables predicate) :boolean)
           (mapv (partial evaluate variables) body)
           (recur branches)))))))

(defmethod evaluate-one :while [variables stmt]
  (let [[_ predicate & body] stmt]
    (while (msg/host (evaluate variables predicate) :boolean)
      (mapv (partial evaluate variables) body))))

(defmethod evaluate-one :until [variables stmt]
  (let [[_ predicate & body] stmt]
    (while (not (msg/host (evaluate variables predicate) :boolean))
      (mapv (partial evaluate variables) body))))

(defmethod evaluate-one :case [variables stmt]
  (let [[_ predicate & whens] stmt
        predicate (evaluate variables predicate)]
    (loop [whens whens]
      (if-let [[when & whens] (seq whens)]
        (let [[_ matcher & body] when
              matcher (evaluate variables matcher)]
          (if (msg/host (msg/ruby predicate "===" [matcher]) :boolean)
            (mapv (partial evaluate variables) body)
            (recur whens)))))))

(defn evaluate [variables stmt]
  (try
    (evaluate-one variables stmt)
    (catch Exception e
      (throw (ex-info "Evaluation failed" {:statement stmt} e)))))

(defn evaluate-all [stmts]
  (let [variables (atom {})]
    (core/register-global-variables variables)
    (loop [stmts stmts]
      (when-let [[stmt & stmts] stmts]
        (evaluate variables stmt)
        (recur stmts)))))
