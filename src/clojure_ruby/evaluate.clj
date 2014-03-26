(ns clojure-ruby.evaluate
  (:require [clojure-ruby.messaging :refer :all]
            [clojure-ruby.corelib :refer :all]))

(declare evaluate)
(defmulti evaluate-one (fn [vars stmt] (first stmt)))

(defmethod evaluate-one :assignment [variables stmt]
  (let [[_ name val] stmt
        val (evaluate variables val)]
    (swap! variables assoc name val)))

(defmethod evaluate-one :var_ref [variables stmt]
  (let [[_ name] stmt]
    (if-let [var (get @variables name)]
      var
      (throw (ex-info "Cannot find variable" {:name name})))))

(defmethod evaluate-one :method_call [variables stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate variables obj)
        args (map (partial evaluate variables) args)]
    (rb-send obj method args)))

(defmethod evaluate-one :number [variables stmt]
  (let [[_ val] stmt]
    (create-number (Long. val))))

(defmethod evaluate-one :string [variables stmt]
  (let [[_ val] stmt]
    (create-string val)))

(defmethod evaluate-one :if [variables stmt]
 (let [[_ & branches] stmt]
   (loop [branches branches]
     (if-let [[branch & branches] branches]
       (let [[_ predicate & body] branch]
         (if (host-send (evaluate variables predicate) :boolean)
           (mapv (partial evaluate variables) body)
           (recur branches)))))))

(defmethod evaluate-one :while [variables stmt]
  (let [[_ predicate & body] stmt]
    (while (host-send (evaluate variables predicate) :boolean)
      (mapv (partial evaluate variables) body))))

(defmethod evaluate-one :until [variables stmt]
  (let [[_ predicate & body] stmt]
    (while (not (host-send (evaluate variables predicate) :boolean))
      (mapv (partial evaluate variables) body))))

(defmethod evaluate-one :case [variables stmt]
  (let [[_ predicate & whens] stmt
        predicate (evaluate variables predicate)]
    (loop [whens whens]
      (if-let [[when & whens] (seq whens)]
        (let [[_ matcher & body] when
              matcher (evaluate variables matcher)]
          (if (host-send (rb-send predicate "===" [matcher]) :boolean)
            (mapv (partial evaluate variables) body)
            (recur whens)))))))

(defn evaluate [variables stmt]
  (try
    (evaluate-one variables stmt)
    (catch Exception e
      (throw (ex-info "Evaluation failed" {:statement stmt} e)))))

(defn evaluate-all [stmts]
  (let [variables (atom {})]
    (register-global-variables variables)
    (loop [stmts stmts]
      (when-let [[stmt & stmts] stmts]
        (evaluate variables stmt)
        (recur stmts)))))
