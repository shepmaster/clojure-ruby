(ns clojure-ruby.evaluate
  (:require [clojure-ruby.messaging :as msg]
            [clojure-ruby.corelib :as core]))

(declare evaluate)

(defn evaluate-body [system body]
  (mapv (partial evaluate system) body))

(defmulti evaluate-one (fn [vars stmt] (first stmt)))

(defmethod evaluate-one :assignment [system stmt]
  (let [[_ name val] stmt
        val (evaluate system val)]
    (swap! (:variables system) assoc name val)))

(defmethod evaluate-one :reference [system stmt]
  (let [[_ name] stmt]
    (if-let [var (get @(:variables system) name)]
      var
      (if-let [meth (get @(:methods system) name)]
        (evaluate-body system meth)
        (throw (ex-info "Cannot find variable or method" {:name name}))))))

(defmethod evaluate-one :method-call [system stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate system obj)
        args (map (partial evaluate system) args)]
    (msg/ruby obj method args)))

(defmethod evaluate-one :number [system stmt]
  (let [[_ val] stmt]
    (core/create-number (Long. val))))

(defmethod evaluate-one :string [system stmt]
  (let [[_ val] stmt]
    (core/create-string val)))

(defmethod evaluate-one :if [system stmt]
 (let [[_ & branches] stmt]
   (loop [branches branches]
     (if-let [[branch & branches] branches]
       (let [[_ predicate & body] branch]
         (if (msg/host (evaluate system predicate) :boolean)
           (evaluate-body system body)
           (recur branches)))))))

(defmethod evaluate-one :while [system stmt]
  (let [[_ predicate & body] stmt]
    (while (msg/host (evaluate system predicate) :boolean)
      (evaluate-body system body))))

(defmethod evaluate-one :until [system stmt]
  (let [[_ predicate & body] stmt]
    (while (not (msg/host (evaluate system predicate) :boolean))
      (evaluate-body system body))))

(defmethod evaluate-one :case [system stmt]
  (let [[_ predicate & whens] stmt
        predicate (evaluate system predicate)]
    (loop [whens whens]
      (if-let [[when & whens] (seq whens)]
        (let [[_ matcher & body] when
              matcher (evaluate system matcher)]
          (if (msg/host (msg/ruby predicate "===" [matcher]) :boolean)
            (evaluate-body system body)
            (recur whens)))))))

(defmethod evaluate-one :method-def [system stmt]
  (let [[_ name args & body] stmt]
    (swap! (:methods system) assoc name body)))

(defn evaluate [system stmt]
  (try
    (evaluate-one system stmt)
    (catch Exception e
      (throw (ex-info "Evaluation failed" {:statement stmt} e)))))

(defn evaluate-all [stmts]
  (let [system {:variables (atom {}), :methods (atom {})}]
    (core/register-global-variables (:variables system))
    (doseq [stmt stmts]
      (evaluate system stmt))))
