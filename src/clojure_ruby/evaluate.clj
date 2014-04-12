(ns clojure-ruby.evaluate
  (:require [clojure-ruby.variables :as var]))

(declare evaluate)

(defn evaluate-body [system body]
  (mapv (partial evaluate system) body))

(defn method-lookup [obj method-name]
  (get-in obj [:methods method-name]))

(defn bind-arguments [vars arg-defns args]
  (reduce #(apply var/add-binding %1 %2) vars (map list arg-defns args)))

(defn ruby-defined-method-call [system method args]
  (let [_ (swap! (:variables system) var/push-bindings)
        _ (swap! (:variables system) bind-arguments (:arg-defs method) args)
        r (evaluate-body system (:body method))
        _ (swap! (:variables system) var/pop-bindings)]
    r))

(defn ruby-msg [system obj method-name args]
  (if-let [meth (method-lookup obj method-name)]
    (if (fn? meth)
      (apply meth system obj args)
      (ruby-defined-method-call system meth args))
    (throw (ex-info "Method lookup failed" {:object obj, :method method-name}))))

(defmulti evaluate-one (fn [vars stmt] (first stmt)))

(defmethod evaluate-one :assignment [system stmt]
  (let [[_ name val] stmt
        val (evaluate system val)]
    (swap! (:variables system) var/add-binding name val)))

(defmethod evaluate-one :reference [system stmt]
  (let [[_ name] stmt]
    (if-let [var (var/get-binding @(:variables system) name)]
      var
      (let [self (var/get-binding @(:variables system) "self")]
        (if (method-lookup self name)
          (ruby-msg system self name [])
          (throw (ex-info "Cannot find variable or method" {:name name})))))))

(defmethod evaluate-one :method-call [system stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate system obj)
        args (map (partial evaluate system) args)]
    (ruby-msg system obj method args)))

(defmethod evaluate-one :number [system stmt]
  (let [[_ val] stmt
        {:keys [create-number]} system]
    (create-number (Long. val))))

(defmethod evaluate-one :string [system stmt]
  (let [[_ val] stmt
        {:keys [create-string]} system]
    (create-string val)))

(defmethod evaluate-one :if [system stmt]
 (let [[_ & branches] stmt
       {:keys [as-host-boolean]} system]
   (loop [branches branches]
     (if-let [[branch & branches] branches]
       (let [[_ predicate & body] branch]
         (if (as-host-boolean (evaluate system predicate))
           (evaluate-body system body)
           (recur branches)))))))

(defmethod evaluate-one :while [system stmt]
  (let [[_ predicate & body] stmt
        {:keys [as-host-boolean]} system]
    (while (as-host-boolean (evaluate system predicate))
      (evaluate-body system body))))

(defmethod evaluate-one :until [system stmt]
  (let [[_ predicate & body] stmt
        {:keys [as-host-boolean]} system]
    (while (not (as-host-boolean (evaluate system predicate)))
      (evaluate-body system body))))

(defmethod evaluate-one :case [system stmt]
  (let [[_ predicate & whens] stmt
        predicate (evaluate system predicate)
        {:keys [as-host-boolean]} system]
    (loop [whens whens]
      (if-let [[when & whens] (seq whens)]
        (let [[_ matcher & body] when
              matcher (evaluate system matcher)]
          (if (as-host-boolean (ruby-msg system predicate "===" [matcher]))
            (evaluate-body system body)
            (recur whens)))))))

(defmethod evaluate-one :method-def [system stmt]
  (let [[_ name args & body] stmt]
    (swap! (:variables system) var/add-method "self" name {:arg-defs (rest args), :body body})))

(defn evaluate [system stmt]
  (try
    (evaluate-one system stmt)
    (catch Exception e
      (throw (ex-info "Evaluation failed" {:statement stmt} e)))))

(defn evaluate-all [create-string create-number as-host-boolean initial-variables stmts]
  (let [system {:variables (atom initial-variables)
                :create-string create-string
                :create-number create-number
                :as-host-boolean as-host-boolean}]
    (doseq [stmt stmts]
      (evaluate system stmt))))
