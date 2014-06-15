(ns clojure-ruby.evaluate
  (:refer-clojure :exclude [loop])
  (:require [clojure-ruby.variables :as var]
            [clojure.core.typed :as t :refer [ann ann-form defalias declare-names loop]]
            [camel-snake-kebab :refer :all]))

;; :-(
(defmacro p [type]
  (let [m-name (-> type
                   ->kebab-case
                   (str "?")
                   symbol)]
    `(ann ~m-name (predicate ~type))
    `(def ~m-name (t/pred ~type))))

;; ugly; needed by vars?
(declare Value ClassName MethodName RubyMethod)

(defalias Vars t/Any)
(defalias VarName String)

(ann ^:no-check clojure-ruby.variables/get-binding [Vars VarName -> Value])
(ann ^:no-check clojure-ruby.variables/add-binding [Vars VarName Value -> Vars])
(ann ^:no-check clojure-ruby.variables/push-bindings [Vars -> Vars])
(ann ^:no-check clojure-ruby.variables/pop-bindings [Vars -> Vars])
(ann ^:no-check clojure-ruby.variables/add-method [Vars ClassName MethodName RubyMethod -> Vars])

;; Parser stuff
(defalias MethodName String)
(defalias ClassName String)
(defalias Statement
  (t/Rec [Statement]
         (t/U
          '[':assignment VarName Statement]
          '[':class-def VarName Statement *]
          '[':if
            '[':if-branch Statement Statement *]
            '[':if-branch Statement Statement *] *]
          '[':method-call Statement MethodName Statement *]
          '[':number String]
          '[':reference VarName]
          '[':string String]
          '[':while Statement Statement *]
          '[':until Statement Statement *]
          '[':case Statement
            '[':when Statement Statement *]
            '[':when Statement Statement *] *]
          '[':method-def MethodName '[':method-def-args VarName *] Statement *]
          '[':class-def ClassName Statement *]
          )))
(defalias IfBranchStmt '[':if-branch Statement Statement *])
(defalias WhenStmt '[':when Statement Statement *])

(defalias Statements (t/Coll Statement))

(defalias VarProv '{:variables (t/Atom1 Vars)})
(defalias PrimProv '{:create-nil [-> Value]
                     :create-string  [String -> Value]
                     :create-number  [Long -> Value]
                     :as-host-boolean  [Value -> Boolean]})
(defalias RubySystem (t/I VarProv PrimProv))
(defalias Args (t/Coll Value))
(defalias RubyMethod '{:body (t/Option Statements), :arg-defs (t/Option (t/Coll String))})

(defalias HostMethod [RubySystem Value Value * -> Value])
(defalias Method (t/U RubyMethod HostMethod))
(defalias Value (t/HMap :mandatory {:class ClassName}
                        :optional {:methods (t/Map MethodName Method)
                                   :instance-methods (t/Map MethodName Method)}))

(defalias ArgTuple '[VarName Value])


(ann evaluate [RubySystem Statement -> Value])
(declare evaluate)

(ann evaluate-body [RubySystem (t/Option Statements) -> Value])
(defn evaluate-body [system body]
  (let [{:keys [create-nil]} system]
    (if (seq body)
      (last (mapv (partial evaluate system) body))
      (create-nil))))

;; get-in no good
(ann ^:no-check method-lookup [RubySystem Value MethodName -> (t/Option Method)])
(defn method-lookup [system obj method-name]
  (or (get-in obj [:methods method-name])
      (let [clz (var/get-binding @(:variables system) (:class obj))]
        (get-in clz [:instance-methods method-name]))))

(ann name-arg-pairs [(t/Option (t/Coll VarName)) (t/Coll Value) -> (t/Coll ArgTuple)])
(defn name-arg-pairs [arg-defns args]
  (map (t/fn> [a :- VarName, b :- Value] [a b]) arg-defns args))

(ann bind-arguments [Vars (t/Option (t/Coll VarName)) Args -> Vars])
(defn bind-arguments [vars arg-defns args]
  (reduce (t/fn> [vs :- Vars
                  [nm v] :- ArgTuple]
                 (var/add-binding vs nm v))
          vars
          (name-arg-pairs arg-defns args)))

(ann evaluate-host-defined-method-call [RubySystem HostMethod Value Args -> Value])
(defn evaluate-host-defined-method-call [system method obj args]
  (apply method system obj args))

(ann evaluate-ruby-defined-method-call [RubySystem RubyMethod Value Args -> Value])
(defn evaluate-ruby-defined-method-call [system method obj args]
  (let [_ (swap! (:variables system) var/push-bindings)
        _ (swap! (:variables system) bind-arguments (:arg-defs method) args)
        _ (swap! (:variables system) var/add-binding "self" obj)
        r (evaluate-body system (:body method))
        _ (swap! (:variables system) var/pop-bindings)]
    r))

(p RubyMethod)

(ann evaluate-ruby-msg [RubySystem Value MethodName Args -> Value])
(defn evaluate-ruby-msg [system obj method-name args]
  (if-let [meth (method-lookup system obj method-name)]
    (if (ruby-method? meth)
      (evaluate-ruby-defined-method-call system meth obj args)
      (evaluate-host-defined-method-call system meth obj args))
    (throw (ex-info "Method lookup failed" {:object obj, :method method-name}))))

(defmulti evaluate-one (fn [system stmt] (first stmt)))

(defmethod evaluate-one :assignment [system stmt]
  (let [[_ name val] stmt
        val (evaluate system val)]
    (swap! (:variables system) var/add-binding name val)
    val))

(ann get-self [RubySystem -> Value])
(defn get-self [system]
  (var/get-binding @(:variables system) "self"))

(ann set-self [RubySystem Value -> t/Any])
(defn set-self [system obj]
  (swap! (:variables system) var/add-binding "self" obj))

(defmethod evaluate-one :reference [system stmt]
  (let [[_ name] stmt]
    (if-let [var (var/get-binding @(:variables system) name)]
      var
      (let [self (get-self system)]
        (if (method-lookup system self name)
          (evaluate-ruby-msg system self name [])
          (throw (ex-info "Cannot find variable or method" {:name name})))))))

(defmethod evaluate-one :method-call [system stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate system obj)
        args (map (partial evaluate system) args)]
    (evaluate-ruby-msg system obj method args)))

(defmethod evaluate-one :number [system stmt]
  (let [[_ ^String val] stmt
        {:keys [create-number]} system]
    (create-number (Long. val))))

(defmethod evaluate-one :string [system stmt]
  (let [[_ val] stmt
        {:keys [create-string]} system]
    (create-string val)))

(defmethod evaluate-one :if [system stmt]
  (let [[_ & branches] stmt
        {:keys [as-host-boolean create-nil]} system]
    (loop [branches1 :- (t/Option (t/Coll IfBranchStmt)) branches]
      (if-let [[branch & branches2] (seq branches1)]
        (let [[_ predicate & body] branch]
          (if (as-host-boolean (evaluate system predicate))
            (evaluate-body system body)
            (recur branches2)))
        (create-nil)))))

(defmethod evaluate-one :while [system stmt]
  (let [[_ predicate & body] stmt
        {:keys [as-host-boolean create-nil]} system]
    (while (as-host-boolean (evaluate system predicate))
      (evaluate-body system body))
    (create-nil)))

(defmethod evaluate-one :until [system stmt]
  (let [[_ predicate & body] stmt
        {:keys [as-host-boolean create-nil]} system]
    (while (not (as-host-boolean (evaluate system predicate)))
      (evaluate-body system body))
    (create-nil)))

(defmethod evaluate-one :case [system stmt]
  (let [[_ predicate & whens] stmt
        predicate (evaluate system predicate)
        {:keys [as-host-boolean create-nil]} system]
    (loop [whens :- (t/Option (t/Coll WhenStmt)) whens]
      (if-let [[when & whens] (seq whens)]
        (let [[_ matcher & body] when
              matcher (evaluate system matcher)]
          (if (as-host-boolean (evaluate-ruby-msg system predicate "===" [matcher]))
            (evaluate-body system body)
            (recur whens)))
        (create-nil)))))

(defmethod evaluate-one :method-def [system stmt]
  (let [[_ name args & body] stmt
        {:keys [as-host-boolean create-nil]} system]
    (let [clz-name (-> system get-self :class)]
      (swap! (:variables system)
             ;; HAX: rest should be like nthnext
             var/add-method clz-name name {:arg-defs (nthnext args 1) :body body}))
    (create-nil)))

(ann ^:no-check build-class [ClassName -> Value])
(defn build-class [cname]
  {:methods
   {"new"
    (fn [system this]
      {:class cname})}})

(defmethod evaluate-one :class-def [system stmt]
  (let [[_ name & body] stmt]
    (let [_ (when-not (var/get-binding @(:variables system) name)
              (swap! (:variables system) var/add-binding name (build-class name)))
          old-self (get-self system)
          _ (set-self system {:class name})
          r (evaluate-body system body)
          _ (set-self system old-self)]
      r)))

(defn evaluate [system stmt]
  (try
    (evaluate-one system stmt)
    (catch Exception e
      (throw (ex-info "Evaluation failed" {:statement stmt} e)))))

(ann ^:no-check create-system [t/Any t/Any t/Any t/Any t/Any -> RubySystem])
(defn create-system [create-nil create-string create-number as-host-boolean initial-variables]
  (let [variables (var/add-binding initial-variables "self" {:class "RubyObject"})]
    {:variables (atom variables)
     :create-nil create-nil
     :create-string create-string
     :create-number create-number
     :as-host-boolean as-host-boolean}))

(ann evaluate-all [RubySystem Statements -> Value])
(defn evaluate-all [system stmts]
  (evaluate-body system stmts))
