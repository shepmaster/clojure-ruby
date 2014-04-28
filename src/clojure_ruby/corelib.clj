(ns clojure-ruby.corelib
  (:require [clojure-ruby.variables :as var])
  (:require [clojure-ruby.evaluate :as eval]))

(defn type? [obj type]
  (= type (:type obj)))

(defn host-msg [obj method-sym & args]
  (if-let [meth (get-in obj [:host-methods method-sym])]
    (apply meth obj args)
    (throw (ex-info "Host method lookup failed" {:object obj, :method method-sym}))))

(defmacro def-rbfn
  "Creates a Ruby method. The special values `system` and `this` will be
  automatically available."
  [name args & body]
  (let [name-sym# (symbol name)]
    `(defn ~name-sym# [~'system ~'this ~@args]
       ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(def global-nil
  {:class "NilClass"})

(def NilClass)

(defn create-nil []
  global-nil)

;;;;;;;;;;;;;;;;;;;;;;;;;

(def global-true
  {:class "TrueClass"
   :host-methods {:boolean (fn true-host-boolean [this] true)}})

(def global-false
  {:class "FalseClass"
   :host-methods {:boolean (fn false-host-boolean [this] false)}})

(def-rbfn true-and [other] other)

(def TrueClass
  {:instance-methods {"&&" true-and}})

(def-rbfn false-and [other] this)

(def FalseClass
  {:instance-methods {"&&" false-and}})

(defn create-boolean [v]
  (if v global-true global-false))

;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-number)

(def-rbfn number-lt [other]
  (let [other (host-msg other :number)]
    (create-boolean (< (:data this) other))))

(def-rbfn number-plus [other]
  (let [other (host-msg other :number)]
    (create-number (+ (:data this) other))))

(def-rbfn number-minus [other]
  (let [other (host-msg other :number)]
    (create-number (- (:data this) other))))

(def-rbfn number-equal [other]
  (let [other (host-msg other :number)]
    (create-boolean (= (:data this) other))))

(def-rbfn number-not-equal [other]
  (let [eq (eval/ruby-msg system this "==" [other])]
    (create-boolean (not (host-msg eq :boolean)))))

(defn number-host-number [this]
  (:data this))

(defn create-number [n]
  {:data n
   :class "RubyNumber"
   :host-methods {:number number-host-number}
   :type :number})

(def RubyNumber
  {:instance-methods {"<" number-lt
                      "+" number-plus
                      "-" number-minus
                      "==" number-equal
                      "!=" number-not-equal}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-string)

(def-rbfn string-size []
  (create-number (count (:data this))))

(def-rbfn string-bracket [idx]
  (let [idx (host-msg idx :number)]
    (create-string (subs (:data this) idx (inc idx)))))

(def-rbfn string-equal [other]
  (let [other (host-msg other :string)]
    (create-boolean (= (:data this) other))))

(defn string-host-string [this]
  (:data this))

(defn create-string [s]
  {:data s
   :class "RubyString"
   :host-methods {:string string-host-string}
   :type :string})

(def RubyString
  {:instance-methods {"size" string-size
                      "[]" string-bracket
                      "==" string-equal
                      "===" string-equal}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(def-rbfn array-bracket [idx]
  (let [idx (host-msg idx :number)]
    (nth @(:data this) idx)))

(def-rbfn array-bracket-assign [idx val]
  (let [idx (host-msg idx :number)]
    (swap! (:data this) assoc idx val)))

(defn create-array [cnt val]
  {:data (atom (vec (repeat cnt val)))
   :class "Array"})

(def-rbfn Array-new [cnt val]
  (let [cnt (host-msg cnt :number)]
    (create-array cnt val)))

(def Array
  {:methods {"new" Array-new}
   :instance-methods {"[]" array-bracket
                      "[]=" array-bracket-assign}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(def-rbfn File-read [name]
  (let [name (host-msg name :string)]
    (create-string (slurp name))))

(def File
  {:methods {"read" File-read}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(def-rbfn STDOUT-putc [obj]
  (let [s (if (type? obj :string)
            (host-msg obj :string)
            (char (host-msg obj :number)))]
    (print s)))

(def STDOUT
  {:class "RubyIO"
   :methods {"putc" STDOUT-putc}})

(def RubyIO
  {})

;;;;;;;;;;;;;;;;;;;;;;;;;

(def RubyObject
  {})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn as-host-boolean [obj]
  (host-msg obj :boolean))

(def global-variables
  (-> (var/create-vars)
      (var/add-binding "RubyObject" RubyObject)
      (var/add-binding "NilClass" TrueClass)
      (var/add-binding "TrueClass" TrueClass)
      (var/add-binding "FalseClass" FalseClass)
      (var/add-binding "RubyNumber" RubyNumber)
      (var/add-binding "RubyString" RubyString)
      (var/add-binding "Array" Array)
      (var/add-binding "File" File)
      (var/add-binding "RubyIO" RubyIO)
      (var/add-binding "STDOUT" STDOUT)))
