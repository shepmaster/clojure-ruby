(ns clojure-ruby.corelib
  (:require [clojure-ruby.messaging :as msg]))

(defn type? [obj type]
  (= type (:type obj)))

(defn host-msg [obj method-sym & args]
  (if-let [meth (get-in obj [:host-methods method-sym])]
    (apply meth obj args)
    (throw (ex-info "Host method lookup failed" {:object obj, :method method-sym}))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(def global-true
  {:methods {"&&" (fn true-and [system this other] other)}
   :host-methods {:boolean (fn true-host-boolean [this] true)}})

(def global-false
  {:methods {"&&" (fn false-and [system this other] this)}
   :host-methods {:boolean (fn false-host-boolean [this] false)}})

(defn create-boolean [v]
  (if v global-true global-false))

;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-number)

(defn number-lt [system this other]
  (let [other (host-msg other :number)]
    (create-boolean (< (:data this) other))))

(defn number-plus [system this other]
  (let [other (host-msg other :number)]
    (create-number (+ (:data this) other))))

(defn number-minus [system this other]
  (let [other (host-msg other :number)]
    (create-number (- (:data this) other))))

(defn number-equal [system this other]
  (let [other (host-msg other :number)]
    (create-boolean (= (:data this) other))))

(defn number-not-equal [system this other]
  (let [eq (msg/ruby system this "==" [other])]
    (create-boolean (not (host-msg eq :boolean)))))

(defn number-host-number [this]
  (:data this))

(defn create-number [n]
  {:data n
   :methods {"<" number-lt
             "+" number-plus
             "-" number-minus
             "==" number-equal
             "!=" number-not-equal}
   :host-methods {:number number-host-number}
   :type :number})

;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-string)

(defn string-size [system this]
  (create-number (count (:data this))))

(defn string-bracket [system this idx]
  (let [idx (host-msg idx :number)]
    (create-string (subs (:data this) idx (inc idx)))))

(defn string-equal [system this other]
  (let [other (host-msg other :string)]
    (create-boolean (= (:data this) other))))

(defn string-host-string [this]
  (:data this))

(defn create-string [s]
  {:data s
   :methods {"size" string-size
             "[]" string-bracket
             "==" string-equal
             "===" string-equal}
   :host-methods {:string string-host-string}
   :type :string})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn array-bracket [system this idx]
  (let [idx (host-msg idx :number)]
    (nth @(:data this) idx)))

(defn array-bracket-assign [system this idx val]
  (let [idx (host-msg idx :number)]
    (swap! (:data this) assoc idx val)))

(defn create-array [cnt val]
  {:data (atom (vec (repeat cnt val)))
   :methods {"[]" array-bracket
             "[]=" array-bracket-assign}})

(defn Array-new [system this cnt val]
  (let [cnt (host-msg cnt :number)]
    (create-array cnt val)))

(def Array
  {:methods {"new" Array-new}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn File-read [system this name]
  (let [name (host-msg name :string)]
    (create-string (slurp name))))

(def File
  {:methods {"read" File-read}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn STDOUT-putc [system this obj]
  (let [s (if (type? obj :string)
            (host-msg obj :string)
            (char (host-msg obj :number)))]
    (print s)))

(def STDOUT
  {:methods {"putc" STDOUT-putc}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn as-host-boolean [obj]
  (host-msg obj :boolean))

(def global-variables
  {"Array" Array
   "File" File
   "STDOUT" STDOUT})
