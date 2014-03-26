(ns clojure-ruby.corelib
  (:require [clojure-ruby.messaging :as msg]))

;;;;;;;;;;;;;;;;;;;;;;;;;

(def global-true
  {:methods {"&&" (fn true-and [this other] other)}
   :host-methods {:boolean (fn true-host-boolean [this] true)}})

(def global-false
  {:methods {"&&" (fn false-and [this other] this)}
   :host-methods {:boolean (fn false-host-boolean [this] false)}})

(defn create-boolean [v]
  (if v global-true global-false))

;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-number)

(defn number-lt [this other]
  (let [other (msg/host other :number)]
    (create-boolean (< (:data this) other))))

(defn number-plus [this other]
  (let [other (msg/host other :number)]
    (create-number (+ (:data this) other))))

(defn number-minus [this other]
  (let [other (msg/host other :number)]
    (create-number (- (:data this) other))))

(defn number-equal [this other]
  (let [other (msg/host other :number)]
    (create-boolean (= (:data this) other))))

(defn number-not-equal [this other]
  (let [other (msg/host other :number)]
    (create-boolean (not= (:data this) other))))

(defn number-host-number [this]
  (:data this))

(defn create-number [n]
  {:data n
   :methods {"<" number-lt
             "+" number-plus
             "-" number-minus
             "==" number-equal
             "!=" number-not-equal}
   :host-methods {:number number-host-number}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-string)

(defn string-size [this]
  (create-number (count (:data this))))

(defn string-bracket [this idx]
  (let [idx (msg/host idx :number)]
    (create-string (subs (:data this) idx (inc idx)))))

(defn string-equal [this other]
  (let [other (msg/host other :string)]
    (create-boolean (= (:data this) other))))

(defn string-host-string [this]
  (:data this))

(defn create-string [s]
  {:data s
   :methods {"size" string-size
             "[]" string-bracket
             "==" string-equal
             "===" string-equal}
   :host-methods {:string string-host-string}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn array-bracket [this idx]
  (let [idx (msg/host idx :number)]
    (nth @(:data this) idx)))

(defn array-bracket-assign [this idx val]
  (let [idx (msg/host idx :number)]
    (swap! (:data this) assoc idx val)))

(defn create-array [cnt val]
  {:data (atom (vec (repeat cnt val)))
   :methods {"[]" array-bracket
             "[]=" array-bracket-assign}})

(defn Array-new [this cnt val]
  (let [cnt (msg/host cnt :number)]
    (create-array cnt val)))

(def Array
  {:methods {"new" Array-new}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn File-read [this name]
  (let [name (msg/host name :string)]
    (create-string (slurp name))))

(def File
  {:methods {"read" File-read}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn STDOUT-putc [this char-int]
  (let [char-int (msg/host char-int :number)]
    (print (char char-int))))

(def STDOUT
  {:methods {"putc" STDOUT-putc}})

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn register-global-variables [variables]
  (swap! variables assoc "Array" Array)
  (swap! variables assoc "File" File)
  (swap! variables assoc "STDOUT" STDOUT))
