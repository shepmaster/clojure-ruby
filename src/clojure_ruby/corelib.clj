(ns clojure-ruby.corelib
  (:require [clojure-ruby.messaging :refer :all]))

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
  (let [other (host-send other :number)]
    (create-boolean (< (:data this) other))))

(defn number-plus [this other]
  (let [other (host-send other :number)]
    (create-number (+ (:data this) other))))

(defn number-minus [this other]
  (let [other (host-send other :number)]
    (create-number (- (:data this) other))))

(defn number-equal [this other]
  (let [other (host-send other :number)]
    (create-boolean (= (:data this) other))))

(defn number-not-equal [this other]
  (let [other (host-send other :number)]
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

(defn create-string [s]
  {:data s
   :methods {"size" (fn string-size [this]
                      (create-number (count (:data this))))
             "[]" (fn string-bracket [this idx]
                    (let [idx (host-send idx :number)]
                      (create-string (subs (:data this) idx (inc idx)))))
             "==" (fn string-triple-equal [this other]
                    (let [other (host-send other :string)]
                      (create-boolean (= (:data this) other))))
             "===" (fn string-triple-equal [this other]
                     (let [other (host-send other :string)]
                       (create-boolean (= (:data this) other))))}
   :host-methods {:string (fn string-host-string [this] (:data this))}})

(defn create-array [cnt val]
  {:data (atom (vec (repeat cnt val)))
   :methods {"[]" (fn array-bracket [this idx]
                    (let [idx (host-send idx :number)]
                      (nth @(:data this) idx)))
             "[]=" (fn array-bracket-assign [this idx val]
                     (let [idx (host-send idx :number)]
                       (swap! (:data this) assoc idx val)))}})

(def Array
  {:methods {"new" (fn Array-new [this cnt val]
                     (let [cnt (host-send cnt :number)]
                       (create-array cnt val)))}})

(def File
  {:methods {"read" (fn File-read [this name]
                      (let [name (host-send name :string)]
                        (create-string (slurp name))))}})

(def STDOUT
  {:methods {"putc" (fn STDOUT-putc [this char-int]
                      (let [char-int (host-send char-int :number)]
                        (print (char char-int))))}})

(defn register-global-variables [variables]
  (swap! variables assoc "Array" Array)
  (swap! variables assoc "File" File)
  (swap! variables assoc "STDOUT" STDOUT))
