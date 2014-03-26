(ns clojure-ruby.core
  (:require [instaparse.core :as insta]))

(def ruby-parser
  (insta/parser (clojure.java.io/resource "ruby.ebnf")))

(defn mutating-method->real-method [meth-name]
  (subs meth-name 0 1))

(defn rewrite-assignment-mutate [var-name meth val]
  (let [real-meth (mutating-method->real-method meth)]
    [:assignment var-name
     [:method_call
      [:var_ref var-name]
      real-meth
      val]]))

(defn rewrite-bracket-assignment-mutate [obj idx meth val]
  (let [real-meth (mutating-method->real-method meth)]
    [:method_call obj "[]=" idx
     [:method_call
      [:method_call obj "[]" idx]
      real-meth
      val]]))

(defn rename-infix [obj meth arg]
  [:method_call obj meth arg])

(defn rename-bracket [obj idx]
  [:method_call obj "[]" idx])

(defn rename-bracket-assignment [obj idx val]
  [:method_call obj "[]=" idx val])

(def cleaning-map
  {:assignment_mutate rewrite-assignment-mutate
   :method_call_bracket_assignment_mutate rewrite-bracket-assignment-mutate
   :method_call_logic rename-infix
   :method_call_relop rename-infix
   :method_call_bracket rename-bracket
   :method_call_bracket_assignment rename-bracket-assignment})

(defn clean-parse-tree [tree]
  (insta/transform cleaning-map tree))

(def variables (atom {}))

(defn host-send [obj method-sym & args]
  (if-let [meth (get-in obj [:host-methods method-sym])]
    (apply meth obj args)
    (throw (RuntimeException. (str "object " obj " does not have host method " method-sym)))))

(defn rb-send [obj method-name args]
  (if-let [meth (get-in obj [:methods method-name])]
    (apply meth obj args)
    (throw (RuntimeException. (str "Object " obj " does not have method " method-name)))))

(def global-true
  {:methods {"&&" (fn true-and [this other] other)}
   :host-methods {:boolean (fn true-host-boolean [this] true)}})

(def global-false
  {:methods {"&&" (fn false-and [this other] this)}
   :host-methods {:boolean (fn false-host-boolean [this] false)}})

(defn create-boolean [v]
  (if v global-true global-false))

(defn create-number [n]
  {:data n
   :methods {"<" (fn number-lt [this other]
                   (let [other (host-send other :number)]
                     (create-boolean (< (:data this) other))))
             "+" (fn number-plus [this other]
                   (let [other (host-send other :number)]
                     (create-number (+ (:data this) other))))
             "-" (fn number-minus [this other]
                   (let [other (host-send other :number)]
                     (create-number (- (:data this) other))))
             "==" (fn number-equal [this other]
                    (let [other (host-send other :number)]
                      (create-boolean (= (:data this) other))))
             "!=" (fn number-not-equal [this other]
                    (let [other (host-send other :number)]
                      (create-boolean (not= (:data this) other))))}
   :host-methods {:number (fn number-host-number [this] (:data this))}})

(defn create-string [s]
  {:data s
   :methods {"size" (fn string-size [this]
                      (create-number (count (:data this))))
             "[]" (fn string-bracket [this idx]
                    (let [idx (host-send idx :number)]
                      (create-string (subs (:data this) idx (inc idx)))))
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

(swap! variables assoc "Array" Array)

(def File
  {:methods {"read" (fn File-read [this name]
                      (let [name (host-send name :string)]
                        (create-string (slurp name))))}})

(swap! variables assoc "File" File)

(def STDOUT
  {:methods {"putc" (fn STDOUT-putc [this char-int]
                      (let [char-int (host-send char-int :number)]
                        (print (char char-int))))}})

(swap! variables assoc "STDOUT" STDOUT)

(declare evaluate)
(defmulti evaluate-one first)

(defmethod evaluate-one :assignment [stmt]
  (let [[_ name val] stmt
        val (evaluate val)]
    (swap! variables assoc name val)))

(defmethod evaluate-one :var_ref [stmt]
  (let [[_ name] stmt]
    (if-let [var (get @variables name)]
      var
      (throw (RuntimeException. (str "Cannot find variable [" name "]"))))))

(defmethod evaluate-one :method_call [stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate obj)
        args (map evaluate args)]
    (rb-send obj method args)))

(defmethod evaluate-one :number [stmt]
  (let [[_ val] stmt]
    (create-number (Long. val))))

(defmethod evaluate-one :string [stmt]
  (let [[_ val] stmt]
    (create-string val)))

(defmethod evaluate-one :if [stmt]
 (let [[_ & branches] stmt]
   (loop [branches branches]
     (if-let [[branch & branches] branches]
       (let [[_ predicate & body] branch]
         (if (host-send (evaluate predicate) :boolean)
           (mapv evaluate body)
           (recur branches)))))))

(defmethod evaluate-one :while [stmt]
  (let [[_ predicate & body] stmt]
    (while (host-send (evaluate predicate) :boolean)
      (mapv evaluate body))))

(defmethod evaluate-one :until [stmt]
  (let [[_ predicate & body] stmt]
    (while (not (host-send (evaluate predicate) :boolean))
      (mapv evaluate body))))

(defmethod evaluate-one :case [stmt]
  (let [[_ predicate & whens] stmt
        predicate (evaluate predicate)]
    (loop [whens whens]
      (if-let [[when & whens] (seq whens)]
        (let [[_ matcher & body] when
              matcher (evaluate matcher)]
          (if (host-send (rb-send predicate "===" [matcher]) :boolean)
            (mapv evaluate body)
            (recur whens)))))))

(defn evaluate [stmt]
  (prn stmt)
  (try
    (evaluate-one stmt)
    (catch Exception e
      (let [[start end] (insta/span stmt)]
        (throw (RuntimeException. (str "evaluation failed; [" start ", " end "]") e))))))

(defn evaluate-all [stmts]
  (when-let [[stmt & stmts] stmts]
    (evaluate stmt)
    (recur stmts)))
