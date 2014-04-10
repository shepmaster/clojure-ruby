(ns clojure-ruby.variables)

(defprotocol Variables
  (add-binding [this name value])
  (get-binding [this name])
  (add-method [this obj-name method-name method-body])
  (push-bindings [this])
  (pop-bindings [this]))

(defn ^:private update-top-bindings [vars f & args]
  (update-in vars [:stack]
             (fn [stack]
               (let [[top-bindings & r] stack]
                 (cons
                  (apply f top-bindings args)
                  r)))))

(defrecord StackVars [stack]
  Variables
  (add-binding [this name value]
    (update-top-bindings this assoc name value))
  (get-binding [_ name]
    (loop [stack stack]
      (if-let [[bindings & stack] stack]
        (if-let [value (get bindings name)]
          value
          (recur stack)))))
  (add-method [this obj-name method-name method-body]
    (update-top-bindings this assoc-in [obj-name :methods method-name] method-body))
  (push-bindings [this]
    (update-in this [:stack] conj {}))
  (pop-bindings [this]
    (update-in this [:stack] rest)))

(defn create-vars []
  (->StackVars '({})))
