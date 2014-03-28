(ns clojure-ruby.messaging)

(defn type? [obj type]
  (= type (:type obj)))

(defn host [obj method-sym & args]
  (if-let [meth (get-in obj [:host-methods method-sym])]
    (apply meth obj args)
    (throw (ex-info "Host method lookup failed" {:object obj, :method method-sym}))))

(defn ruby [obj method-name args]
  (if-let [meth (get-in obj [:methods method-name])]
    (apply meth obj args)
    (throw (ex-info "Method lookup failed" {:object obj, :method method-name}))))
