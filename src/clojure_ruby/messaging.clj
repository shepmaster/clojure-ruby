(ns clojure-ruby.messaging)

(defn ruby [system obj method-name args]
  (if-let [meth (get-in obj [:methods method-name])]
    (apply meth system obj args)
    (throw (ex-info "Method lookup failed" {:object obj, :method method-name}))))
