(ns mistletoe.dependency)

;;;; # Dependency Resolution

(defprotocol Dependency
  (-resolve [self vnode visited]))

(extend-protocol Dependency
  default
  (-resolve [self _ _] self))

(defn resolve [v vnode]
  (if-some [v (-resolve v vnode (js/Set.))]
    v
    (throw (ex-info "unresolvable VDOM dependency" {:at v}))))
