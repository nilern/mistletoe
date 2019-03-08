(ns mistletoe.dom
  (:require [clojure.string :as s]
            [goog.object :as obj]
            [mistletoe.signal :as sgn]
            [mistletoe.signal.util :refer [alloc-watch-key]]))

;;;; # DOM Node Watch Lifecycle

(defprotocol Watcher
  "An object that watches [[IWatchable]]:s only when it is active (e.g. mounted to the DOM)."
  (add-watchee [self watchee k f] "Add a `watchee` with key `k` and callback `f` to `self`,
                                  but don't [[add-watch]] to `watchee` yet.")
  (remove-watchee [self watchee k] "Remove the `watchee` with key `k` from `self`.")
  (activate-watches [self] "Activate the watches in `self` (i.e. call [[add-watch]] for each of them).")
  (deactivate-watches [self] "Deactivate the watches in `self` (i.e. call [[remove-watch]] for each of them)."))

(extend-protocol Watcher
  js/Node
  (add-watchee [self watchee k f]
    (let [watchees (.-__mistletoeWatchees self)]
      (set! (.-__mistletoeWatchees self) (update watchees watchee assoc k f))))

  (remove-watchee [self watchee k]
    (let [watchees (.-__mistletoeWatchees self)]
      (set! (.-__mistletoeWatchees self) (update watchees watchee dissoc k))))

  (activate-watches [self]
    (reduce-kv (fn [_ watchee kfs]
                 (reduce-kv add-watch watchee kfs))
               nil (.-__mistletoeWatchees self)))

  (deactivate-watches [self]
    (reduce-kv (fn [_ watchee kfs]
                 (reduce-kv (fn [watchee k _] (remove-watch watchee k))
                            watchee kfs))
               nil (.-__mistletoeWatchees self))))

(defn- detached?
  "Is `dom` not mounted to the unmanaged DOM?"
  [dom]
  (.-__mistletoeDetached dom))

(def ^:private mounted?
  "Is the node mounted to the unmanaged DOM?"
  (complement detached?))

(defn- run-children!
  "Call the side-effecting function `f` on each child of `element`."
  [f element]
  (loop [child (aget (.-childNodes element) 0)]
    (when child
      (f child)
      (recur (.-nextSibling child)))))

(defprotocol DOMMount
  "Lifecycle protocol (for i.e. for activating and deactivating signal subscriptions)."
  (mount! [node] "Called when mounting to DOM.")
  (unmount! [node] "Called when unmounting from DOM."))

(extend-protocol DOMMount
  js/Node
  (mount! [element]
    (run-children! mount! element)
    (activate-watches element)
    (set! (.-__mistletoeDetached element) false))

  (unmount! [element]
    (run-children! unmount! element)
    (deactivate-watches element)
    (set! (.-__mistletoeDetached element) true))


  default
  (mount! [_])
  (unmount! [_]))

;; TODO: Use this:
(defprotocol Drop
  "Manual destructor / resource disposal."
  (drop! [self] "Destructor; we are done with this object, release any (non-memory) resources immediately."))

;;;; # Children

(defn- scalar? [v] (or (string? v) (not (seqable? v))))

(defn insert-before!
  "A version of `Element/insertBefore` that also uses [[DOMMount]] appropriately."
  [parent child next-child]
  (.insertBefore parent child next-child)
  (when (mounted? parent)
    (mount! child)
    (activate-watches parent)))

(defn append-child!
  "A version of `Element/appendChild` that also uses [[DOMMount]] appropriately."
  [parent child]
  (.appendChild parent child)
  (when (mounted? parent)
    (mount! child)
    (activate-watches parent)))

(defn remove-child!
  "A version of `Element/removeChild` that also uses [[DOMMount]] appropriately."
  [parent child]
  (.removeChild parent child)
  (when (mounted? parent)
    (unmount! child)))

(defn replace-child!
  "A version of `Element/replaceChild` that also uses [[DOMMount]] appropriately."
  [parent new-child old-child]
  (.replaceChild parent new-child old-child)
  (when (mounted? parent)
    (unmount! old-child)
    (mount! new-child)))

(defprotocol FlattenChild
  "Producing a flat sequence of child nodes."
  (flatten-child [child] "Flatten any sequences and deref any [[IDeref]]:s in `child`."))

(extend-protocol FlattenChild
  sgn/SourceSignal
  (flatten-child [child] (flatten-child @child))

  sgn/ConstantSignal
  (flatten-child [child] (flatten-child @child))

  sgn/DerivedSignal
  (flatten-child [child] (flatten-child @child))

  default
  (flatten-child [child]
    (if (or (string? child) (not (seqable? child)))
      (list child)
      (mapcat flatten-child child))))

;; OPTIMIZE: Probably allocates and thunkifies unnecessarily:
(defn- flat-children
  "Get the children of `parent` as a flat, signal-free sequence."
  [parent]
  (flatten-child (.-__mistletoeChildArgs parent)))

(defn- append-tail! [parent tail]
  (run! (partial append-child! parent) tail))

(defn- remove-tail! [parent tail-head]
  (loop [old-child tail-head]
    (when old-child
      (let [next-old-child (.-nextSibling old-child)]
        (remove-child! parent old-child)
        (recur next-old-child)))))

(defn- rearrange-children!
  "Rearrange the children of `parent` to match the current state of its watchees."
  [parent]
  (loop [new-children (flat-children parent)
         old-child (aget (.-childNodes parent) 0)]
    (if (seq new-children)
      (if old-child
        (let [[new-child & new-children] new-children
              next-old-child (if (identical? new-child old-child)
                               (.-nextSibling old-child)
                               (do (insert-before! parent new-child old-child)
                                   old-child))]
          (recur new-children next-old-child))
        (append-tail! parent new-children))
      (when old-child
        (remove-tail! parent old-child)))))

(defprotocol Child
  "Things that can be parented by DOM nodes."
  (-init-child! [self parent] "Append `self` under `parent` and do any other required initialization
                              (i.e. set up watches)."))

(defn init-child!
  "Append `child` under `parent` and do any other required initialization (i.e. set up watches)."
  [element child]
  (-init-child! child element))

(defprotocol SignalChild
  (-init-signal-child! [initial-value child parent] "Implementation of [[-init-child!]] for a signal child."))

(extend-protocol SignalChild
  js/Node
  (-init-signal-child! [v sgn parent]
    (let [child v]
      (add-watchee parent sgn (alloc-watch-key)
                   (fn [_ _ old-child new-child]
                     (replace-child! parent new-child old-child)))
      (append-child! parent child)))

  default
  (-init-signal-child! [v sgn parent]
    (if (scalar? v)
      (let [child (.createTextNode js/document (str v))]
        (set! (.-__mistletoeDetached child) true)
        (add-watchee child sgn (alloc-watch-key)
                     (fn [_ _ _ v] (set! (.-nodeValue child) (str v))))
        (append-child! parent child))

      (do (add-watchee parent sgn (alloc-watch-key)
                       ;; OPTIMIZE: Rearranges all children every time:
                       (fn [_ _ _ _] (rearrange-children! parent)))
          (run! (partial append-child! parent) v)))))

(extend-protocol Child
  js/Node
  (-init-child! [child parent] (append-child! parent child))

  sgn/SourceSignal
  (-init-child! [child parent] (-init-signal-child! @child child parent))

  sgn/ConstantSignal
  (-init-child! [child parent] (-init-signal-child! @child child parent))

  sgn/DerivedSignal
  (-init-child! [child parent] (-init-signal-child! @child child parent))

  default
  (-init-child! [child parent]
    (if (scalar? child)
      (.appendChild parent (.createTextNode js/document (str child)))
      (run! #(-init-child! % parent) child))))

;;;; # Attributes

(defprotocol AttributeValue
  (-init-attr! [self name element] "Set initial attribute value and eny other setup (i.e. set up watches),
                                   dispatching on the initial value.")
  (-init-style-attr! [self name element] "Set initial style value and eny other setup (i.e. set up watches),
                                         dispatching on the initial value."))

(defn- -init-signal-attr!
  "Implementation of [[-init-attr!]] for signal attribute values."
  [sgn k element]
  (.setAttribute element k @sgn)
  (add-watchee element sgn (alloc-watch-key) (fn [_ _ _ v] (.setAttribute element k v))))

(defn- -init-signal-style-attr!
  "Implementation of [[-init-style-attr!]] for signal attribute values."
  [sgn k element]
  (obj/set (.-style element) k @sgn)
  (add-watchee element sgn (alloc-watch-key) (fn [_ _ _ v] (obj/set (.-style element) k v))))

(extend-protocol AttributeValue
  default
  (-init-attr! [v k element] (.setAttribute element k v))
  (-init-style-attr! [v k element] (obj/set (.-style element) k v))

  sgn/SourceSignal
  (-init-attr! [v k element] (-init-signal-attr! v k element))
  (-init-style-attr! [v k element] (-init-signal-style-attr! v k element))

  sgn/ConstantSignal
  (-init-attr! [v k element] (-init-signal-attr! v k element))
  (-init-style-attr! [v k element] (-init-signal-style-attr! v k element))

  sgn/DerivedSignal
  (-init-attr! [v k element] (-init-signal-attr! v k element))
  (-init-style-attr! [v k element] (-init-signal-style-attr! v k element)))

(defmulti init-attr!
          "Set initial attribute value and eny other setup (i.e. set up watches)."
          (fn [_element k _v]
            (if (s/starts-with? k "on")
              :event
              k)))

(defmethod init-attr! :default [element k v]
  (-init-attr! v k element))

(defmethod init-attr! :event [element k f]
  (.addEventListener element (subs k 2) f))

(defmethod init-attr! "style" [element _ style-attrs]
  (reduce-kv (fn [element k v] (-init-style-attr! v (name k) element)) element style-attrs))

;;;; # Creating Reactive Elements

(defn el
  "Instantiate the [[INamed]] `tag` (e.g. `:div`) to a [[js/Element]]."
  [tag & args]
  (let [el (.createElement js/document (name tag))]
    (set! (.-__mistletoeDetached el) true)
    (loop [args args, child-args []]
      (if-not (empty? args)
        (let [[arg & args] args]
          (if (keyword? arg)
            (if (empty? args)
              (throw (js/Error. (str "No value for attribute " arg)))
              (let [[arg* & args] args]
                (init-attr! el (name arg) arg*)
                (recur args child-args)))
            (do (init-child! el arg)
                (recur args (conj child-args arg)))))
        (set! (.-__mistletoeChildArgs el) child-args)))
    el))
