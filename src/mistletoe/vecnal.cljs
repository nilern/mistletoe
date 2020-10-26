(ns mistletoe.vecnal ; OPTIMIZE
  (:require [clojure.core.rrb-vector :as rrb]))

(defn- insert [coll i v]
  (if (= i (count coll))
    (conj coll v)
    (rrb/catvec (rrb/subvec coll 0 i) [v] (rrb/subvec coll i))))

(defn- rrb-dissoc [coll i]
  (rrb/catvec (rrb/subvec coll 0 i) (rrb/subvec coll (inc i))))

;;;; # Seqnal

(defprotocol Vecnal
  (add-multi-watch [self k w])
  (remove-multi-watch [self k]))

(defprotocol VecnalWatcher
  (on-insert [self i v])
  (on-assoc [self i v])
  ; TODO: (on-move [self i i*])
  (on-dissoc [self i]))

;;;; # Constant

(deftype ConstantVecnal [coll]
  IDeref
  (-deref [_] coll)

  Vecnal
  (add-multi-watch [_ _ _] nil)
  (remove-multi-watch [_ _] nil))

(defn pure [coll] (ConstantVecnal. coll))

;;;; # Imux

;; OPTIMIZE: Use linear time Levenstein or at least a better heuristic:
(defn- imux-watcher [imux-vecnal]
  (fn [k signal coll coll*]
    (loop [i 0, [v :as coll] coll, [v* :as coll*] coll*]
      (if (seq coll)
        (do
          (if (seq coll*)
            (when-not (= v v*)
              (on-assoc imux-vecnal i v*))
            (on-dissoc imux-vecnal i))
          (recur (inc i) (rest coll) (rest coll*)))
        (when (seq coll*)
          (on-insert imux-vecnal i v*)
          (recur (inc i) (rest coll) (rest coll*)))))))

(deftype ImuxVecnal [signal ^:mutable watchers]
  IDeref
  (-deref [_] (-deref signal))

  Vecnal
  (add-multi-watch [self k w]
    (when (empty? watchers)
      (add-watch signal self (imux-watcher self)))
    (set! watchers (assoc watchers k w)))

  (remove-multi-watch [_ k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (remove-watch signal k)))

  VecnalWatcher
  (on-insert [_ i v*]
    (doseq [[_ w] watchers]
      (on-insert w i v*)))

  (on-assoc [_ i v*]
    (doseq [[_ w] watchers]
      (on-assoc w i v*)))

  (on-dissoc [_ i]
    (doseq [[_ w] watchers]
      (on-dissoc w i))))

(defn imux [signal] (ImuxVecnal. signal {}))

;;;; # Mux

(defn- refresh-mux [mux-signal]
  (let [coll (.-value mux-signal)
        coll* @(.-vecnal mux-signal)]
    (set! (.-value mux-signal) coll*)
    (-notify-watches mux-signal coll coll*)))

(defn- mux-watcher [mux-signal]
  (reify VecnalWatcher
    (on-insert [_ _ _] (refresh-mux mux-signal))
    (on-assoc [_ _ _] (refresh-mux mux-signal))
    (on-dissoc [_ _] (refresh-mux mux-signal))))

(deftype MuxSignal [^:mutable value vecnal ^:mutable watchers]
  IDeref
  (-deref [_]
    (when (empty? watchers)
      (set! value (vec @vecnal)))
    value)

  IWatchable
  (-add-watch [self k f]
    (when (empty? watchers)
      (add-multi-watch vecnal self (mux-watcher self)))
    (set! watchers (assoc watchers k f)))

  (-remove-watch [self k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (remove-multi-watch vecnal self)))

  (-notify-watches [self v v*]
    (when-not (= v v*)
      (doseq [[k f] watchers]
        (f k self v v*)))))

(defn mux [vecnal] (MuxSignal. (vec @vecnal) vecnal {}))

;;;; # Functor

(defn- map-vecnal-watcher [map-vecnal f inputs]
  (reify VecnalWatcher
    (on-insert [_ i v*]
      (let [v* (apply f (map (comp #(nth % i) deref) inputs))
            coll (.-coll map-vecnal)]
        (when-not (and (< i (count coll))
                       (= v* (get coll i)))
          (set! (.-coll map-vecnal) (insert coll i v*))
          (doseq [[_ w] (.-watchers map-vecnal)]
            (on-insert w i v*)))))

    (on-assoc [_ i v*]
      (let [v* (apply f (map (comp #(nth % i) deref) inputs))
            coll (.-coll map-vecnal)]
        (when-not (= v* (get coll i))
          (set! (.-coll map-vecnal) (assoc coll i v*))
          (doseq [[_ w] (.-watchers map-vecnal)]
            (on-assoc w i v*)))))

    (on-dissoc [_ i]
      (set! (.-coll map-vecnal) (rrb-dissoc (.-coll map-vecnal) i))
      (doseq [[_ w] (.-watchers map-vecnal)]
        (on-dissoc w i)))))

(deftype MapSeqnal [f ^:mutable coll inputs ^:mutable watchers]
  IDeref
  (-deref [_]
    (when (empty? watchers)
      (set! coll (apply (partial mapv f) (map deref inputs))))
    coll)

  Vecnal
  (add-multi-watch [self k w]
    (when (empty? watchers)
      (doseq [input inputs]
        (add-multi-watch input self (map-vecnal-watcher self f inputs))))
    (set! watchers (assoc watchers k w)))

  (remove-multi-watch [self k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (doseq [input inputs]
        (remove-multi-watch input self)))))

(defn smap-map [f & inputs]
  (MapSeqnal. f (apply (partial mapv f) (map deref inputs)) (vec inputs) {}))

;;;; # Concat

(defn- concat-vecnal-watcher [concat-vecnal input-index]
  (reify VecnalWatcher
    (on-insert [_ i v*]
      (let [i (+ (aget (.-pre-counts concat-vecnal) input-index) i)]
        (set! (.-coll concat-vecnal) (insert (.-coll concat-vecnal) i v*))
        (doseq [input-index (range (inc input-index) (count (.-inputs concat-vecnal)))]
          (aset (.-pre-counts concat-vecnal) input-index
                (inc (aget (.-pre-counts concat-vecnal) input-index))))
        (doseq [[_ w] (.-watchers concat-vecnal)]
          (on-insert w i v*))))

    (on-assoc [_ i v*]
      (let [i (+ (aget (.-pre-counts concat-vecnal) input-index) i)]
        (set! (.-coll concat-vecnal) (assoc (.-coll concat-vecnal) i v*))
        (doseq [[_ w] (.-watchers concat-vecnal)]
          (on-assoc w i v*))))

    (on-dissoc [_ i]
      (let [i (+ (aget (.-pre-counts concat-vecnal) input-index) i)]
        (set! (.-coll concat-vecnal) (rrb-dissoc (.-coll concat-vecnal) i))
        (doseq [input-index (range (inc input-index) (count (.-inputs concat-vecnal)))]
          (aset (.-pre-counts concat-vecnal) input-index
                (dec (aget (.-pre-counts concat-vecnal) input-index))))
        (doseq [[_ w] (.-watchers concat-vecnal)]
          (on-dissoc w i))))))

(deftype ConcatSeqnal [^:mutable coll pre-counts inputs ^:mutable watchers]
  IDeref
  (-deref [_]
    (when (empty? watchers)
      (set! coll (into [] (mapcat deref) inputs))
      (doseq [[i c] (reductions + 0 (map (comp count deref) (butlast inputs)))]
        (aset pre-counts i c)))
    coll)

  Vecnal
  (add-multi-watch [self k w]
    (when (empty? watchers)
      (doseq [[i input] (map-indexed vector inputs)]
        (add-multi-watch input self (concat-vecnal-watcher self i))))
    (set! watchers (assoc watchers k w)))

  (remove-multi-watch [self k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (doseq [input inputs]
        (remove-multi-watch input self)))))

(defn smap-concat [& inputs]
  (ConcatSeqnal. (into [] (mapcat deref) inputs)
                 (to-array (reductions + 0 (map (comp count deref) (butlast inputs))))
                 (vec inputs) {}))
