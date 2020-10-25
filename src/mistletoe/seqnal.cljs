(ns mistletoe.seqnal
  (:require [clojure.core.rrb-vector :as rrb]))

;;; FIXME: `.-value`:s might be stale if dependencies were not being watched

(defn- insert [coll i v]
  (if (= i (count coll))
    (conj coll v)
    (rrb/catvec (rrb/subvec coll 0 i) [v] (rrb/subvec coll i))))

(defn- rrb-dissoc [coll i]
  (rrb/catvec (rrb/subvec coll 0 i) (rrb/subvec coll (inc i))))

;;;; # Seqnal

(defprotocol Seqnal
  (add-multi-watch [self k w])
  (remove-multi-watch [self k]))

(defprotocol SeqnalWatcher
  (on-insert [self i v])
  (on-assoc [self i v])
  ; TODO: (on-move [self i i*])
  (on-dissoc [self i]))

;;;; # Imux

;; OPTIMIZE: Use linear time Levenstein or at least a better heuristic:
(defn- imux-watcher [imux-seqnal]
  (fn [k signal coll coll*]
    (loop [i 0, [v :as coll] coll, [v* :as coll*] coll*]
      (if (seq coll)
        (do
          (if (seq coll*)
            (when-not (= v v*)
              (on-assoc imux-seqnal i v*))
            (on-dissoc imux-seqnal i))
          (recur (inc i) (rest coll) (rest coll*)))
        (when (seq coll*)
          (on-insert imux-seqnal i v*)
          (recur (inc i) (rest coll) (rest coll*)))))))

(deftype ImuxSeqnal [signal ^:mutable watchers]
  IDeref
  (-deref [_] (-deref signal))

  Seqnal
  (add-multi-watch [self k w]
    (when (empty? watchers)
      (add-watch signal self (imux-watcher self)))
    (set! watchers (assoc watchers k w)))

  (remove-multi-watch [_ k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (remove-watch signal k)))

  SeqnalWatcher
  (on-insert [_ i v*]
    (doseq [[_ w] watchers]
      (on-insert w i v*)))

  (on-assoc [_ i v*]
    (doseq [[_ w] watchers]
      (on-assoc w i v*)))

  (on-dissoc [_ i]
    (doseq [[_ w] watchers]
      (on-dissoc w i))))

(defn imux [signal] (ImuxSeqnal. signal {}))

;;;; # Mux

(defn- refresh-mux [mux-signal]
  (let [coll (.-value mux-signal)
        coll* @(.-seqnal mux-signal)]
    (set! (.-value mux-signal) coll*)
    (-notify-watches mux-signal coll coll*)))

(defn- mux-watcher [mux-signal]
  (reify SeqnalWatcher
    (on-insert [_ _ _] (refresh-mux mux-signal))
    (on-assoc [_ _ _] (refresh-mux mux-signal))
    (on-dissoc [_ _] (refresh-mux mux-signal))))

(deftype MuxSignal [value seqnal ^:mutable watchers]
  IDeref
  (-deref [_] value)

  IWatchable
  (-add-watch [self k f]
    (when (empty? watchers)
      (add-multi-watch seqnal self (mux-watcher self)))
    (set! watchers (assoc watchers k f)))

  (-remove-watch [self k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (remove-multi-watch seqnal self)))

  (-notify-watches [self v v*]
    (when-not (= v v*)
      (doseq [[k f] watchers]
        (f k self v v*)))))

(defn mux [seqnal] (MuxSignal. (vec @seqnal) seqnal {}))

;;;; # Functor

(defn- map-seqnal-watcher [map-seqnal f inputs]
  (reify SeqnalWatcher
    (on-insert [_ i v*]
      (let [v* (apply f (map (comp #(nth % i) deref) inputs))
            coll (.-coll map-seqnal)]
        (when-not (and (< i (count coll))
                       (= v* (get coll i)))
          (set! (.-coll map-seqnal) (insert coll i v*))
          (doseq [[_ w] (.-watchers map-seqnal)]
            (on-insert w i v*)))))

    (on-assoc [_ i v*]
      (let [v* (apply f (map (comp #(nth % i) deref) inputs))
            coll (.-coll map-seqnal)]
        (when-not (= v* (get coll i))
          (set! (.-coll map-seqnal) (assoc coll i v*))
          (doseq [[_ w] (.-watchers map-seqnal)]
            (on-assoc w i v*)))))

    (on-dissoc [_ i]
      (set! (.-coll map-seqnal) (rrb-dissoc (.-coll map-seqnal) i))
      (doseq [[_ w] (.-watchers map-seqnal)]
        (on-dissoc w i)))))

(deftype MapSeqnal [f ^:mutable coll inputs ^:mutable watchers]
  IDeref
  (-deref [_] coll)

  Seqnal
  (add-multi-watch [self k w]
    (when (empty? watchers)
      (doseq [input inputs]
        (add-multi-watch input self (map-seqnal-watcher self f inputs))))
    (set! watchers (assoc watchers k w)))

  (remove-multi-watch [self k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (doseq [input inputs]
        (remove-multi-watch input self)))))

(defn smap-map [f & inputs]
  (MapSeqnal. f (apply (partial mapv f) (map deref inputs)) (vec inputs) {}))

;;;; # Concat

(defn- concat-seqnal-watcher [concat-seqnal input-index]
  (reify SeqnalWatcher
    (on-insert [_ i v*]
      (let [i (+ (aget (.-pre-counts concat-seqnal) input-index) i)]
        (set! (.-coll concat-seqnal) (insert (.-coll concat-seqnal) i v*))
        (doseq [input-index (range (inc input-index) (count (.-inputs concat-seqnal)))]
          (aset (.-pre-counts concat-seqnal) input-index
                (inc (aget (.-pre-counts concat-seqnal) input-index))))
        (doseq [[_ w] (.-watchers concat-seqnal)]
          (on-insert w i v*))))

    (on-assoc [_ i v*]
      (let [i (+ (aget (.-pre-counts concat-seqnal) input-index) i)]
        (set! (.-coll concat-seqnal) (assoc (.-coll concat-seqnal) i v*))
        (doseq [[_ w] (.-watchers concat-seqnal)]
          (on-assoc w i v*))))

    (on-dissoc [_ i]
      (let [i (+ (aget (.-pre-counts concat-seqnal) input-index) i)]
        (set! (.-coll concat-seqnal) (rrb-dissoc (.-coll concat-seqnal) i))
        (doseq [input-index (range (inc input-index) (count (.-inputs concat-seqnal)))]
          (aset (.-pre-counts concat-seqnal) input-index
                (dec (aget (.-pre-counts concat-seqnal) input-index))))
        (doseq [[_ w] (.-watchers concat-seqnal)]
          (on-dissoc w i))))))

(deftype ConcatSeqnal [coll pre-counts inputs ^:mutable watchers]
  IDeref
  (-deref [_] coll)

  Seqnal
  (add-multi-watch [self k w]
    (when (empty? watchers)
      (doseq [[i input] (map-indexed vector inputs)]
        (add-multi-watch input self (concat-seqnal-watcher self i))))
    (set! watchers (assoc watchers k w)))

  (remove-multi-watch [self k]
    (set! watchers (dissoc watchers k))
    (when (empty? watchers)
      (doseq [input inputs]
        (remove-multi-watch input self)))))

(defn smap-concat [& inputs]
  (ConcatSeqnal. (into [] (mapcat deref) inputs)
                 (to-array (cons 0 (map (comp count deref) (butlast inputs))))
                 (vec inputs) {}))

