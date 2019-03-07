(ns mistletoe.signal.util
  (:require [mistletoe.signal :refer [smap]]))

(defprotocol KeyDepot
  (-alloc-key [self])
  (-return-key [self k]))

;; OPTIMIZE: `used` could just be a mutable stack (e.g. JS array), queue vs. stack doesn't really matter:
(deftype PerpetualKeyDepot [^:mutable used, ^:mutable counter]
  KeyDepot
  (-alloc-key [_]
    (if (seq used)
      (let [k (peek used)]
        (set! used (pop used))
        k)
      (let [k counter]
        (set! counter (inc k))
        k)))

  (-return-key [_ k] (set! used (conj used k))))

(def ^:private key-depot (->PerpetualKeyDepot #queue [] 0))

(defn alloc-watch-key [] (-alloc-key key-depot))

;; FIXME: Actually use this (in `mistletoe.dom`):
(defn free-watch-key [k] (-return-key key-depot k))

(defn seqsig->sigseq
  "Make a sequence of signals for the elements of the
  signal of sequnces `seqsig`."
  [seqsig]
  (map-indexed (fn [i _] (smap #(nth % i) seqsig)) @seqsig))

(def ^:private lookup-sentinel (js-obj))

(defn map-index-cached
  "`(map-index-cached f)` acts like `(partial mapv f)` but for indices that were
  also filled on the previous invocation the previous value gets returned
  instead of `(f v)`. Useful for templating, where you want to reuse and update
  existing UI subtrees and possibly add fresh ones to the end."
  [f]
  (let [prev (volatile! [])]
    (fn [coll]
      (let [prev-coll @prev
            res (into []
                      (map-indexed (fn [i v]
                                     (let [x (get prev-coll i lookup-sentinel)]
                                       (if (identical? x lookup-sentinel)
                                         (f v)
                                         x))))
                      coll)]
        (vreset! prev res)))))

(defn map-key-cached
  "Like [[map-index-cached]], but caches based on `(get-key i v)` instead of just `i`
  (where `v` is a value in the [[IReduce]] `coll` and `i` is its index in `coll`."
  [get-key f]
  (let [cache (volatile! {})]
    (fn [coll]
      (let [cachev @cache]
        (loop [i 0, vs coll, res (transient []), cachev* (transient {})]
          (if (empty? vs)
            (do (vreset! cache (persistent! cachev*))
                (persistent! res))
            (let [v (first vs)
                  k (get-key i v)
                  cached-y (get cachev k lookup-sentinel)
                  y (if (identical? cached-y lookup-sentinel)
                      (f v)
                      cached-y)]
              (recur (inc i) (rest vs) (conj! res y) (assoc! cachev* k y)))))))))
