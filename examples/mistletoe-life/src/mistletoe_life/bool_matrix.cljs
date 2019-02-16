(ns mistletoe-life.bool-matrix)

(defn- in-bounds? [rows cols i j]
  (and (< -1 i rows) (< -1 j cols)))

(defn- internal-index [cols i j]
  (+ (* cols i) j))

(defprotocol MatrixGet
  (mget [m i j]))

(defprotocol MatrixSet
  (mset! [m i j v]))

(deftype BoolMatrix [cells rows cols]
  MatrixGet
  (mget [_ i j]
    (when (in-bounds? rows cols i j)
      (> (aget cells (internal-index cols i j)) 0))))

(defn bool-matrix [rows cols]
  (BoolMatrix. (js/Uint8Array. (* rows cols)) rows cols))

(deftype TransientBoolMatrix [cells rows cols editable?]
  MatrixSet
  (mset! [self i j v]
    {:pre [editable?]}
    (when (in-bounds? rows cols i j)
      (aset cells (internal-index cols i j) (if v 1 0)))
    self))

(defn transient-bool-matrix [rows cols]
  (TransientBoolMatrix. (js/Uint8Array. (* rows cols)) rows cols true))

(defn persistent-bool-matrix! [m]
  (set! (.-editable? m) false)
  (BoolMatrix. (.-cells m) (.-rows m) (.-cols m)))

