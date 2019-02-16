(ns mistletoe-life.byte-matrix)

(defn- in-bounds? [rows cols i j]
  (and (< -1 i rows) (< -1 j cols)))

(defn- internal-index [cols i j]
  (+ (* cols i) j))

(defprotocol MatrixGet
  (mget [m i j]))

(defprotocol MatrixSet
  (mset! [m i j v]))

(deftype ByteMatrix [cells rows cols]
  MatrixGet
  (mget [_ i j]
    (when (in-bounds? rows cols i j)
      (aget cells (internal-index cols i j)))))

(defn byte-matrix [rows cols]
  (ByteMatrix. (js/Uint8Array. (* rows cols)) rows cols))

(deftype TransientByteMatrix [cells rows cols editable?]
  MatrixSet
  (mset! [self i j v]
    {:pre [editable?]}
    (when (in-bounds? rows cols i j)
      (aset cells (internal-index cols i j) v))
    self))

(defn transient-byte-matrix [rows cols]
  (TransientByteMatrix. (js/Uint8Array. (* rows cols)) rows cols true))

(defn persistent-byte-matrix! [m]
  (set! (.-editable? m) false)
  (ByteMatrix. (.-cells m) (.-rows m) (.-cols m)))

