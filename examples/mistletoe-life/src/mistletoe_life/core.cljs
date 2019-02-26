(ns mistletoe-life.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.signal.util :refer [seqsig->sigseq]]
            [mistletoe.dom :refer [el append-child!]]
            [mistletoe-life.byte-matrix :as m]))

;;;;

(def ^:private width 80)
(def ^:private height 80)
(def ^:private framerate 2)

;;;;

(def ^:private live 1)
(def ^:private dead 0)

(defn- live? [cell] (> cell 0))

(defn- toggle-cell [cell] (bit-xor cell 1))

(defn- empty-grid []
  (m/byte-matrix height width))

(defn- next-cells [cells]
  (let [get-cell (fn [i j] (or (m/mget cells i j) 0))
        live-neighbours (fn [i j]
                          (+ (get-cell (dec i) (dec j))
                             (get-cell (dec i) j)
                             (get-cell (dec i) (inc j))
                             (get-cell i (dec j))
                             (get-cell i (inc j))
                             (get-cell (inc i) (dec j))
                             (get-cell (inc i) j)
                             (get-cell (inc i) (inc j))))
        next-cell (fn [i j]
                    (let [cell (m/mget cells i j)]
                      (if (live? cell)
                        (case (live-neighbours i j)
                          (0 1) dead ; underpopulation
                          (2 3) live
                          dead) ; overpopulation
                        (if (= (live-neighbours i j) 3)
                          live ; reproduction
                          dead))))]
    (loop [i 0, cells* (m/transient-byte-matrix height width)]
      (if (< i height)
        (recur (inc i)
               (loop [j 0, cells* cells*]
                 (if (< j width)
                   (recur (inc j)
                          (m/mset! cells* i j (next-cell i j)))
                   cells*)))
        (m/persistent-byte-matrix! cells*)))))

;;;;

(defn- next-phase [current-phase]
  (case current-phase
    :edit :run
    :run :edit))

(def ^:private state
  (sgn/source {:phase :edit
               :cells (empty-grid)}))

(def ^:private editable? (smap #(= (:phase %) :edit) state))

;;;;

(defn- row-view [i row]
  (el :tr
      (for [[j cell] (map-indexed vector (seqsig->sigseq row))]
        (el :td
            :style {:width            "15px", :height "15px"
                    :border           "1px solid black"
                    :background-color (smap #(if (live? %) "black" "transparent") cell)}
            :onclick (fn [_]
                       (when @editable?
                         (swap! state update :cells
                                #(m/mupdate % i j toggle-cell))))))))

(defn- ui-main [state]
  (el :div
      (el :input :type "button" :value (smap (comp name next-phase :phase) state)
          :onclick (fn [_]
                     (case (:phase (swap! state update :phase next-phase))
                       :run (swap! state assoc :interval-id
                                   (.setInterval js/window
                                                 (fn [] (swap! state update :cells next-cells))
                                                 (/ 1000 framerate)))
                       :edit (.clearInterval js/window (:interval-id @state)))))

      (el :input :type "button" :value "clear"
          :onclick (fn [_]
                     (when @editable?
                       (swap! state assoc :cells (empty-grid)))))

      (el :table
          :style {:border-collapse "collapse"}
          (for [[i row] (->> state
                             (smap (comp m/rows :cells) state)
                             seqsig->sigseq
                             (map-indexed vector))]
            (row-view i row)))))

;;;;

(defn main []
  (let [parent (.getElementById js/document "app-root")]
    (append-child! parent (ui-main state))))

