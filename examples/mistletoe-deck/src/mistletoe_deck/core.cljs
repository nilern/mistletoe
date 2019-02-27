(ns mistletoe-deck.core
  (:require [mistletoe.signal :as sgn :refer [smap]]
            [mistletoe.signal.util :refer [map-index-cached]]
            [mistletoe.dom :refer [el append-child!]]))

(defn- rand [min max]
  (->> (.random js/Math)
       (* (- max min))                                      ; zoom
       (.floor js/Math)                                     ; clip
       (+ min)))                                            ; translate

(def ^:private framerate 3)

;;;;

(defn- rank-symbol [rank]
  (case rank
    1 "A"
    11 "J"
    12 "Q"
    13 "K"
    (str rank)))

(def ^:private suits [:clubs :diamonds :hearts :spades])

(def ^:private suit-symbol
  {:clubs    "\u2663"
   :diamonds "\u2666"
   :hearts   "\u2665"
   :spades   "\u2660"})

(def ^:private suit-color
  {:clubs    "black"
   :diamonds "crimson"
   :hearts   "crimson"
   :spades   "black"})

(def ^:private nsuits (count suits))
(def ^:private nranks 13)
(def ^:private deck-len (* nsuits nranks))

(defrecord Card [suit rank]
  Object
  (toString [_] (str (rank-symbol rank) (suit-symbol suit))))

;;;;

(def ^:private state
  (sgn/source {:phase       :select
               :phase-state nil
               :deck        (into [] (for [suit suits
                                           rank (range 1 (inc nranks))]
                                       (->Card suit rank)))}))

(def ^:private editable? (smap #(= (:phase %) :select) state))

;;;;

(defn- card-view [i card]
  (el :div :class "card"
      :style {:color      (smap (comp suit-color :suit) card)
              :transform  (str "translate(" (* 35 i) "px)")
              :transition (str "transform " (/ 1000 framerate 2) "ms ease-in-out")}
      (smap str card)))

(defn- ui-main [state]
  (let [deck (smap :deck state)]
    (el :div
        (el :div
            (el :input :type "button" :value "Fisher-Yates shuffle"
                :onclick (fn [_]
                           (let [step (fn [] (swap! state
                                                    (fn [state]
                                                      (let [i (-> state :phase-state :index)]
                                                        (if (< i (dec deck-len))
                                                          (let [j (rand i deck-len)]
                                                            (-> state
                                                                (assoc-in [:deck i] (get-in state [:deck j]))
                                                                (assoc-in [:deck j] (get-in state [:deck i]))
                                                                (update-in [:phase-state :index] inc)))
                                                          (do (.clearInterval js/window (:interval-id state))
                                                              (assoc state :phase :select, :phase-state nil)))))))]
                             (when @editable?
                               (swap! state assoc :phase :fisher-yates)
                               (swap! state assoc :phase-state {:index 0})
                               (swap! state assoc :interval-id (.setInterval js/window step (/ 1000 framerate))))))))

        (el :div
            (->> deck
                 (smap (partial map-indexed (fn [i _] [i (smap #(nth % i nil) deck)])))
                 (smap (map-index-cached (fn [[i card]] (card-view i card)))))))))

;;;;

(defn- main []
  (let [parent (.getElementById js/document "app-root")]
    (append-child! parent (ui-main state))))
