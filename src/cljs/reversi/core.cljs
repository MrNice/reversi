(ns reversi.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType])
    (:import goog.History))

(def board-size 8)
(def initial-board [[4 4 "white"] [4 5 "black"] [5 4 "black"] [5 5 "white"]])
(def test-moves [[1 1 "white"]])

;; -------------------------
;; Views
(defn make-empty-board []
  "Creates an empty 8 x 8 board, which
   is really just two level nested vectors
   with 0 as a placeholder"
  (vec (repeat board-size (vec (repeat board-size 0)))))

(defn set-board-tile [x y color board]
  "Updates a specific board tile, returning the new board"
  (let [xi (dec x) yi (dec y)]
    (assoc-in board [xi yi] color)))

(defn apply-move [board move]
  "Applies a board moves defined as [x y color]
   and returns the new board. Note the arguments
   are in order for reduction / folding"
  (let [[x y color] move]
    (set-board-tile x y color board)))

(defn apply-moves [moves board]
  "Applies multiple moves stored in a nested vector
   see: initial-board"
  (reduce apply-move board moves))

(defn initialize-board [] (apply-moves initial-board (make-empty-board)))

(defonce board (atom (initialize-board)))

(defn handle-move [x y]
  "Player always plays as black"
  (swap! board #(set-board-tile (inc x) (inc y) "black" %)))

(defn is-valid-move? [x y board])

;;--- VIEWS
(defn count-black [board]
  (count (filter #(= "black" %) (flatten board))))

(defn score [board]
  [:h2.score "Score: " (count-black board)])

(defn render-cell [x y color]
  ^{:key (str x y)}
  [:td
    [:div.piece {:class color
      :on-click #(handle-move x y)}]])

(defn render-row [x row]
  ^{:key (str x)} [:tr (map-indexed (partial render-cell x) row)])

(defn render-board [board]
  [:table (map-indexed render-row board)])

(defn give-up []
  "The forfeit button, resets the game state"
  [:div.give-up {:on-click #(reset! board (initialize-board))} "Forfeit"])

(defn reversi []
  [:div
    [score @board]
    [render-board @board]
    [give-up]])
;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [reversi] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
