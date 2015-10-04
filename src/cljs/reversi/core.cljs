(ns reversi.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [goog.events :as events]
              [goog.history.EventType :as EventType])
    (:import goog.History))

(defonce *debug* (atom true))
(defonce message (atom ""))
(defn toggle-debug [] (swap! *debug* not))

(def board-size 8)
;; TODO (Nicholas): Generate these based off of board size
(def initial-board [[2 3 "white"] [1 3 "white"] [3 3 "white"] [3 4 "black"] [4 3 "black"] [4 4 "white"]])
; (def initial-board [[3 3 "white"] [3 4 "black"] [4 3 "black"] [4 4 "white"]])
(def test-moves [[1 1 "white"]])

;; -------------------------
;; Setup and Board manipulation helpers
(defn make-empty-board []
  "Creates an empty 8 x 8 board, which
   is really just two level nested vectors
   with 0 as a placeholder"
  (vec (repeat board-size (vec (repeat board-size 0)))))

(defn set-board-tile [x y color board]
  "Updates a specific board tile, returning the new board"
  (assoc-in board [x y] color))

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

(defn apply-moves! [moves]
  (swap! board #(apply-moves moves %))
  true)

(defn opposite-color [color]
  (if (= color "black") "white" "black"))

;;--- MOVE VALIDATION AND PIECE FLIPPING LOGIC

(defn within-bounds? [min max value]
  "Helper function to determine if a number is between two others"
  (and (>= value min) (<= value max)))

(defn on-board? [x y]
  "Ensure a move is actually on the board"
  (let [max (dec board-size)]
    (and (within-bounds? 0 max x) (within-bounds? 0 max y))))

(defn unoccupied? [x y board]
  (= (get-in board [x y]) 0))

(defn occupied? [x y board]
  (not (unoccupied? x y board)))

(defn get-space [x y board]
  (get-in board [x y]))

(def directions [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]])

(defn keep-looking? [x y other-color board]
  (and (on-board? x y) (= (get-space x y board) other-color)))

(defn traverse [board start-color startx starty direction]
  "Given a possible move, figure out which stones will flip
   if the move is played. Returns an empty vector if none"
  (let [tryx (+ startx (first direction))
        tryy (+ starty (last direction))
        other-color (opposite-color start-color)]
    (if (keep-looking? tryx tryy other-color board)
      (vec (concat (traverse board start-color tryx tryy direction) [[tryx tryy start-color]]))
      (if (= (get-space tryx tryy board) start-color)
        [true]
        [false]))))

(defn validator [moves]
  (if (and (true? (first moves)) (> (count moves) 1))
    (vec (rest moves))
    false))

(defn valid-move? [color x y board]
  "Test to see if a move is valid. If it is, return a vector
   of all the moves to make if the move is played. Uses structural sharing
   of immutable data to great effect here"
  (if (and (on-board? x y) (unoccupied? x y board))
    (let [flip-moves (first (filterv #(not= false %) (mapv validator (mapv (partial traverse board color x y) directions))))]
      (if (< 0 (count flip-moves))
        (apply-moves! flip-moves)
        false))
    false))

(defn handle-move [color x y]
  (if (valid-move? color x y @board)
    (do (swap! board #(set-board-tile x y color %)) (reset! message ""))
    (reset! message "Not a valid move")))

(def handle-player-move (partial handle-move "black"))

(def handle-computer-move (partial handle-move "white"))

;;--- VIEW HELPERS
(defn count-black [board]
  (count (filter #(= "black" %) (flatten board))))

;;--- VIEWS
(defn score [board]
  [:h2.score "Score: " (count-black board)])

(defn render-cell [x y color]
  ^{:key (str x y)}
  [:td (str x ", " y)
    [:div.piece {:class color
      :on-click #(handle-player-move x y)}]])

(defn render-row [x row]
  ^{:key (str x)} [:tr (map-indexed (partial render-cell x) row)])

(defn render-board [board]
  [:table (map-indexed render-row board)])

(defn give-up []
  "The forfeit button, resets the game state"
  [:div.give-up {:on-click #(reset! board (initialize-board))} "Forfeit"])

(defn debug-button []
  (let [debug @*debug*]
    [:div.give-up {:on-click toggle-debug
      :style (if debug {:background-color "rgb(227, 87, 87)"}
                       {:background-color "#fff"})}
      (str "Debug: " debug )]))

(defn message-header []
  [:h2.score {:style {:color "rgba(135, 0, 0, 1)"}} @message])

(defn reversi []
  [:div
    [score @board]
    [message-header]
    [render-board @board]
    [give-up][debug-button]])


;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [reversi] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
