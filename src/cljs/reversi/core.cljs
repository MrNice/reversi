(ns reversi.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [cljs.core.async :refer [chan sliding-buffer timeout <! put! take!]]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [alandipert.storage-atom :refer [local-storage]]
            [goog.history.EventType :as EventType])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:import goog.History))

(defonce message (atom ""))
(defonce *debug* (local-storage (atom true) :debug))
(defonce *watch* (local-storage (atom false) :watch))

(def player-chan (chan (sliding-buffer 1)))
(def board-size 8)
; The below was a test board
; (def initial-board [[3 3 "black"] [3 4 "white"] [4 3 "white"] [2 4 "black"] [4 2 "black"]])
;; TODO (Nicholas): Generate these based off of board size
(def initial-board [[3 3 "white"] [3 4 "black"] [4 3 "black"] [4 4 "white"]])
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

(defonce board (local-storage (atom (initialize-board)) :board))

(defn apply-moves! [moves]
  (swap! board #(apply-moves moves %)))

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
   if the move is played. Returns an empty vector if none.
   Note: in order to decide whether or not we hit the start color,
   we prefix the move list with true or false once the recursion
   bottoms out"
  (let [tryx (+ startx (first direction))
        tryy (+ starty (last direction))
        other-color (opposite-color start-color)]
    (if (keep-looking? tryx tryy other-color board)
      (vec (concat (traverse board start-color tryx tryy direction) [[tryx tryy start-color]]))
      ; If we hit an edge
      (if (= (get-space tryx tryy board) start-color)
        [true]
        [false]))))

(defn validator [moves]
  "Turn the output of traverse into something that can be consumed by
   apply-moves!"
  (if (and (true? (first moves)) (> (count moves) 1))
    (vec (rest moves))
    false))

(defn valid-move? [color x y board]
  "Test to see if a move is valid. If it is, return a vector
   of all the moves to make if the move is played. Uses structural sharing
   of immutable data to great effect here"
  (if (and (on-board? x y) (unoccupied? x y board))
    (let [flip-moves (apply concat (filterv #(not= false %)
                              (mapv validator
                                (mapv (partial traverse board color x y)
                                      directions))))]
      (if (< 0 (count flip-moves))
        (concat [[x y color]] flip-moves)
        false))
    false))

(defn handle-move [color x y]
  (if-let [moves (valid-move? color x y @board)]
    (do (reset! message "") (put! player-chan moves))
    (reset! message "Not a valid move")))

(def handle-player-move (partial handle-move "black"))

(defn any-moves? [color]
  "See if there is a single place on the board to where there's a valid move
  for this color"
  (let [moves (filterv identity
                (for [x (range board-size) y (range board-size)]
                  (valid-move? color x y @board)))]
    (if (< 0 (count moves))
      moves
      false)))

;;--- AI Computer stuff
(defn computer-move [moves]
  "Computer naively selects the largest scoring move"
  ;; Find the greatest when count is applied.... probably a better way to do this
  (reduce
    (fn [acc val]
      (if (> (count acc) (count val))
        acc
        val))
    (filterv identity moves)))

;; Game play
;; Computer scans to see if black has any available moves
  ;; Player 1 goes
;; Computer scans to see if white has any available moves
  ;; Computer selects current largest move and plays (max strat, weak tho)
;; If neither player can play, game over
;; If board is full, game over
(defonce game-loop (go-loop []
  (if-not (or (any-moves? "black") (any-moves? "white"))
    (do (js/alert "Game Over!") (reset! board (initialize-board))))
  (if (any-moves? "black")
    (if @*watch*
      (if-let [moves (any-moves? "black")]
        (do
          (<! (timeout 200))
          (apply-moves! (computer-move moves))))
      (apply-moves! (<! player-chan)))
    true)
  (<! (timeout (if @*watch* 200 1500)))
  (if-let [moves (any-moves? "white")]
    (apply-moves! (computer-move moves)))
  (recur)))

;;--- VIEW HELPERS
(defn count-black [board]
  (count (filter #(= "black" %) (flatten board))))

;;--- VIEWS
(defn score [board]
  [:h2.score "Score: " (count-black board)])

(defn render-cell [x y color]
  (let [debug @*debug*]
    ^{:key (str x y debug)}
    [:td (if debug (str x ", " y))
      [:div.piece {:class color
        :on-click #(handle-player-move x y)}]]))

(defn render-row [x row]
  ^{:key (str x)} [:tr (doall (map-indexed (partial render-cell x) row))])

(defn render-board [board]
  [:div
    [:table (map-indexed render-row board)]
    [:span.bottom]])

(defn give-up []
  "The forfeit button, resets the game state"
  [:div.give-up {:on-click #(reset! board (initialize-board))} "Forfeit"])

(defn toggle-debug [] (swap! *debug* not))

(defn debug-button []
  (let [debug @*debug*]
    [:div.give-up {:on-click toggle-debug
      :style (if debug {:background-color "rgb(227, 87, 87)"}
                       {:background-color "#fff"})}
      (str "Debug: " debug )]))

(defn toggle-watch []
  (if @*watch*
    (if-let [moves (any-moves? "black")]
      (put! player-chan (computer-move moves))))
  (swap! *watch* not))

(defn watch-button []
  (let [watch @*watch*]
    [:div.give-up {:on-click toggle-watch
      :style (if watch {:background-color "rgb(0, 255, 247)"}
                       {:background-color "#fff"})}
      (str "Watch: " watch )]))

(defn message-header []
  [:h2.score {:style {:color "rgba(135, 0, 0, 1)"}} @message])

(defn reversi []
  [:div
    [score @board]
    [message-header]
    [render-board @board]
    [give-up]
    [watch-button]
    [debug-button]])

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [reversi] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
