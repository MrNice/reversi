(ns reversi.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [cljs.core.async :refer [chan sliding-buffer timeout <! put! take! alts!]]
            [alandipert.storage-atom :refer [local-storage]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defonce *flags* (local-storage (atom {:watch false :debug true}) :flags))

(defn flag [k]
  "Helper function to get keys out of the *flags* atom easily
   Example: (if (flag :debug) (pr-str 'Debugging'))"
  (k @*flags*))

(defn flip-bool [k]
  "Returns a function that flips a boolean, such as watch
   debug mode toggles. Simply pass it the key to change.
   For example: (swap! *flags* (flip-bool :debug))"
  (fn [m] (conj m [k (not (k m))])))

(def flip (memoize flip-bool))

(def board-size 8)
; The below was a test board
; (def initial-board [[3 3 "black"] [3 4 "white"] [4 3 "white"] [2 4 "black"] [4 2 "black"]])
;; TODO (Nicholas): Generate these based off of board size
(def initial-board [[3 3 "white"] [3 4 "black"] [4 3 "black"] [4 4 "white"]])
(def test-moves [[1 1 "white"]])

;;--- Setup and Board manipulation helpers
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

(defn initialize-board []
  "Creates a start-of-game board object"
  (apply-moves initial-board (make-empty-board)))

(defonce board (local-storage (atom (initialize-board)) :board))

(defn apply-moves! [moves]
  "Put a set of moves onto the board"
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

(defn get-space [x y board]
  "Simply return the value of a position on the board"
  (get-in board [x y]))

(defn unoccupied? [x y board]
  "Used when validating moves. Why keep checking the move
   if there's already a piece in there?"
  (= (get-space x y board) 0))

(defn keep-looking? [x y other-color board]
  "Helper function to traverse which lets the traversal
   know whether or not to continue"
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

(def cardinal-directions [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]])
(defn valid-move? [color x y board]
  "Test to see if a move is valid. If it is, return a vector of all
   the moves to make if the move is played. Uses structural sharing
   of immutable data to great effect here"
  (if (and (on-board? x y) (unoccupied? x y board))
    (let [flip-moves (apply concat (filterv #(not= false %)
                              (mapv validator
                                (mapv (partial traverse board color x y)
                                      cardinal-directions))))]
      (if (< 0 (count flip-moves))
        (concat [[x y color]] flip-moves)
        false))
    false))

(defn any-moves? [color]
  "See if there is a single place on the board to
  where there's a valid move for this color"
  (let [moves (filterv identity
                (for [x (range board-size) y (range board-size)]
                  (valid-move? color x y @board)))]
    (if (< 0 (count moves))
      moves
      false)))

(defn count-pieces [color]
  "Returns a function which returns the number of pieces
   on the board which are a specific color, when passed a
   board"
  (fn [board]
    (count (filter #(= color %) (flatten board)))))

(def count-black (count-pieces "black"))
(def count-white (count-pieces "white"))

(defonce player-chan (chan (sliding-buffer 1)))
(defn handle-player-move [x y]
  (if-let [moves (valid-move? "black" x y @board)]
    (put! player-chan moves)
    (js/alert "Not a valid move! Please try again!")))

;;--- AI Computer stuff
(defn compute-move [moves]
  "Computer naively selects the largest scoring move"
  (apply max-key count (mapv vec (filterv identity moves))))

;;--- Core Game Logic Loops ----------------------------------------------------
;; The Player Game Loop:
;; When asked for a move, gives an empty move if black cannot play
;; checks to see if we are watching, plays a computer move if we are
;; Otherwise, it simply waits for human input
;; TODO (Nicholas): Clean up the watch toggle logic and bugs
(defonce get-player-move (chan (sliding-buffer 1)))
(defonce player-move (chan (sliding-buffer 1)))
(defonce player-loop
  (go-loop []
    (let [go (<! get-player-move)]
      (if-let [moves (any-moves? "black")]
        (if (flag :watch)
          (do (<! (timeout 200))
              (>! player-move (compute-move moves)))
          (>! player-move (<! player-chan)))
        (>! player-move []))
      (recur))))

;; The Computer Game Loop:
;; When asked for a move, waits a variable amount of time and then
;; Computes a move, unless none is available, in which case it simply
;; passes along an empty move
(defonce get-computer-move (chan (sliding-buffer 1)))
(defonce computer-move (chan (sliding-buffer 1)))
(defonce computer-loop (go-loop []
  (let [go (<! get-computer-move)]
    (<! (timeout (if (flag :watch) 200 1500)))
    (if-let [moves (any-moves? "white")]
      (>! computer-move (compute-move moves))
      (>! computer-move []))
    (recur))))

;; Main Game Loop:
;; Ensure that the game has not ended, because someone can play
;; Request a player move
;; Apply the player's move to the board
;; Request a computer move
;; Apply the computer's move to the board
;; Recur
(defonce game-loop (go-loop []
  (if-not (or (any-moves? "black") (any-moves? "white"))
    (do
      (js/alert
        (if (> (count-black @board)
               (count-white @board))
            "You Win!"
            "You lose, try again?"))
      (reset! board (initialize-board))))
  (>! get-player-move true)
  (apply-moves! (<! player-move))
  (>! get-computer-move true)
  (apply-moves! (<! computer-move))
  (recur)))

;;--- VIEWS --------------------------------------------------------------------
(defn score [board]
  [:h2.score "SCORE: "
    [:span {:style {:color "black"}} (count-black board)]
    [:span {:style {:color "grey"}} " : "]
    [:span {:style {:color "white"}} (count-white board)]])

;; Board rendering functions
(defn render-cell [x y color]
  (let [debug (:debug @*flags*)]
    ^{:key (str x y debug)}
    [:td
      [:a {:title (str x ", " y) :class (if debug "tooltip" "")}
      [:div.piece {:class color
        :on-click #(handle-player-move x y)}]]]))

(defn render-row [x row]
  ^{:key (str x)} [:tr (doall (map-indexed (partial render-cell x) row))])

(defn render-board [board]
  [:div
    [:table (doall (map-indexed render-row board))]])

;;--- Button Views and State Helpers
(defn forfeit []
  "Forfeit button resets the game state"
  [:div.button {:on-click #(reset! board (initialize-board))} "Forfeit"])

(defn toggle-debug []
  "Flip the debug button"
  (swap! *flags* (flip :debug)) (reagent/flush))

(defn debug-button []
  (let [debug (:debug @*flags*)]
    [:div.button {:on-click toggle-debug
      :style (if debug {:background-color "rgb(227, 87, 87)"}
                       {:background-color "#fff"})}
      (str "Debug: " debug)]))

(defn toggle-watch []
  "Flip the watch toggle, and also shove a move into
   the player's channel if we click watch. This is why
   we use a sliding buffer"
  (if-not (:watch @*flags*)
    (if-let [moves (any-moves? "black")]
      (put! player-chan (compute-move moves))))
  (swap! *flags* (flip :watch)))

(defn watch-button []
  (let [watch (:watch @*flags*)]
    [:div.button {:on-click toggle-watch
      :style (if watch {:background-color "rgb(0, 255, 247)"}
                       {:background-color "#fff"})}
      (str "Watch: " watch )]))

(defn buttons []
  [:div.buttons
    [forfeit]
    [watch-button]
    [debug-button]])

;;--- Core View
(defn reversi []
  [:div
    [score @board]
    [render-board @board]
    [buttons]])

;; -------------------------
;; Initialize app
(defn mount-root []
  (reagent/render [reversi] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
