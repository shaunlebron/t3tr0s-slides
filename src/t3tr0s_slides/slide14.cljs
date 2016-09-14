(ns t3tr0s-slides.slide14
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")
(def dark-red "#944")
(def light-red "#f8c")

(def pieces
  {:I [[-1  0] [ 0  0] [ 1  0] [ 2  0]]
   :L [[ 1 -1] [-1  0] [ 0  0] [ 1  0]]
   :J [[-1 -1] [-1  0] [ 0  0] [ 1  0]]
   :S [[ 0 -1] [ 1 -1] [-1  0] [ 0  0]]
   :Z [[-1 -1] [ 0 -1] [ 0  0] [ 1  0]]
   :O [[ 0 -1] [ 1 -1] [ 0  0] [ 1  0]]
   :T [[ 0 -1] [-1  0] [ 0  0] [ 1  0]]})

(defn rotate-coord [[x y]] [(- y) x])
(defn rotate-piece [piece] (mapv rotate-coord piece))

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))
(def filled-board
  [[ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 0 0 0 0 0 0 0 0]
   [ 0 0 1 0 0 0 0 0 1 0]
   [ 0 1 1 1 0 1 1 0 1 1]
   [ 1 1 1 1 0 1 1 1 1 1]
   [ 1 1 1 1 0 1 1 0 1 1]
   [ 1 1 1 1 0 1 1 1 1 1]])

(def initial-pos [5 2])

(def app (atom {:board filled-board
                :piece (rotate-piece (:I pieces))
                :position initial-pos}))

(defn write-piece
  [board coords [cx cy] value]
  (if-let [[x y] (first coords)]
    (recur (try (assoc-in board [(+ y cy) (+ x cx)] value)
                (catch js/Error _ board))
           (rest coords)
           [cx cy]
           value)
    board))

(declare get-drop-pos)

(defn create-drawable-board
  [board piece [x y]]
  (let [gy    (get-drop-pos board  piece [x y])
        board1 (write-piece board  piece [x gy] "G")
        board2 (write-piece board1 piece [x y ] "P")]
    board2))

(defn app-drawable-board!
  []
  (let [board (:board @app)
        piece (:piece @app)
        pos (:position @app)]
    (create-drawable-board board piece pos)))

(defn lock-piece! []
  (let [{:keys [piece position]} @app]
    (swap! app
      update-in [:board]
        write-piece piece position 1)))

(defn piece-fits?
  [board piece [cx cy]]
  (every? (fn [[x y]]
            (zero? (get-in board [(+ y cy) (+ x cx)])))
          piece))

(defn app-piece-fits?
  []
  (boolean (piece-fits? (:board @app) (:piece @app) (:position @app))))

(defn try-shift! [dx]
  (let [piece (:piece @app)
        [x y] (:position @app)
        board (:board @app)
        new-pos [(+ x dx) y]]
    (when (piece-fits? board piece new-pos)
      (swap! app assoc :position new-pos))))

(defn try-rotate! []
  (let [piece (:piece @app)
        pos (:position @app)
        board (:board @app)
        new-piece (rotate-piece piece)]
    (when (piece-fits? board new-piece pos)
      (swap! app assoc :piece new-piece))))

(defn get-drop-pos
  [board piece [x y]]
  (let [collide? (fn [cy] (not (piece-fits? board piece [x cy])))
        cy (first (filter collide? (iterate inc y)))]
    (max y (dec cy))))

(defn filled-rows
  [board]
  (->> (map-indexed vector board)
       (filter (fn [[i row]] (every? pos? row)))
       (map first)
       (apply hash-set)))

(defn collapse-rows
  [board]
  (let [filled? (filled-rows board)
        cleared (->> board
                     (map-indexed vector)
                     (remove #(filled? (first %)))
                     (map second))
        n (count filled?)]
    (into (vec (repeat n empty-row)) cleared)))

(defn collapse-rows! []
  (swap! app update :board collapse-rows))

(defn spawn-piece! []
  (swap! app assoc :position initial-pos
                   :piece (rand-nth (vals pieces))))

(defn piece-done! []
  (lock-piece!)
  (collapse-rows!)
  (spawn-piece!))

(defn soft-drop! []
  (let [{:keys [piece board position]} @app
        [x y] position
        new-pos [x (inc y)]]
    (if (piece-fits? board piece new-pos)
      (swap! app assoc :position new-pos)
      (piece-done!))))

(defn hard-drop! []
  (let [piece (:piece @app)
        [x y] (:position @app)
        board (:board @app)
        ny (get-drop-pos board piece [x y])]
    (swap! app assoc :position [x ny])
    (piece-done!)))

(defn data-row
  [board row]
  [:span
    "["
    (for [col (range cols)]
      (str " " (get-in board [row col])))
    " ]"])

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
     (sx/cmt "; TRY IT: press space to hard-drop.") "\n"
     (sx/cmt ";         press left/right/down to move.") "\n"
     (sx/cmt ";         press up to rotate.") "\n"
     "\n"
     "(" (sx/core "defn") " collapse-rows\n"
     "  [board]\n"
     "  (" (sx/core "let") " [filled? (filled-rows board)\n"
     "        cleared (" (sx/core "->>") " board\n"
     "                     (" (sx/core "map-indexed") " " (sx/core "vector") ")\n"
     "                     (" (sx/core "remove") " #(filled? (" (sx/core "first") " %)))\n"
     "                     (map " (sx/core "second") "))\n"
     "        n (" (sx/core "count") " filled?)]\n"
     "    (" (sx/core "into") " (" (sx/core "vec") " (" (sx/core "repeat") " n empty-row)) cleared)))\n"
     "\n"
     "(" (sx/core "defn") " collapse-rows! []\n"
     "  (" (sx/core "swap!") " game " (sx/core "update-in") " [" (sx/kw ":board") "] collapse-rows))\n"
     "\n"
     (sx/old
       "(" (sx/core "defn") " piece-done! []\n"
       "  (lock-piece!)\n")
     (sx/new
       "  (collapse-rows!)\n")
     (sx/old
       "  (spawn-piece!))\n")]]])


(def cell-size (quot 600 rows))

(defn draw-cell!
  [ctx [x y] is-center]
  (set! (.. ctx -lineWidth) 2)
  (let [rx (* cell-size x)
        ry (* cell-size y)
        rs cell-size
        cx (* cell-size (+ x 0.5))
        cy (* cell-size (+ y 0.5))
        cr (/ cell-size 4)]

    (.. ctx (fillRect rx ry rs rs))
    (.. ctx (strokeRect rx ry rs rs))
    (when is-center
      (.. ctx beginPath)
      (.. ctx (arc cx cy cr 0 (* 2 (.-PI js/Math))))
      (.. ctx fill)
      (.. ctx stroke))))



(defn piece-abs-coords
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) piece))

(defn draw-piece!
  [ctx piece pos]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos))))

(defn draw-board!
  [ctx board]
  (let [filled? (filled-rows board)]
    (doseq [y (range rows)
            x (range cols)]
      (set! (.. ctx -globalAlpha) (if (filled? y) 0.25 1))
      (let [v (get-in board [y x])]
        (when-not (zero? v)
          (draw-cell! ctx [x y] false))))))


(defn draw-canvas!
  [canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (set! (.. ctx -fillStyle)   dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board @app))
    (set! (.. ctx -globalAlpha) 1)

    (let [piece (:piece @app)
          pos (:position @app)
          drop-y (get-drop-pos (:board @app) piece pos)
          drop-pos (assoc pos 1 drop-y)
          fits (app-piece-fits?)]

      (when (and piece pos)
        (set! (.. ctx -fillStyle)   "#333")
        (set! (.. ctx -strokeStyle) "#666")
        (draw-piece! ctx piece drop-pos)
        (set! (.. ctx -fillStyle)   (if fits dark-purple dark-red))
        (set! (.. ctx -strokeStyle) (if fits light-purple light-red))
        (draw-piece! ctx piece pos)))))


(def key-names
  {37 :left
   38 :up
   39 :right
   40 :down
   32 :space})

(def key-name #(-> % .-keyCode key-names))

(defn key-down [e]
  (let [kname (key-name e)]
   (case kname
     :left  (try-shift! -1)
     :right (try-shift! 1)
     :up    (try-rotate!)
     :down  (soft-drop!)
     :space (hard-drop!)
     nil)
   (when (#{:down :left :right :space :up} kname)
     (.preventDefault e))))

(def canvas-mixin
  {:did-mount
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (set! (.. canvas -width) (* cols cell-size))
      (set! (.. canvas -height) (* rows cell-size))
      (draw-canvas! canvas)
      state))
   :did-update
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (draw-canvas! canvas)
      state))})

(rum/defc canvas < canvas-mixin []
  [:.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}}]])

(rum/defc slide []
  [:div
   [:h1 "14. Collapse rows."]
   (code)
   (canvas)])

(def slide-elm)
(defn render []
  (rum/mount (slide) slide-elm))

(defn init [id]
  (set! slide-elm (js/document.getElementById id))
  (render)
  (add-watch app :render render))

(defn resume []
  (.addEventListener js/window "keydown" key-down))

(defn stop []
  (.removeEventListener js/window "keydown" key-down))
