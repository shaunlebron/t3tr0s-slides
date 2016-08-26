(ns t3tr0s-slides.slide07
  (:require
    [rum.core :as rum]
    [t3tr0s-slides.syntax-highlight :as sx]))

(def dark-green "#143")
(def light-green "#175")
(def dark-purple "#449")
(def light-purple "#6ad")

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

(def initial-pos [4 6])

(def app (atom {:board empty-board
                :piece (:T pieces)
                :position initial-pos}))

(defn write-piece
  [board coords [cx cy]]
  (reduce (fn [board [x y]]
            (try
              (assoc-in board [(+ y cy) (+ x cx)] 1)
              (catch js/Error _ board)))
          board
          coords))

(defn lock-piece! []
  (let [{:keys [piece position]} @app]
    (swap! app
      update-in [:board]
        write-piece piece position)))

(defn data-row
  [row]
  [:span
    "["
    (for [col (range cols)]
      (let [x (get-in @app [:board row col])
            highlight (if (= 0 x) sx/out sx/core)]
        (list " " (highlight x))))
    " ]"])

(rum/defc code []
  [:.code-cb62a
   [:pre
    [:code
     (sx/cmt "; TRY IT: click-drag canvas on the right") "\n\n"
     "(" (sx/core "defn") " write-piece\n"
     "  [board coords [cx cy]]\n"
     "  (" (sx/core "reduce") " (" (sx/core "fn") " [board [x y]]\n"
     "            (" (sx/core "assoc-in") " board [(" (sx/core "+") " y cy) (" (sx/core "+") " x cx)] " (sx/lit "1") "))\n"
     "    board coords))\n"
     "\n"
     "(" (sx/core "defn") " lock-piece! []\n"
     "  (" (sx/core "let") " [{" (sx/kw ":keys") " [piece position]} @game]\n"
     "    (" (sx/core "swap!") " game " (sx/core "update-in") " [" (sx/kw ":board") "]\n"
     "        write-piece piece position)))\n"
     "\n"
     "> (" (sx/kw ":board") " @game)\n"
     (for [row (range rows)]
       (condp = row
         0          (list "  [" (data-row row) "\n")
         (dec rows) (list "   " (data-row row) "])\n")
         (list "   " (data-row row) "\n")))]]])

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
  (doseq [y (range rows)
          x (range cols)]
    (let [v (get-in board [y x])]
      (when-not (zero? v)
        (draw-cell! ctx [x y] false)))))


(defn draw-canvas!
  [canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (set! (.. ctx -fillStyle) dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board @app))

    (let [piece (:piece @app)
          pos (:position @app)]
      (when (and piece pos)
        (set! (.. ctx -fillStyle) dark-purple)
        (set! (.. ctx -strokeStyle) light-purple)
        (draw-piece! ctx piece pos)))))

(def global-canvas)
(defn canvas-mouse [e]
  (let [clicking (:clicking @app)
        canvas global-canvas
        rect (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left rect))
        y (- (.-clientY e) (.-top rect))
        col (quot x cell-size)
        row (quot y cell-size)]
    (swap! app assoc :position [col row])
    (when clicking
      (lock-piece!))))

(def canvas-mixin
  {:did-mount
   (fn [state]
     (let [canvas (rum/ref state "canvas")]
      (set! global-canvas canvas)
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
  [:div.canvas-2a4d7
   [:canvas
    {:ref "canvas"
     :style {:position "relative"}
     :onMouseDown #(do (swap! app assoc :clicking true)
                       (lock-piece!))
     :onMouseUp    #(swap! app assoc :clicking false)
     :onMouseLeave #(swap! app assoc :clicking false)
     :onMouseMove #(canvas-mouse %)}]])

(rum/defc slide []
  [:div
   [:h1 "7. Write piece to board."]
   (code)
   (canvas)])

(def slide-elm)
(defn render []
  (rum/mount (slide) slide-elm))

(defn init [id]
  (set! slide-elm (js/document.getElementById id))
  (render)
  (add-watch app :render render))

(defn resume [])
(defn stop [])
