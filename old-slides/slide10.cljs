(ns t3tr0s-slides.slide10
  (:require
    [om.core :as om :include-macros true]
    [om-tools.core :refer-macros [defcomponent]]
    [sablono.core :refer-macros [html]]
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
   [ 0 0 1 0 0 0 0 0 0 0]
   [ 0 0 1 0 0 0 0 0 0 0]
   [ 0 1 1 1 0 0 0 0 1 0]
   [ 1 1 1 1 0 0 0 0 1 0]
   [ 1 1 1 1 1 0 0 0 1 0]
   [ 1 1 1 1 1 1 1 1 1 0]])

(def initial-pos [4 2])

(def app-state (atom {:board filled-board
                      :piece (:T pieces)
                      :position initial-pos}))

(defn write-piece
  [board coords [cx cy]]
  (if-let [[x y] (first coords)]
    (recur (try (assoc-in board [(+ y cy) (+ x cx)] 1)
                (catch js/Error _ board))
           (rest coords)
           [cx cy])
    board))

(defn lock-piece! []
  (let [{:keys [piece position]} @app-state]
    (swap! app-state
      update-in [:board]
        write-piece piece position)))

(defn piece-fits?
  [board piece [cx cy]]
  (every? (fn [[x y]]
            (zero? (get-in board [(+ y cy) (+ x cx)])))
          piece))

(defn app-piece-fits?
  []
  (boolean (piece-fits? (:board @app-state) (:piece @app-state) (:position @app-state))))

(defn try-shift! [dx]
  (let [piece (:piece @app-state)
        [x y] (:position @app-state)
        board (:board @app-state)
        new-pos [(+ x dx) y]]
    (when (piece-fits? board piece new-pos)
      (swap! app-state assoc :position new-pos))))

(defn try-rotate! []
  (let [piece (:piece @app-state)
        pos (:position @app-state)
        board (:board @app-state)
        new-piece (rotate-piece piece)]
    (when (piece-fits? board new-piece pos)
      (swap! app-state assoc :piece new-piece))))

(defn get-drop-pos
  [board piece [x y]]
  (let [collide? (fn [cy] (not (piece-fits? board piece [x cy])))
        cy (first (filter collide? (iterate inc y)))]
    (max y (dec cy))))

(defn hard-drop! []
  (let [piece (:piece @app-state)
        [x y] (:position @app-state)
        board (:board @app-state)
        ny (get-drop-pos board piece [x y])]
    (swap! app-state assoc :position [x ny])
    (lock-piece!)
    (swap! app-state assoc :position initial-pos
                           :piece (rand-nth (vals pieces)))))


(defcomponent code
  [app owner]
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code
         (sx/cmt "; TRY IT: press space to hard-drop.") "\n"
         (sx/cmt ";         press left/right to move.") "\n"
         (sx/cmt ";         press up to rotate.") "\n"
         "\n"
         "(" (sx/core "defn") " get-drop-pos\n"
         "  [board piece [x y]]\n"
         "  (" (sx/core "let") " [clear? #(piece-fits? board piece [x %]))\n"
         "        cy (" (sx/core "first") " (" (sx/core "remove") " clear? (" (sx/core "iterate") " " (sx/core "inc") " y)))]\n"
         "    (" (sx/core "max") " y (" (sx/core "dec") " cy))))\n"
         "\n\n"
         "(" (sx/core "defn") " hard-drop! []\n"
         "  (" (sx/core "let") " [piece (" (sx/kw ":piece") " @app-state)\n"
         "        [x y] (" (sx/kw ":position") " @app-state)\n"
         "        board (:board @app-state)\n"
         "        ny (get-drop-pos board piece [x y])]\n"
         "    (" (sx/core "swap!") " app-state " (sx/core "assoc") " " (sx/kw ":position") " [x ny])\n"
         "    (lock-piece!)))\n"
         "\n\n"]]])))


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
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (set! (.. ctx -fillStyle) dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board app))

    (let [piece (:piece app)
          pos (:position app)
          fits (app-piece-fits?)]

      (when (and piece pos)
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
     :space (hard-drop!)
     nil)
   (when (#{:down :left :right :space :up} kname)
     (.preventDefault e))))

(defcomponent canvas
  [app owner]
  (did-mount [_]
    (let [canvas (om/get-node owner "canvas")]
      (set! (.. canvas -width) (* cols cell-size))
      (set! (.. canvas -height) (* rows cell-size))
      (draw-canvas! app (om/get-node owner "canvas"))))

  (did-update [_ _ _]
    (draw-canvas! app (om/get-node owner "canvas")))

  (render [_]
    (html
      [:div.canvas-2a4d7
       [:canvas
        {:ref "canvas"
         :style {:position "relative"}}]])))



(defcomponent slide
  [app owner]
  (render
    [_]
    (html
      [:div
       [:h1 "10. Add hard-drop."]
       (om/build code app)
       (om/build canvas app)])))

(defn init
  [id]
  (om/root
    slide
    app-state
    {:target (. js/document (getElementById id))}))

(defn resume
  []
  (.addEventListener js/window "keydown" key-down))


(defn stop
  []
  (.removeEventListener js/window "keydown" key-down))
