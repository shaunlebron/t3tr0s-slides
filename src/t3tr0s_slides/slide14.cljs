(ns t3tr0s-slides.slide14
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! take! close! <! >! alts! chan timeout]]
    [om.core :as om :include-macros true]
    [om-tools.core :refer-macros [defcomponent]]
    [sablono.core :refer-macros [html]]
    [t3tr0s-slides.syntax-highlight :as sx]
    ))

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

(def nrows 20)
(def ncols 10)
(def empty-row (vec (repeat ncols 0)))
(def empty-board (vec (repeat nrows empty-row)))
(def filled-board
  [[ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 1 0 0 0 0 0 1 0 ]
   [ 0 1 1 1 0 1 1 0 1 1 ]
   [ 1 1 1 1 0 1 1 1 1 1 ]
   [ 1 1 1 1 0 1 1 0 1 1 ]
   [ 1 1 1 1 0 1 1 1 1 1 ]])

(def initial-pos [5 2])

(def app-state (atom {:board filled-board
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
  (let [board (:board @app-state)
        piece (:piece @app-state)
        pos (:position @app-state)]
    (create-drawable-board board piece pos)))

(defn lock-piece! []
  (let [{:keys [piece position]} @app-state]
    (swap! app-state
      update-in [:board]
        write-piece piece position 1)))

(defn piece-fits?
  [board piece [cx cy]]
  (every? (fn [[x y]]
            (zero? (get-in board [(+ y cy) (+ x cx)])))
          piece))

(defn app-piece-fits?
  []
  (boolean (piece-fits? (:board @app-state) (:piece @app-state) (:position @app-state))))

(defn try-shift! [dx]
  (when-let [piece (:piece @app-state)]
    (let[[x y] (:position @app-state)
         board (:board @app-state)
         new-pos [(+ x dx) y]]
      (when (piece-fits? board piece new-pos)
        (swap! app-state assoc :position new-pos)))))

(defn try-rotate! []
  (when-let [piece (:piece @app-state)]
    (let [pos (:position @app-state)
          board (:board @app-state)
          new-piece (rotate-piece piece)]
      (when (piece-fits? board new-piece pos)
        (swap! app-state assoc :piece new-piece)))))

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
  [board rows]
  (let [cleared-board (->> board
                           (map-indexed vector)
                           (remove #(rows (first %)))
                           (map second))
        n (count rows)
        new-board (into (vec (repeat n empty-row)) cleared-board)]
    new-board))

(defn clear-rows
  [board rows]
  (vec (map-indexed
   (fn [i row]
     (if (rows i) empty-row row)) board)))

(def prev-filled (atom nil))
(def prev-cleared (atom nil))
(def prev-collapsed (atom nil))
(def anim-index (atom nil))

(defn on-change-anim-index
  [_ _ _ i]
  (when i
    (reset! app-state (get [@prev-cleared
                            @prev-filled
                            @prev-cleared
                            @prev-collapsed] i))))

(add-watch anim-index :history on-change-anim-index)


(defn go-go-collapse! []

  (let [board     (:board @app-state)
        rows      (filled-rows board)
        cleared   (clear-rows board rows)
        collapsed (collapse-rows board rows)]

    (reset! prev-filled    (assoc @app-state :board board))
    (reset! prev-cleared   (assoc @app-state :board cleared))
    (reset! prev-collapsed (assoc @app-state :board collapsed))

    (go
      (dotimes [i 3]

        (reset! anim-index 0)
        (<! (timeout 170))

        (reset! anim-index 1)
        (<! (timeout 170)))

      (reset! anim-index 2)
      (<! (timeout 220))

      (reset! anim-index 3)
      (<! (timeout 170))

      (reset! anim-index nil)
      )))

(defn spawn-piece!
  []
  (swap! app-state assoc
         :position initial-pos
         :piece (rand-nth (vals pieces))))

(defn hard-drop! []
  (when-let [piece (:piece @app-state)]
    (let [[x y] (:position @app-state)
          board (:board @app-state)
          ny (get-drop-pos board piece [x y])]
      (swap! app-state assoc :position [x ny])
      (lock-piece!)

      (let [board (:board @app-state)
            rows (filled-rows board)]
        (if (empty? rows)
          (spawn-piece!)
          (do
            (swap! app-state assoc
                   :piece nil
                   :position nil)
            (go
              (<! (go-go-collapse!))
              (spawn-piece!))
            ))))))


(defn data-row
  [index content]
  [:span
   {:class (if (= @anim-index index) "active-row-534ed" "")
    :onMouseEnter #(when @prev-filled
                     (reset! anim-index index))
    }
   content])

(defcomponent code
  [app owner]
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code
         "(" (sx/core "defn") " clear-rows\n"
         "  [board rows]\n"
         "  (" (sx/core "vec") " (" (sx/core "map-indexed") "\n"
         "   (" (sx/core "fn") " [i row]\n"
         "     (" (sx/core "if") " (rows i) empty-row row)) board)))\n"
         "\n"
         "(" (sx/core "defn") " go-go-collapse! []\n"
         "\n"
         "  (" (sx/core "let") " [board     (" (sx/kw ":board") " @game-state)\n"
         "        rows      (filled-rows board)\n"
         "        cleared   (clear-rows board rows)\n"
         "        collapsed (collapse-rows board rows)]\n"
         "\n"
         "    (" (sx/core "go") "\n"
         "      (" (sx/core "dotimes") " [_ " (sx/lit "3") "]\n"
         "\n"
         "        " (data-row 0 (list "(" (sx/core "swap!") " game-state " (sx/core "assoc") " " (sx/kw ":board") " cleared)")) "\n"
         "        (" (sx/core "<!") " (" (sx/core "timeout") " " (sx/lit "170") "))\n"
         "\n"
         "        " (data-row 1 (list "(" (sx/core "swap!") " game-state " (sx/core "assoc") " " (sx/kw ":board") " board)")) "\n"
         "        (" (sx/core "<!") " (" (sx/core "timeout") " " (sx/lit "170") ")))\n"
         "\n"
         "      " (data-row 2 (list "(" (sx/core "swap!") " game-state " (sx/core "assoc") " " (sx/kw ":board") " cleared)")) "\n"
         "      (" (sx/core "<!") " (" (sx/core "timeout") " " (sx/lit "220") "))\n"
         "\n"
         "      " (data-row 3 (list "(" (sx/core "swap!") " game-state " (sx/core "assoc") " " (sx/kw ":board") " collapsed)")) ")))\n"
         ]]])))

(def cell-size (quot 600 nrows))

(defn draw-cell!
  [ctx [x y] is-center]
  (set! (.. ctx -lineWidth) 2)
  (let [rx (* cell-size x)
        ry (* cell-size y)
        rs cell-size
        cx (* cell-size (+ x 0.5))
        cy (* cell-size (+ y 0.5))
        cr (/ cell-size 4)
        ]
    (.. ctx (fillRect rx ry rs rs))
    (.. ctx (strokeRect rx ry rs rs))
    (when is-center
      (.. ctx beginPath)
      (.. ctx (arc cx cy cr 0 (* 2 (.-PI js/Math))))
      (.. ctx fill)
      (.. ctx stroke)
      )
    ))

(defn piece-abs-coords
  [piece [cx cy]]
  (mapv (fn [[x y]] [(+ cx x) (+ cy y)]) piece))

(defn draw-piece!
  [ctx piece pos ]
  (doseq [[i c] (map-indexed vector (piece-abs-coords piece pos))]
    (draw-cell! ctx c (= c pos))))

(defn draw-board!
  [ctx board]
  (let [filled? (filled-rows board)]
    (doseq [y (range nrows)
            x (range ncols)]
      (set! (.. ctx -globalAlpha) (if (filled? y) 0.3 1))
      (let [v (get-in board [y x])]
        (when-not (zero? v)
          (draw-cell! ctx [x y] false)))
      )))

(defn draw-canvas!
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size ncols) (* cell-size nrows)))

    (set! (.. ctx -fillStyle)   dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board app))
    (set! (.. ctx -globalAlpha) 1)

    (when-let [piece (:piece app)]
      (let [pos (:position app)
            drop-y (get-drop-pos (:board @app-state) piece pos)
            drop-pos (assoc pos 1 drop-y)
            fits (app-piece-fits?)
            ]
        (when (and piece pos)
          (set! (.. ctx -fillStyle)   "#333")
          (set! (.. ctx -strokeStyle) "#666")
          (draw-piece! ctx piece drop-pos)
          (set! (.. ctx -fillStyle)   (if fits dark-purple dark-red))
          (set! (.. ctx -strokeStyle) (if fits light-purple light-red))
          (draw-piece! ctx piece pos))))
    ))

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
      (set! (.. canvas -width) (* ncols cell-size))
      (set! (.. canvas -height) (* nrows cell-size))
      (draw-canvas! app (om/get-node owner "canvas"))
      ))
  (did-update [_ _ _]
    (draw-canvas! app (om/get-node owner "canvas"))
    )
  (render [_]
    (html
      [:div.canvas-2a4d7
       [:canvas
        {:ref "canvas"
         :style {:position "relative"}
         }
        ]])))

(defcomponent slide
  [app owner]
  (render
    [_]
    (html
      [:div
       [:h1 "14. Animate collapse."]
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
  (.addEventListener js/window "keydown" key-down)
  )

(defn stop
  []
  (.removeEventListener js/window "keydown" key-down)
  )
