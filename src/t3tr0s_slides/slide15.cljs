(ns t3tr0s-slides.slide15
  (:require
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

(def rows 20)
(def cols 10)
(def empty-row (vec (repeat cols 0)))
(def empty-board (vec (repeat rows empty-row)))
(def filled-board
  [[ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 1 0 0 1 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 1 0 0 0 0 1 0 0 ]
   [ 0 0 0 1 1 1 1 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 0 0 0 ]
   [ 0 0 0 0 0 0 0 1 0 0 ]
   [ 0 0 0 1 0 0 1 1 0 0 ]
   [ 1 0 1 1 1 0 1 1 0 0 ]
   [ 1 0 1 1 1 0 1 1 0 0 ]
   [ 1 1 1 1 1 1 1 1 1 0 ]
   [ 1 1 1 1 1 1 1 1 1 0 ]
   [ 1 1 1 1 1 1 1 1 1 0 ]
   [ 1 1 1 1 1 1 1 1 1 0 ]])

(def initial-pos [4 6])

(def app-state (atom {:board filled-board
                      :piece (last (take 4 (iterate rotate-piece (:L pieces))))
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

(defn data-row
  [row app]
  [:span
    "["
    (for [col (range cols)]
      (str " " (get-in @app-state [:board row col])))
    " ]"])

(defcomponent code
  [app owner]
  (render
    [_]
    (html
      [:div.code-cb62a
       [:pre
        [:code
         (sx/cmt ";; You can probably guess how to implement the") "\n"
         (sx/cmt ";; rest of the features, like gravity and") "\n"
         (sx/cmt ";; the game over animation.") "\n"
         "\n"
         (sx/cmt ";; Now go play some " (sx/kw [:a {:href "https://github.com/imalooney/t3tr0s"} "T3TR0S"]) "!") "\n"
         "\n"
         (sx/cmt ";; Thanks to these cool people:") "\n"
         "\n"
         (sx/cmt ";;  - " (sx/lit "Elaine Looney")) "\n"
         (sx/cmt ";;  - " (sx/lit "Luis Gutierrez")) "\n"
         (sx/cmt ";;  - " (sx/lit "Chris Oakman")) "\n"
         (sx/cmt ";;  - " (sx/lit "Brett Darnell")) "\n"
         (sx/cmt ";;  - " (sx/lit "Phil Gambling")) "\n"
         "\n\n"
         (sx/cmt ";; If you really want to look at the code") "\n"
         (sx/cmt ";; for this presentation...") "\n"
         (sx/cmt ";; ") (sx/core [:a {:href "https://github.com/shaunlebron/t3tr0s-slides"} "here it is."]) "\n"
         ]]])))

(def cell-size (quot 600 rows))

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
  (doseq [y (range rows)
          x (range cols)]
    (let [v (get-in board [y x])]
      (when-not (zero? v)
        (draw-cell! ctx [x y] false)))
    ))

(defn draw-canvas!
  [app canvas]
  (let [ctx (.. canvas (getContext "2d"))]

    (set! (.. ctx -fillStyle) "#222")
    (.. ctx (fillRect 0 0 (* cell-size cols) (* cell-size rows)))

    (set! (.. ctx -fillStyle) dark-green)
    (set! (.. ctx -strokeStyle) light-green)
    (draw-board! ctx (:board app))
    ))

(defcomponent canvas
  [app owner]
  (did-mount [_]
    (let [canvas (om/get-node owner "canvas")]
      (set! (.. canvas -width) (* cols cell-size))
      (set! (.. canvas -height) (* rows cell-size))
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
         }
        ]])))

(defcomponent slide
  [app owner]
  (render
    [_]
    (html
      [:div
       [:h1 "That's all for now..."]
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
  )

(defn stop
  []
  )
