(ns dunjeon.core
  (:import [java.awt Color Graphics2D Dimension Font]
           [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent KeyAdapter])
  (:require [clojure.set :as set])
  (:gen-class))

(set! *warn-on-reflection* true)

;; constants and utility functions

(def key-table
   {[:move :east] #{KeyEvent/VK_NUMPAD6 KeyEvent/VK_D KeyEvent/VK_RIGHT KeyEvent/VK_L KeyEvent/VK_F}
    [:move :west] #{KeyEvent/VK_NUMPAD4 KeyEvent/VK_A KeyEvent/VK_LEFT KeyEvent/VK_H KeyEvent/VK_B}
    [:move :north] #{KeyEvent/VK_NUMPAD8 KeyEvent/VK_W KeyEvent/VK_UP KeyEvent/VK_K KeyEvent/VK_P}
    [:move :south] #{KeyEvent/VK_NUMPAD2 KeyEvent/VK_S KeyEvent/VK_DOWN KeyEvent/VK_J KeyEvent/VK_N}
    [:action] #{KeyEvent/VK_SPACE  KeyEvent/VK_NUMPAD5 KeyEvent/VK_PERIOD KeyEvent/VK_ENTER}})

(def game-width (int 50))
(def game-height (int 50))

(defn random [min range] (+ min (rand-int range)))
(defn rand-elt [set] (rand-nth (vec set)))
(defn signum [x] (if (zero? x) x (if (pos? x) 1 -1)))
(defn abssq [[x y]] (+ (* x x) (* y y)))
(defn dist [p0 p1] (abssq (map - p0 p1))) ; really dist^2

(def panel-width (* 11 (+ 2 game-width)))
(def panel-height (* 11 (+ game-height 10)))

(def char-rep {::floor ".", nil "#", ::stairs ">", ::gold "$", ::booze "!",::player "@", ::monster "m" ::slither "s"})

(def color-rep {::floor Color/white, nil Color/gray, ::stairs Color/orange, ::gold Color/yellow ::booze Color/magenta
                ::monster Color/red, ::player Color/green})

(def auto-use #{::gold ::booze})

(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})
(def distribution {::gold 5, ::booze (random 3 4), ::stairs 1})

;; random dungeon generation

(defmulti pointify :type)

(defmethod pointify :rect [{x :x, y :y, width :w, height :h}]
  (mapcat (fn [[row y]] (map #(vector % y) row))
          (partition 2 (interleave (repeat height (range x (+ x width))) (range y (+ y height))))))

(defmethod pointify :circ [{x :x, y :y width :w height :h}]
  (let [rad (/ width 2)]
    (for [xx (range (- rad) rad), yy (range (- rad) rad)
          :when (< (+ (* xx xx) (* yy yy)) (* rad rad))] [(+ x rad xx) (+ y rad yy)])))

(defn intersects [r0 r1] (not (empty? (set/intersection (set (pointify r0)) (set (pointify r1))))))
(defn empty-map [w h] {:width w, :height h, :rooms #{}})

(defn room [{:keys [width height]}]
  (if (zero? (rand-int 3))
      (let [s (random 3 10)] {:type :circ :x (random 2 (- width 14)) :y (random 2 (- height 14)) :w s, :h s})
      {:type :rect :x (random 2 (- width 14)) :y (random 2 (- height 14)) :w (random 3 10) :h (random 3 10)}))

(defn add-rooms
  ([level n] (nth (iterate add-rooms level) n))
  ([{rooms :rooms :as level}]
     (letfn [(available? [r] (not-any? #(intersects r %) rooms))]
       (loop [r (room level), i 0]
         (cond (available? r) (assoc level :rooms (conj rooms r))
               (> i 1000) level
               :else (recur (room level) (inc i)))))))

(defn connect [{:keys [width height]} r0 r1]
  (let [[x0 y0] (rand-nth (pointify r0)),
        [x1 y1] (rand-nth (pointify r1)),
        dx (signum (- x1 x0)), dy (signum (- y1 y0))]
    (loop [x x0, y y0, points #{}, horizontal? (not= (rand-int 2))]
      (if (and (= x x1) (= y y1)) points
          (let [[x y] (if (or (= y y1) (and horizontal? (not= x x1))) [(+ x dx) y] [x (+ y dy)])]
            (if-not (and (> x 0) (> y 0) (< x width) (< y height)) points
                    (recur x y (conj points [x y]) (not= 0 (rand-int 10)))))))))

(defn connect-rooms [{:keys [width, height, rooms] :as level}]
  (loop [from (rand-elt rooms), conn #{from}, unconn (disj rooms from), paths #{}]
    (if (empty? unconn) (assoc level :paths paths)
      (let [to (rand-elt unconn), unconn (disj unconn to), conn (conj conn to)]
        (recur (rand-elt conn), conn, unconn, (conj paths (connect level from to)))))))

(defn levelify-map [{:keys [width, height, rooms, paths]}]
  {:width width, :height height, :seen #{},
   :points (merge (zipmap (mapcat pointify rooms) (repeat ::floor))
                  (zipmap (apply concat paths) (repeat ::floor)))})

(defn place-randomly [level n tile]
  (reduce (fn [{p :points :as l} t] (assoc-in l [:points ((rand-elt p) 0)] t))
          level (repeat n tile)))

(defn make-monster [pos] {:pos pos, :health 10})

(defn add-monsters [{:keys [points] :as level} n]
  (let [mpts (map #(% 0) (take n (shuffle (vec points))))]
    (assoc level :monsters (set (map make-monster mpts)))))


(defn finalize [level]
  (add-monsters (reduce (fn [lvl [item, num]] (place-randomly lvl num item)) level distribution)
                (random 5 5)))

(defn gen-level [width height rooms]
  (-> (empty-map width height) (add-rooms rooms) connect-rooms levelify-map finalize))

;; game logic

(defn can-see? [{pts :points} source end]
  (or (= source end)
      (let [dest (map #(+ %2 (/ (signum (- % %2)) 2.0)) source end)
            [distx disty :as dist] (map - dest source)
            length (max (Math/abs (double distx)) (Math/abs (double disty)))
            delta (map #(/ % length) dist)]
        (loop [len length, pos source]
          (or (neg? len)
              (let [moved (map (comp int (partial + 0.5)) pos)]
                (cond (= moved dest) true
                      (and (not (= moved source)) (not (pts moved))) false
                      :else (recur (dec len) (map + delta pos)))))))))

(defn update-vision [{{seen :seen :as lvl} :level,{[x y] :pos :as player} :player :as game-state}]
  (let [visible (for [xx (range -10 10), yy (range -10 10)
                      :when (and (<= (abssq [xx yy]) 100) (can-see? lvl [x y] [(+ x xx) (+ y yy)]))]
                  [(+ xx x) (+ yy y)])]
    (-> game-state
        (assoc-in [:level :seen] (reduce conj seen visible))
        (assoc-in [:player :sees] (set visible)))))

(defn add-msg
  ([gs msg] (add-msg gs Color/white msg))
  ([gs col msg] (update-in gs [:messages] conj {:col col :txt msg})))

(defn initialize-gamestate []
  (let [level (gen-level game-width game-height (random 5 4))]
    (-> {:level level
         :player {:pos ((rand-elt (:points level)) 0),:health 50, :sees #{} :score 0, :level 0, :dead false}
         :messages ()}
        update-vision
        (add-msg Color/blue "(entering (dungeon))")
        (add-msg "Entered level 0."))))

(defn clear-tile [gs pos] (assoc-in gs [:level :points pos] ::floor))


(defmulti use-tile (fn [gs pos item] item))
(defmethod use-tile :default [gs _ _] gs)
(defmethod use-tile ::floor [gs _ _] gs)
(defmethod use-tile ::booze [{{h :health l :level} :player :as game-state} pos _]
  (let [healed (random 8 8) hnow (min (+ (* 5 l) 50) (+ h healed)) del (- hnow h)]
    (-> game-state
        (clear-tile pos)
        (assoc-in [:player :health] hnow)
        (add-msg (str "The booze heals you for " (if (zero? del) "no" del) " points.")))))

(defmethod use-tile ::gold [game-state pos _]
  (-> game-state
      (clear-tile pos)
      (update-in [:player :score] + 25)
      (add-msg (str "You find 25 gold on the ground. Score!"))))

(defmethod use-tile ::stairs [{p :player :as game-state} _ _]
  (let [next-floor (gen-level 50 50 (random 4 5))]
    (-> game-state
        (assoc :level next-floor)
        (assoc-in [:player :pos] ((rand-elt (:points next-floor)) 0))
        (update-in [:player :level] inc)
        (add-msg "You go down the stairs."))))

(defn fight-pm [{{ms :monsters} :level :as gs} {h :health :as mon}]
  (let [d (random 3 5), left (- h d), damaged (assoc mon :health left), alive? (pos? left), ms (disj ms mon)]
    (-> gs
        (assoc-in [:level :monsters] (if alive? (conj ms damaged) ms))
        (add-msg (str "The monster " (if alive? (str "takes " d " damage.") "dies.")))
        (update-in [:player :score] + (if alive? 0 (random 25 25))))))

(defn monster-at [{{mons :monsters} :level} pos]
  (loop [[{p :pos :as m} & ms] (seq mons)]
    (cond (= p pos) m, (not ms) nil, :else (recur ms))))

(defn autoheal [{{l :level h :health} :player :as gs}]
  (if (and (< h 50) (zero? (rand-int 5))) (update-in gs [:player :health] + (rand-int (+ l 3))) gs))

(defmulti tick-player (fn [gamestate [kind & args]] kind))

(defmethod tick-player :default [game-state _] game-state)
(defmethod tick-player :action [{{p :pos} :player, {pts :points} :level :as gs} _] (use-tile gs p (pts p)))
(defmethod tick-player :move [{{pos :pos h :health} :player, level :level :as gs} [_ dir]]
  (let [newpos (map + pos (directions dir)), tile ((:points level) newpos), mon (monster-at gs newpos)]
    (cond (not tile) gs
          mon (fight-pm gs mon)
          (auto-use tile) (use-tile (assoc-in gs [:player :pos] newpos) newpos tile)
          :else (assoc-in gs [:player :pos] newpos))))

(defn die [{{:keys [level score]} :player :as gs}]
  (-> gs (assoc-in [:player :dead] true) (assoc-in [:player :health] 0)
      (add-msg Color/red (str "You have died on level " level " with " score " points."))
      (add-msg Color/red "Press enter to try again.")))

(defn attack-player [{{l :level h :health :as player} :player :as gs}]
  (if (zero? (rand-int 4)) (add-msg gs "You dodge the monster's attack!")
      (let [dam (random (+ l 3) (+ l 7))
            next-gs (-> gs (update-in [:player :health] - dam)
                        (add-msg (str "The monster attacks you for " dam " damage!")))]
        (if-not (pos? (- (:health (:player gs)) dam)) (die (assoc-in next-gs [:player :dead] true))
                next-gs))))

(defn tick-monster [{{ms :monsters pts :points :as lvl} :level {pp :pos} :player :as gs} {mp :pos :as mon}]
  (let [vis (can-see? lvl mp pp),
        del (if vis (map (comp signum -) pp mp) (rand-nth (vals directions))),
        pos (map + mp del)]
    (cond (= pos pp) (attack-player gs)
          (pts pos) (-> gs (update-in [:level :monsters] disj mon) (update-in [:level :monsters] conj (assoc mon :pos pos)))
          :else gs)))

(defn tick-monsters [{{ms :monsters, pts :points} :level {pp :pos} :player :as gs}] (reduce tick-monster gs ms))

(defn tick [{{dead? :dead} :player :as game-state} input]
  (cond (not dead?) (-> game-state (tick-player input) autoheal tick-monsters update-vision)
        (and dead? (= input [:action])) (initialize-gamestate)
        :else game-state))

(defn comprehend [code] (first (for [[key codes] key-table :when (codes code)] key)))

(defn draw [^Graphics2D g {{[px py :as pos] :pos h :health s :score sees :sees, lv :level :as play} :player,
                           {:keys [points width height monsters seen]} :level, msgs :messages}]
  (doto g
    (.setColor Color/black)
    (.fillRect 0 0 panel-width panel-height)
    (.setFont (Font. "Monospaced" Font/PLAIN 13))
    (.setColor Color/gray)
    (.drawRect 5 5 (int (* 11 (inc game-width))) (int (* 11 (+ 2 game-height))))
    (.drawRect 5  (int (+ 5 (* 11 (+ 2 game-height)))) (* 15 11) (* 5 11))
    (.drawRect (+ 5 (* 15 11)) (int (+ 5 (* 11 (+ 2 game-height)))) (int (* 11 (- game-width 14))) (* 5 11))
    (.drawRect 5 (+ 5 (int (* 11 (+ 7 game-height)))) (* 11 (inc game-width)) 22)
    (.translate 11.0 11.0))
  (let [monpts (set/intersection sees (set (map :pos monsters)))
        post-process (fn [col sees? seen?]
                       (cond (not sees?) (.darker (.darker ^Color col))
                             :else col))]
    (doseq [xx (range width), yy (range (inc height))]
      (let [p [xx yy],
            type (cond (= p pos) ::player, (monpts p) ::monster, :else (points p))
            p-vis (sees p), p-rem (seen p)]
        (when (or p-vis p-rem)
          (.setColor g (post-process (color-rep type) p-vis p-rem))
          (.drawString g ^String (char-rep type) (int (* xx 11)) (int (* (inc yy) 11)))))))
  (.setColor g Color/white)
  (dorun 3 (map-indexed
            (fn [i {c :col m :txt}]
              (doto g (.setColor c) (.drawString ^String m (* 15 11) (int (* 11 (+ (- 6 i) game-height)))))) msgs))
  (doto g
    (.setColor Color/white)
    (.drawString (str "Health: " h) 0 (int (* 11 (+ 3 game-height))))
    (.drawString (str "Score:  " s) 0 (int (* 11 (+ game-height 4))))
    (.drawString (str "Level:  " lv) 0 (int (* 11 (+ 5 game-height))))
    (.setColor Color/green)
    (.drawString "Move is arrows/hjkl/numpad/wasd. Action is ret/spc/num5." 0 (int (* 11 (+ 8 game-height))))))

(defn create-ui []
  (let [game-state (atom (initialize-gamestate))
        dim (Dimension. panel-width panel-height)
        panel (doto (proxy [JPanel] [] (paint [g] (draw g @game-state)))
                (.setMinimumSize dim)
                (.setPreferredSize dim)
                (.setMaximumSize dim))
        ka (proxy [KeyAdapter] []
             (keyPressed [^KeyEvent e]
               (when-let [input (comprehend (.getKeyCode e))]
                 (swap! game-state tick input)
                 (.repaint panel))))]
    {:panel panel, :ka ka}))

(defn init-frame []
  (let [{:keys [panel ka]} (create-ui)]
    (doto (JFrame. "(dunjeon)")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add ^JPanel panel) .pack
      (.setResizable false)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.addKeyListener ^KeyListener ka))))

(defn -main [& args] (init-frame)) 


