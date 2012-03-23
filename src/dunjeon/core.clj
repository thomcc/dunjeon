(ns dunjeon.core
  (:import [java.awt Color Graphics2D Dimension Font]
           [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent KeyAdapter])
  (:require [clojure.set :as set]))

(set! *warn-on-reflection* true)

;; constants and utility functions

(def game-width (int 50))
(def game-height (int 50))

(defn random [min range] (+ min (rand-int range)))
(defn rand-elt [set] (rand-nth (vec set)))
(defn signum [x] (if (zero? x) x (if (pos? x) 1 -1)))

(def panel-width (* 11 game-width))
(def panel-height (* 11 (+ game-height 5)))

(def char-rep {::floor ".", nil "#", ::stairs ">", ::gold "$", ::booze "!",::player "@", ::monster "m"})

(def color-rep {::floor Color/white, nil Color/gray, ::stairs Color/white, ::gold Color/yellow ::booze Color/pink
                ::monster Color/red, ::player Color/green})
(def auto-use #{::gold})
(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})
(def distribution {::gold 5, ::booze (random 2 3), ::stairs 1})

;; random dungeon generation

(defn intersects [{x0 :x, y0 :y, w0 :w, h0 :h}, {x1 :x, y1 :y, w1 :w, h1 :h}]
  (and (or (and (>= x0 x1) (<= x0 (+ x1 w1))) (and (>= x1 x0) (<= x1 (+ x0 w0))))
       (or (and (>= y0 y1) (<= y0 (+ y1 h1))) (and (>= y1 y0) (<= y1 (+ y0 h0))))))

(defn empty-map [w h] {:width w, :height h, :rooms #{}})
(defn room [{:keys [width height]}]
  {:x (random 2 (- width 13)) :y (random 2 (- width 13)) :w (random 3 10) :h (random 3 10)})

(defn add-rooms
  ([level n] (nth (iterate add-rooms level) n))
  ([{rooms :rooms :as level}]
     (letfn [(available? [r] (not-any? #(intersects r %) rooms))]
       (loop [r (room level), i 0]
         (cond (available? r) (assoc level :rooms (conj rooms r))
               (> i 1000) level
               :else (recur (room level) (inc i)))))))

(defn connect [{:keys [width height]}, {fx :x, fy :y, fw :w, fh :h}, {tx :x, ty :y, tw :w, th :h}]
  (let [x0 (random fx fw), y0 (random fy fh)
        x1 (random tx tw), y1 (random ty th)
        dx (signum (- x1 x0)), dy (signum (- y1 y0))]
    (loop [x x0, y y0, points #{}, horizontal? (not= (rand-int 2))]
      (if (and (= x x1) (= y y1)) points
          (let [[x y] (if (or (= y y1) (and horizontal? (not= x x1))) [(+ x dx) y] [x (+ y dy)])]
            (if-not (and (> x 0) (> y 0) (< x width) (< y height)) points        ; i use not= instead of zero? here
                    (recur x y (conj points [x y]) (not= 0 (rand-int 10))))))))) ; because otherwise compiler complains.

(defn connect-rooms [{:keys [width, height, rooms] :as level}]
  (loop [from (rand-elt rooms), conn #{from}, unconn (disj rooms from), paths #{}]
    (if (empty? unconn) (assoc level :paths paths)
      (let [to (rand-elt unconn), unconn (disj unconn to), conn (conj conn to)]
        (recur (rand-elt conn), conn, unconn, (conj paths (connect level from to)))))))

(defn pointify [{x :x, y :y, width :w, height :h}]
  (mapcat (fn [[row y]] (map #(vector % y) row))
          (partition 2 (interleave (repeat height (range x (+ x width)))
                                   (range y (+ y height))))))

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
  (add-monsters (reduce (fn [lvl [item, num]] (place-randomly lvl num item)) level distribution) (random 5 5)))

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
                      :when (and (<= (+ (* xx xx) (* yy yy)) 100) (can-see? lvl [x y] [(+ x xx) (+ y yy)]))]
                  [(+ xx x) (+ yy y)])]
    (-> game-state
        (assoc-in [:level :seen] (reduce conj seen visible))
        (assoc-in [:player :sees] (set visible)))))

(defn initialize-gamestate []
  (let [level (gen-level game-width game-height (random 5 4))]
    (update-vision
     {:level level
      :player {:pos ((rand-elt (:points level)) 0),
               :health 50 :inv [], :sees #{} :score 0, :level 0}
      :messages '("You enter level 0.", "Welcome to the dunjeon.")})))

(defn clear-tile [gs pos] (assoc-in gs [:level :points pos] ::floor))
(defn add-msg [gs msg] (update-in gs [:messages] conj msg))

(defmulti use-tile (fn [gs pos item] item))
(defmethod use-tile :default [gs _ _] gs)
(defmethod use-tile ::floor [gs _ _] gs)
(defmethod use-tile ::booze [game-state pos _]
  (let [healed (random 2 7)]
    (-> game-state
        (clear-tile pos)
       (update-in [:player :health] + healed)
       (add-msg (str "The booze heals you for " healed " points.")))))

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

(defn fight [{{ms :monsters} :level :as gs} {h :health :as mon}]
  (let [d (random 3 5), left (- h d), damaged (assoc mon :health left), alive? (pos? left), ms (disj ms mon)]
    (-> gs
        (assoc-in [:level :monsters] (if alive? (conj ms damaged) ms))
        (add-msg (str "The monster " (if alive? (str "takes " d " damage.") "dies.")))
        (update-in [:player :score] + (if alive? 0 (random 25 25))))))

(defn monster-at [{{mons :monsters} :level} pos]
  (loop [[{p :pos :as m} & ms] (seq mons)]
    (cond (= p pos) m, (not ms) nil, :else (recur ms))))

(defmulti tick-player (fn [gamestate [kind & args]] kind))
(defmethod tick-player :default [game-state _] game-state)
(defmethod tick-player :action [{{p :pos} :player, {pts :points} :level :as gs} _] (use-tile gs p (pts p)))

(defmethod tick-player :move [{{pos :pos} :player, level :level :as gs} [_ dir]]
  (let [newpos (map + pos (directions dir)), tile ((:points level) newpos), mon (monster-at gs newpos)]
    (cond (not tile) gs
          mon (fight gs mon)
          (auto-use tile) (use-tile (assoc-in gs [:player :pos] newpos) newpos tile)
          :else (assoc-in gs [:player :pos] newpos))))

(defn tick-monsters [{{ms :monsters, pts :points} :level {pp :pos} :player :as gs}]
  (assoc-in gs [:level :monsters]
            (set (map (fn [{p :pos :as m}]
                        (let [d (rand-nth (vals directions)), npos (map + p d), can? (and (pts npos) (not= pp npos))]
                          (if can? (assoc m :pos npos) m)))
                      ms))))

(defn tick [game-state input] (-> game-state (tick-player input) tick-monsters update-vision))

(defn comprehend [^KeyEvent input]
  ({KeyEvent/VK_UP [:move :north], KeyEvent/VK_DOWN [:move :south], KeyEvent/VK_RIGHT [:move :east],
    KeyEvent/VK_LEFT [:move :west], KeyEvent/VK_ENTER [:action]}
   (.getKeyCode input)))

(defn draw [^Graphics2D g {{[px py :as pos] :pos h :health s :score sees :sees, lv :level :as play} :player,
                           {:keys [points width height monsters seen]} :level, msgs :messages}]
  (doto g
    (.setColor Color/black)
    (.fillRect 0 0 panel-width panel-height)
    (.setFont (Font. "Monospaced" Font/PLAIN 13)))
  (let [monpts (set/intersection sees (set (map :pos monsters)))
        post-process (fn [col sees? seen?]
                       (cond (not (or sees? seen?)) Color/black
                             (not sees?) (.darker (.darker ^Color col))
                             :else col))]
    (doseq [xx (range width), yy (range (inc height))]
      (let [p [xx yy],
            type (cond (= p pos) ::player, (monpts p) ::monster, :else (points p))
            p-vis (sees p), p-rem (seen p)]
        (.setColor g (post-process (color-rep type) p-vis p-rem))
        (.drawString g ^String (char-rep type) (int (* xx 11)) (int (* yy 11))))))
  (.setColor g Color/white)
  (dorun 3 (map-indexed #(.drawString g ^String %2 (* 15 11) (int (* 11 (+ 4 game-height (- %))))) msgs))
  (doto g
    (.drawString (str "Health: " h) 11 (int (* 11 (inc game-height))))
    (.drawString (str "Score: " s) 11 (int (* 11 (+ game-height 2))))
    (.drawString (str "Level: " lv) 11 (int (* 11 (+ 3 game-height))))))

(defn -main [& args]
  (let [game-state (atom (initialize-gamestate))
        dim (Dimension. panel-width panel-height)
        panel (doto (proxy [JPanel] [] (paint [g] (draw g @game-state)))
                (.setMinimumSize dim)
                (.setPreferredSize dim)
                (.setMaximumSize dim))
        ka (proxy [KeyAdapter] [] (keyPressed [e] (when-let [input (comprehend e)]
                                                    (swap! game-state tick input)
                                                    (.repaint panel))))]
    (doto (JFrame. "(dunjeon)")
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add panel) .pack
      (.setResizable false)
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.addKeyListener ka))))


