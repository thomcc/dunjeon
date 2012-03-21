(ns dun.core
  (:use [dun level]))

(def char-rep {:floor ".", nil "#", :wall "#", :stairs ">", :gold "$", :booze "q", :sword "(", :armor "[",
               :shield "+", :spawner "!"})

(defn draw-level [{width :width, height :height lvl :points}]
  (doseq [y (range height), x (range width)]
    (when (= 0 x) (.println System/out))
    (.print System/out (char-rep (lvl [x y])))))

(defn -main [& args] (draw-level (gen-level 80 80 20)))


