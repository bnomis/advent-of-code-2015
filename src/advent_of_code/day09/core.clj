(ns advent-of-code.day09.core
  (:require [clojure.string :as str]))

;; data structures
;; link [:source :exit distance] ; read from input file
;; normalised links -> map of sources with their exits and distance
;; exits are [:destination distance]
;; normalised map is
;; {:source [[:exit1 distance1] [:exit2 distance2] ...]}
;; journey -> list of links [[:city1 :city2 distance] [:city2 :city3 distance]]
;; route -> list of cities [:city1 :city2 :city3 ... ]

;; In an attempt to avoid doing an exhaistive search of all routes, my first attempt
;; at this was to use the initial links supplied in the files as the start of a journey,
;; follow to the end and measure distance. This worked for finding the shortest route.
;; But it failed for finding the longest route.
;; So, now I find all routes and sort by distance.


(defn link-distance [link]
  (let [length (count link)]
    (get link (- length 1))))

(defn link-destination [link]
  ;;(println "link-destination:" link (count link))
  (let [length (count link)]
    (get link (- length 2))))

(defn link-source [link]
  (let [length (count link)]
    (if (= length 3)
      (get link 0)
      nil)))

(defn link-reverse [link]
  [(get link 1) (get link 0) (get link 2)])

(defn exit->link [source exit]
  (into [] (cons source exit)))

(defn link->exit [link]
  (into [] (rest link)))

(defn find-link [links city]
  (loop [l (first links)
          links (rest links)]
    (if (= city (link-destination l))
      l
      (recur (first links) (rest links)))))

(defn city-distance [normalised source destination]
  (let [link (find-link (get normalised source) destination)]
    (link-distance link)))

(defn route-distances [normalised route]
  (let [length (count route)]
    (loop [distances []
            current 0]
      (if (= current (- length 1))
        distances
        (recur (conj distances (city-distance normalised (get route current) (get route (+ 1 current )))) (inc current))))))

(defn route-distance [normalised route]
  (reduce + (route-distances normalised route)))

(defn make-route-and-distance-vector [normalised routes]
  (loop [out []
          r (first routes)
          routes (rest routes)]
    (if-not r
      out
      (recur (conj out {:distance (route-distance normalised r) :route r}) (first routes) (rest routes)))))

(defn sort-route-and-distance-vector [routes-with-distance]
  (sort-by :distance routes-with-distance))

(defn journey-distance [links]
  (reduce + (mapv link-distance links)))

(defn sort-exits [exits]
  (sort-by link-distance exits))

(defn visited? [visited destination]
  ;;(println "visited?:" visited)
  ;;(println "visited?:" destination)
  (loop [v (first visited)
          visited (rest visited)]
    (if-not v
      false
      (if (= v destination)
        true
        (recur (first visited) (rest visited))))))

(defn conj-if-not-visited [visited out link]
  ;;(println "conj-if-not-visited:" visited)
  ;;(println "conj-if-not-visited:" out)
  ;;(println "conj-if-not-visited:" link)
  (let [destination (link-destination link)]
    (if (visited? visited destination)
      out
      (conj out link))))

(defn filter-exits [visited exits]
  ;;(println "filter-exits:" visited)
  ;;(println "filter-exits:" exits)
  (loop [out []
          link (first exits)
          exits (rest exits)]
    ;;(println "filter-exits:" link)
    ;;(println "filter-exits:" exits)
    (if-not link
      out
      (recur (conj-if-not-visited visited out link) (first exits) (rest exits)))))

(defn next-exit [normalised visited current]
  ;;(println "next-exit: visited" visited)
  ;;(println "next-exit: current" current)
  (let [exits (get normalised current)
        filtered-exits (filter-exits visited exits)
        sorted-exits (sort-exits filtered-exits)]
    ;;(println "next-exit: exits:" exits)
    ;;(println "next-exit: filtered:" filtered-exits)
    ;;(println "next-exit: sorted:" sorted-exits)
    (if (empty? sorted-exits)
      nil
      (first sorted-exits))))

(defn find-journey [normalised link]
  ;;(println "find-journey:" link)
  (loop [visited [(link-source link) (link-destination link)]
          journey [link]
          current (link-destination link)
          exit (next-exit normalised visited current)]
      (let [exit-destination (link-destination exit)]
        ;;(println "find-journey: visited:" visited)
        ;;(println "find-journey: current:" current)
        ;;(println "find-journey: exit:" exit)
        ;;(println "find-journey: exit-destination:" exit-destination)
        (if-not exit
          journey
          (recur (conj visited exit-destination) (conj journey (exit->link current exit)) exit-destination (next-exit normalised visited exit-destination))))))

(defn find-journeys [normalised links]
  ;;(println "find-journeys:" normalised)
  ;;(println "find-journeys:" links)
  (loop [out []
          link (first links)
          links (rest links)]
    (if-not link
      out
      (recur (conj out (find-journey normalised link)) (first links) (rest links)))))

(defn sort-exits-long [exits]
  (reverse (sort-exits exits)))

(defn next-exit-long [normalised visited current]
  (let [exits (get normalised current)
        filtered-exits (filter-exits visited exits)
        sorted-exits (sort-exits-long filtered-exits)]
    (if (empty? sorted-exits)
      nil
      (first sorted-exits))))

(defn find-journey-long [normalised link]
  (loop [visited [(link-source link) (link-destination link)]
          journey [link]
          current (link-destination link)
          exit (next-exit-long normalised visited current)]
      (let [exit-destination (link-destination exit)]
        (if-not exit
          journey
          (recur (conj visited exit-destination) (conj journey (exit->link current exit)) exit-destination (next-exit-long normalised visited exit-destination))))))

(defn find-journeys-long-loop [normalised links]
  (loop [out []
          link (first links)
          links (rest links)]
    (if-not link
      out
      (recur (conj out (find-journey-long normalised link)) (first links) (rest links)))))

(defn find-journeys-long [normalised links]
  (concat
    (find-journeys-long-loop normalised links)
    (find-journeys-long-loop normalised (mapv link-reverse links))))


;; finding all the routes

;; finds the exits for a route
(defn route-exits [normalised route]
  ;;(println "route-exits:" route)
  (let [exits (get normalised (last route))]
    (filter-exits route exits)))

;; takes a route and adds exits to it
;; returns an array of routes
;; if at the end of a route, the array contains the original single route
;; if the route has two exits, returns an array two routes and so on
;; therefore can test the length of the returned array (i.e. = 1) to see if at end of route
(defn add-exits [normalised route]
  (let [exits (route-exits normalised route)]
    (if (or (empty? exits) (not exits))
      ;; end of route
      [route]
      (loop [out []
              e (first exits)
              exits (rest exits)]
        (if-not e
          out
          (recur (conj out (conj route (link-destination e))) (first exits) (rest exits)))))))

(defn add-route-data [data route]
  (if (= 1 (count route))
    (update data :terminals #(conj % (get route 0)))
    (update data :non-terminals #(into [] (concat % route)))))

(defn find-routes [normalised routes terminals]
  ;;(println "find-routes: routes:" routes)
  ;;(println "find-routes: terminals:" terminals)
  (if (or (empty? routes) (not routes))
    terminals
    (do
      ;; extend our input routes if possible
      (let [extended (loop [out []
                            r (first routes)
                            routes (rest routes)]
                        (if-not r
                          out
                          (recur (conj out (add-exits normalised r)) (first routes) (rest routes))))
            filtered (loop [data {:terminals [] :non-terminals []}
                            e (first extended)
                            extended (rest extended)]
                      (if-not e
                        data
                        (recur (add-route-data data e) (first extended) (rest extended))))]
        (find-routes normalised (:non-terminals filtered) (into [] (concat terminals (:terminals filtered))))))))

(defn start-find-routes [normalised]
  (let [keys (keys normalised)]
    (loop [out []
            k (first keys)
            keys (rest keys)]
      (if-not k
        out
        (recur (into [] (concat out (find-routes normalised [[k]] []))) (first keys) (rest keys))))))

;; normalisation

(defn update-exits [out source exit]
  (update out source #(conj % exit)))

(defn create-exits [out source exit]
  (assoc out source [exit]))

(defn add-link [out link]
  (let [source (link-source link)
        exit (link->exit link)]
    (if (contains? out source)
      (update-exits out source exit)
      (create-exits out source exit))))

(defn normalise-link [out link]
  (-> out
    (add-link link)
    (add-link (link-reverse link))))

(defn normalise-links [links]
  ;;(println "normalise-links:" links)
  (loop [out {}
          l (first links)
          links (rest links)]
    (if-not l
      out
      (recur (normalise-link out l) (first links) (rest links)))))

(defn sort-journeys [journeys]
  (sort-by journey-distance journeys))

(defn shortest-journey [journeys]
  ;;(println "shortest-journey:" journeys)
  (first (sort-journeys journeys)))

(defn longest-journey [journeys]
  ;;(println "shortest-journey:" journeys)
  (last (sort-journeys journeys)))

(defn line->link [line]
  (let [tokes (str/split line #" ")]
    [(keyword (get tokes 0)) (keyword (get tokes 2)) (Integer/parseInt (get tokes 4))]))

(defn lines->links [lines]
  (mapv line->link lines))

(defn read-links [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->links lines)))

(defn find-path [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        journeys (find-journeys normalised links)
        journey (shortest-journey journeys)]
    (println "shortest:" journey)
    (journey-distance journey)))

(defn find-path-longest [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        journeys (find-journeys-long normalised links)
        journey (longest-journey journeys)]
    ;;(println "journeys:" journeys)
    (println "longest:" journey)
    (journey-distance journey)))

(defn routes-for-file [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        routes (start-find-routes normalised)]
    (println "roots-for-file:" file)
    (mapv println routes))
  true)

(defn find-ordered-routes [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        routes (start-find-routes normalised)
        routes-with-distance (make-route-and-distance-vector normalised routes)
        sorted (sort-route-and-distance-vector routes-with-distance)]
    ;;(println "find-ordered-routes: sorted" sorted)
    sorted))

(defn find-shortest [file]
  (:distance (first (find-ordered-routes file))))

(defn find-longest [file]
  (:distance (last (find-ordered-routes file))))

(defn run []
  (println "Day 09, part 1:" (find-shortest "src/advent_of_code/day09/input.txt"))
  (println "Day 09, part 2:" (find-longest "src/advent_of_code/day09/input.txt")))
