(ns advent-of-code.day22.core
  (:require
    [clojure.string :as str]))

;; data structures
;; state {:player {:mana :hit-points :armor :mana-spent} :boss {:hit-points :damage} :effects [{:timer :fn}] :history [{:desc :state}] :hard false}

(defn add-history [state desc old-state]
  (update-in state [:history] conj {:desc desc :state old-state}))

(defn magic-missile-buy [state]
  (-> state (update-in [:player :mana] - 53) (update-in [:player :mana-spent] + 53)))

(defn magic-missle-apply [state]
  (-> state (update-in [:boss :hit-points] - 4)))

(defn drain-buy [state]
  (-> state (update-in [:player :mana] - 73) (update-in [:player :mana-spent] + 73)))

(defn drain-apply [state]
  (-> state (update-in [:player :hit-points] + 2) (update-in [:boss :hit-points] - 2)))

(defn shield-buy [state]
  (-> state (update-in [:player :mana] - 113) (update-in [:player :mana-spent] + 113)))

(defn shield-apply [state]
  (-> state (assoc-in [:player :armor] 7)))

(defn shield-reset [state]
  (-> state (assoc-in [:player :armor] 0)))

(defn poison-buy [state]
  (-> state (update-in [:player :mana] - 173) (update-in [:player :mana-spent] + 173)))

(defn poison-apply [state]
  (-> state (update-in [:boss :hit-points] - 3)))

(defn recharge-buy [state]
  (-> state (update-in [:player :mana] - 229) (update-in [:player :mana-spent] + 229)))

(defn recharge-apply [state]
  (-> state (update-in [:player :mana] + 101)))

(def spell-list
  [ {:name "Magic Missile" :cost 53 :damage 4 :buy magic-missile-buy :apply magic-missle-apply}
    {:name "Drain" :cost 73 :damage 2 :hit-points 2 :buy drain-buy :apply drain-apply}
    {:name "Shield" :cost 113 :armor 7 :turns 6 :buy shield-buy :apply shield-apply}
    {:name "Poison" :cost 173 :damage 3 :turns 6 :buy poison-buy :apply poison-apply}
    {:name "Recharge" :cost 229 :mana 101 :turns 5 :buy recharge-buy :apply recharge-apply}])

(defn spell-cost [spell]
  (:cost spell))

(defn effect? [spell]
  (contains? spell :turns))

(defn active-effect? [state spell]
  (let [effects (:effects state)
        name (:name spell)]
    (loop [e (first effects)
            effects (rest effects)]
      (if-not e
        false
        (if (= (get-in e [:spell :name]) name)
          true
          (recur (first effects) (rest effects)))))))

(defn apply-effects [state]
  (let [effects (:effects state)]
    (loop [ state (shield-reset state)
            out []
            e (first effects)
            effects (rest effects)]
      (if-not e
        (assoc state :effects out)
        (let [timer (- (:timer e) 1)
              func (get-in e [:spell :apply])]
          (if (> timer 0)
            (recur (func state) (conj out (assoc e :timer timer)) (first effects) (rest effects))
            (recur (func state) out (first effects) (rest effects))))))))

(defn player-dead? [state]
  (<= (get-in state [:player :hit-points]) 0))

(defn player-alive? [state]
  (> (get-in state [:player :hit-points]) 0))

(defn player-bankrupt? [state]
  (< (get-in state [:player :mana]) 53))

(defn player-hit-points [state]
  (get-in state [:player :hit-points]))

(defn player-armor [state]
  (get-in state [:player :armor]))

(defn player-mana [state]
  (get-in state [:player :mana]))

(defn player-mana-spent [state]
  (get-in state [:player :mana-spent]))

(defn boss-dead? [state]
  (<= (get-in state [:boss :hit-points]) 0))

(defn boss-hit-points [state]
  (get-in state [:boss :hit-points]))

(defn boss-damage [state]
  (get-in state [:boss :damage]))

(defn boss-attack [state]
  (let [damage (max (- (boss-damage state) (player-armor state)) 1)
        new-state (update-in state [:player :hit-points] - damage)]
    (add-history new-state "boss attack" new-state)))

(defn spell-choices [state]
  (let [mana (player-mana state)]
    (loop [out []
            s (first spell-list)
            spells (rest spell-list)]
      (if-not s
        out
        (if (and (>= mana (spell-cost s)) (not (active-effect? state s)))
          (recur (conj out s) (first spells) (rest spells))
          (recur out (first spells) (rest spells)))))))

(defn add-effect [state effect]
  (-> state ((:buy effect)) (update-in [:effects] conj {:timer (:turns effect) :spell effect})))

(defn cast-spell [state spell]
  (-> state ((:buy spell)) ((:apply spell))))

(defn apply-spells [state spells]
  (loop [states []
          s (first spells)
          spells (rest spells)]
    (if-not s
      states
      (if (effect? s)
        (recur (conj states (add-effect state s)) (first spells) (rest spells))
        (recur (conj states (cast-spell state s)) (first spells) (rest spells))))))

(defn print-state [state]
  (println "Player:" (player-hit-points state) (player-armor state) (player-mana state) (player-mana-spent state))
  (println "Boss:" (boss-hit-points state) (boss-damage state))
  (let [effects (:effects state)]
    (loop [e (first effects)
            effects (rest effects)]
      (when e
        (println "Effect:" (get-in e [:spell :name]) "timer" (:timer e))
        (recur (first effects) (rest effects))))))

(defn print-history-state [history]
  (println "--" (:desc history))
  (print-state (:state history)))

(def reset-winning-state {:player {:mana-spent 0}})

(def winning-state (atom reset-winning-state))

(defn record-winning-state [state]
  (let [mana (player-mana-spent state)
        current (player-mana-spent @winning-state)]
    (when (or (= 0 current) (< mana current))
      (reset! winning-state state))))

(defn print-winning-state []
  (println "Winning state: mana spent:" (player-mana-spent @winning-state))
  (let [history (:history @winning-state)]
    (loop [h (first history)
            history (rest history)]
      (when h
        (print-history-state h)
        (recur (first history) (rest history))))))

(defn deduct-one-player-hit-point [state]
  (update-in state [:player :hit-points] - 1))

(defn hard-state [state]
  (if (:hard state)
    (deduct-one-player-hit-point state)
    state))

(defn turn [state who loops]
  ;;(println "turn:" who)
  ;;(println state)
  (if (> loops 0)
    (if (= :player who)
      (let [state (hard-state state)]
        (when (player-alive? state)
          (let [state (apply-effects state)]
            (when-not (player-bankrupt? state)
              (let [spells (spell-choices state)
                    states (apply-spells state spells)]
                (mapv #(turn % :boss (- loops 1)) states))))))
      (if (boss-dead? state)
        (record-winning-state state)
        (let [state (apply-effects state)]
          (if (boss-dead? state)
            (record-winning-state state)
            (let [state (boss-attack state)]
              (when-not (player-dead? state)
                (turn state :player (- loops 1))))))))))

(defn line->boss [boss line]
  (let [tokes (str/split line #":")
        val (Integer/parseInt (str/trim (get tokes 1)))]
    (case (first tokes)
      "Hit Points" (assoc boss :hit-points val)
      "Damage" (assoc boss :damage val)
      "Armor" (assoc boss :armor val)
      boss)))

(defn lines->boss [lines]
  (loop [boss {}
          l (first lines)
          lines (rest lines)]
    (if-not l
      boss
      (recur (line->boss boss l) (first lines) (rest lines)))))

(defn read-boss [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->boss lines)))

(defn start [state]
  (reset! winning-state reset-winning-state)
  (turn state :player 20)
  ;;(print-winning-state)
  (player-mana-spent @winning-state))

(defn run []
  (let [boss (read-boss "src/advent_of_code/day22/input.txt")
        state {:player {:hit-points 50 :mana 500 :armor 0 :mana-spent 0} :boss boss :effects [] :history [] :hard false}
        state (add-history state "Start" state)]
    (println "Day 22, part 1:" (start state))
    (println "Day 22, part 2:" (start (assoc state :hard true)))))
