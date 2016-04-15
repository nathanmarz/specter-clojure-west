(ns specter-demo.examples
  (:use [com.rpl.specter]
        [com.rpl.specter.macros]
        [clojure.pprint :only [pprint]]
        [com.rpl.specter.impl :only [benchmark]])
  (:require [clojure.string :as str]))

(def ^:dynamic *ACTIVE* false)

(defmacro print-results [form]
  (let [val (last form)]
    `(let [res# ~form]
       (when *ACTIVE*
         (println " ")
         (println " ")
         (pprint ~val)
         (println "->")
         (pprint res#)
         ))))

(defmacro print-manual [form]
  (let [val (second form)]
    `(let [res# ~form]
       (when *ACTIVE*
         (println " ")
         (println " ")
         (pprint ~val)
         (println "->")
         (pprint res#)
         ))))

(declare world)
(defn bank-print-results [val]
  (when *ACTIVE*
    (println " ")
    (pprint world)
    (println "->")
    (pprint val)
    (println " ")))











;; Increment all the values in a map manually:
(print-manual
 (->> {:a 1 :b 2 :c 3}
      (map (fn [[k v]] [k (inc v)]))
      (into {}))
 )

;; You do this enough and you eventually extract a map-vals function
(defn map-vals [m afn]
  (->> m
       (map (fn [[k v]] [k (afn v)]))
       (into {})))

(print-manual
 (map-vals {:a 1 :b 2 :c 3} inc))

;; `map-vals` is a constantly re-implemented function.
;; (show slide)
;; It's not a good function.

;; What if list of maps?
(print-results
 (map
  (fn [m] (map-vals m inc))
  '({:a 1 :b 2} {:a 3}))
 )

;; What if vector of maps?
(print-results
 (mapv ; reconstruction logic
  (fn [m] (map-vals m inc))
  [{:a 1 :b 2} {:a 3}])
 )

;; What if map of maps?
(print-manual
 (map-vals
  {:a {:x 1 :y 2} :b {:x 3}}
  (fn [m] (map-vals m inc)))
 )


;; What if map of vectors of maps?
(print-manual
 (map-vals
  {:a [{:a 1} {:x 2 :y 3}] :b [{:x 3}]}
  (fn [l]
    (mapv (fn [m] (map-vals m inc)) l)))
 )

;; `map-vals` is specific to one particular nested transformation
;; and does not compose well.

;; You must constantly worry about how to reconstruct the original
;; data structure around the targeted changes.

;; Another example of this:
(def PEOPLE-DATA
  [{:name "Alice" :money 10 :age 24}
   {:name "Bob" :money 8}
   {:name "Charlie" :money 50}])

(defn give-money [data]
  (mapv ; reconstruction logic
    (fn [m]
      (update m :money
        (fn [v]
          (if (< v 15) ; desired transformation nested 2 levels deep
            (+ v 10)
            v)) ; reconstruction logic
        ))
    data
    ))



;; This is a constant annoyance.
;; (why I made Specter)

;; Back to map-vals. Here's how to do equivalent in Specter:
(print-results
 (transform [ALL LAST]
   inc
   {:a 1 :b 2 :c 3})
 )

;; Can factor a specific "map vals" navigator from this:

(def MAP-VALS (comp-paths ALL LAST))
(print-results
 (transform MAP-VALS
    inc
    {:a 1 :b 2 :c 3})
 )

;; Unlike map-vals function, this is composable:

;; list of maps:
(print-results
 (transform [ALL MAP-VALS]
   inc
   '({:a 1 :b 2} {:a 3}))
 )

;; vector of maps (exact same logic):
(print-results
 (transform [ALL MAP-VALS]
   inc
   [{:a 1 :b 2} {:a 3}])
 )

;; map of maps
(print-results
 (transform [MAP-VALS MAP-VALS]
   inc
   {:a {:x 1 :y 2} :b {:x 3}})
 )

;; map of vector of maps:
(print-results
 (transform [MAP-VALS ALL MAP-VALS]
   inc
   {:a [{:a 1} {:x 2 :y 3}] :b [{:x 3}]})
 )

;; What's happened here is simple:
;;  - Navigation is its own independent abstraction.
;;  - Navigation is dissassociated from the desired transformation.

;; You're heard "100 functions operate on one data structure than
;; 10 functions on 10 data structures."
;; I would counter that it's dramatically better to have a handful
;; of navigators that compose to those 100 functions.

;; Another example of common manipation that can be reframed as
;; composition of simple navigators:

;; Goal: add a value to a nested set.
(def DATA
  {:a #{2 3}
   :b #{4 5}
   })

;; Manual Clojure:
(print-manual
 (update
   DATA
   :a
   (fn [s]
     (if s (conj s 1) #{1})))
 )

;;Specter (take 1):
(print-results
 (transform [:a NIL->SET]
   (fn [s] (conj s 1))
   DATA)
 )

;;Specter (take 2):
(print-results
 (transform [:a (subset nil)]
   (fn [_] #{1})
   DATA)
 )

;;Specter (take 3):
(print-results
 (setval [:a (subset nil)]
   #{1}
   DATA)
 )

;; This is a different way of thinking.
;; Instead of "Adding to a nested set", you think "Navigate to
;; empty subset and replace it with a new value".
;; Navigation doesn't have to be to an explicit nested data structure.
;; Another common navigation is to inherent substructure.

;; Even more apparent with sequences:
(print-results
 (setval (srange 4 9)
   [:hello :world]
   [0 1 2 3 4 5 6 7 8 9 10 11])
 )

(print-results
 (transform (srange 4 9)
   reverse
   [0 1 2 3 4 5 6 7 8 9 10 11])
 )

(print-results
 (transform [(srange 4 9) ALL even?]
   #(* 100 %)
   [0 1 2 3 4 5 6 7 8 9 10 11])
 )

(print-results
 (transform (filterer even?)
   reverse
   [0 1 2 3 4 5 6 7 8 9 10 11])
 )

(print-results
 (transform [(srange 4 13) (filterer even?)]
   reverse
   [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])
 )

;; So far I've only shown transformations, but navigation paths
;; work just as well for querying:

(print-results
 (select [ALL :a even?]
   [{:a 1} {:a 2 :b 3} {:a 4 :c -1}])
 )

(print-results
 (select [(srange 4 9) ALL even?]
   [0 1 2 3 4 5 6 7 8 9 10 11])
 )

;; With this abstraction, you have precise and elegant control over
;; your data structures. As you "level up" in skill with Specter,
;; extraction of "navigation" as its own abstraction enables
;; programming in different ways.

(def world
  {:people [{:money 129825 :name "Alice Brown"}
            {:money 100 :name "John Smith"}
            {:money 5000000000 :name "Scrooge McDuck"}
            {:money 2870 :name "Charlie Johnson"}
            {:money 8273820 :name "Michael Smith"}
            ]
   :bank {:funds 470000000000}}
  )

;; In Clojure, how to manually transfer from user to the bank:
(defn user->bank [world name amt]
  (let [curr-funds (->> world
                        :people
                        (filter (fn [user] (= (:name user) name)))
                        first
                        :money
                        )]
    (if (< curr-funds amt)
     (throw (IllegalArgumentException. "Not enough funds!"))
     (-> world
         (update
          :people
          (fn [user-list]
            (mapv (fn [user]
                    (if (= (:name user) name)
                      (update user :money #(- % amt))
                      user
                      ))
                  user-list)))
         (update-in
          [:bank :funds]
          #(+ % amt))
         ))))


;; Transfer function with Specter:
(defn transfer
  [world from-path to-path amt]
  (let [givers (select from-path world)

        receivers (select to-path world)

        total-receive (* amt (count givers))

        total-give (* amt (count receivers))]
    (if (every? #(>= % total-give) givers)
      (->> world
           (transform from-path #(- % total-give))
           (transform to-path #(+ % total-receive))
           )
      (throw (IllegalArgumentException. "Not enough funds!"))
      )))


;; - Generic: fixed many-to-many transfers between any sets of
;;   entities
;; - Easy to read and elegant.
;; - Agnostic to the details of the world data structure!

(defn user-path [name]
  [:people
   ALL
   (fn [u] (= (:name u) name))]
  )

(defn user->bank [world name amt]
  (transfer
    world
    [(user-path name) :money]
    [:bank :funds]
    amt
    ))

(bank-print-results
  (user->bank world "John Smith" 10))

;; Can use transfer to do lots of operations!

(defn pay-fee [world]
  (transfer world
            [:people ALL :money]
            [:bank :funds]
            1))

(bank-print-results
  (pay-fee world))

(defn bank-give-dollar [world]
  (transfer world
            [:bank :funds]
            [:people ALL :money]
            1))

(bank-print-results
  (bank-give-dollar world))

(defn bank-loyal-bonus [world]
  (transfer world
            [:bank :funds]
            [:people (srange 0 3) ALL :money]
            5000))

(bank-print-results
  (bank-loyal-bonus world))


;; Specter is data structure agnostic.
;; At core is a generic protocol and efficient means to compose
;; instances of that protocol together.
;; On top of that are "batteries included" set of navigators
;; for common use cases with maps, lists, sets, and vectors.
(comment
  (defprotocol StructurePath
    (select* [this structure next-fn])
    (transform* [this structure next-fn]))



  (defpath keypath [key]
    (select* [this structure next-fn]
             (next-fn (get structure key)))
    (transform* [this structure next-fn]
                (assoc structure key (next-fn (get structure key)))
                ))

  (defpath subset [aset]
    (select* [this structure next-fn]
             (next-fn (set/intersection structure aset)))
    (transform* [this structure next-fn]
                (let [subset (set/intersection structure aset)
                      newset (next-fn subset)]
                  (-> structure
                      (set/difference subset)
                      (set/union newset))
                  )))


  )


;; Specter is meant for everyday data structure querying and
;; manipulation. It must also be usable in performance sensitive
;; code!

(def MDATA {:a {:b {:c 1}}})

(def compiled-path (comp-paths :a :b :c))

(defn manual-transform [data]
  (update data
          :a
          (fn [d1]
            (update d1
                    :b
                    (fn [d2]
                      (update d2 :c inc))))))

(comment
  (benchmark 1000000 #(get-in MDATA [:a :b :c]))

  (benchmark 1000000 #(select [:a :b :c] MDATA))

  (benchmark 1000000 #(select compiled-path MDATA))

  (benchmark 1000000 #(compiled-select compiled-path MDATA))

  (benchmark 1000000 #(-> MDATA :a :b :c vector))

  (benchmark 1000000 #(update-in MDATA [:a :b :c] inc))

  (benchmark 1000000 #(transform [:a :b :c] inc MDATA))

  (benchmark 1000000 #(transform compiled-path inc MDATA))

  (benchmark 1000000 #(compiled-transform compiled-path inc MDATA))

  (benchmark 1000000 #(manual-transform MDATA))

  )



;; As you get better, you will learn to use Specter to handle very
;; sophisticated tasks.
;; (show visual of recursive DAG.)

;; Recursive graph squashing example (with squashing operation):
(comment
  (declarepath ReachableGraph)

  (providepath ReachableGraph
    (continue-then-stay
      TOPSORT
      NODE
      #(instance? SubGraphNode %)
      :graph
      ReachableGraph
      ))

  ;; This is an immutable transformation!
  (transform
    [ReachableGraph
     TOPSORT
     (selected? NODE #(instance? MarkerNode %))]
    (fn [[graph node-id]]
      (graph/squash graph node-id))
    my-graph)


  ;; Another simple graph example:
  (transform [TOPSORT
                (collect PARENTS NODE :name)
                NODE
                (collect-one :name)
                :royal-name
                ]
               (fn [parent-names name _]
                 (str name " of " (str/join ", " parent-names)))
               ancestry-graph
               )

)


;;**********************************
;; More examples below:
;;  - late-bound parameterization
;;  - walkthrough of transformations
;;  - value collection



;; example of late-bound parameterization

(defn reverse-matching-in-range [aseq start end predicate]
  (transform [(srange start end) (filterer predicate)]
             reverse
             aseq))

(def MATCHING-RANGE (comp-paths srange (filterer pred)))
(defn reverse-matching-in-range-fast [aseq start end predicate]
  (compiled-transform (MATCHING-RANGE start end predicate)
                      reverse
                      aseq))

(def RANGE [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20])

(comment
  (benchmark 100000 #(reverse-matching-in-range RANGE 4 11 odd?))
  (benchmark 100000 #(reverse-matching-in-range-fast RANGE 4 11 odd?))
  )





(comment
 (transform [ALL :a even?]
            dec
            [{:a 2 :b 3} {:a 1} {:a 4}])

  ;;=>input
  [{:a 2 :b 3} {:a 1} {:a 4}]
  ;;=>ALL
  {:a 2 :b 3}
  {:a 1}
  {:a 4}
  ;;=>:a
  2
  1
  4
  ;;=>even?
  2
  4
  ;;=> dec
  1
  3
  ;;=>even?
  1
  1
  3
  ;;=>:a
  {:a 1 :b 3}
  {:a 1}
  {:a 3}
  ;;=>ALL
  [{:a 1 :b 3} {:a 1} {:a 3}]
  ;;=>output


 (transform [(filterer odd?) LAST]
            inc
            [1 2 3 4 5 6 7 8 9 18 12 14])

  ;;=>input
  [1 2 3 4 5 6 7 8 9 18 12 14]
  ;;=>(filterer odd?)
  [1 3 5 7 9]
  ;;=>LAST
  9
  ;;=>inc
  10
  ;;=>LAST
  [1 3 5 7 10]
  ;;=>(filterer odd?)
  [1 2 3 4 5 6 7 8 10 18 12 14]
  ;;=>output


 (setval [ALL END]
         [:a :b]
         [[1] '(1 2) [:c]])

 (transform [ALL (collect-one :b) :a even?]
            +
            [{:a 1 :b 3} {:a 2 :b -10} {:a 4 :b 10} {:a 3}])

 (setval [ALL
          (selected? (filterer even?) (view count)
                     #(>= % 2))
          END]
         [:c :d]
         [[1 2 3 4 5 6] [7 0 -1] [8 8] []])

  )


(alter-var-root #'*ACTIVE* (fn [_] true))
