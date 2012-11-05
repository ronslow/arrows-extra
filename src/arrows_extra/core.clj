
(ns arrows-extra.core)
(use 'conduit.core 'arrows.core)

(def-proc block [input]
 []
)

(defn arr-filter [pred]
 (a-if (partial apply pred) pass-through block)
)
(defn arr-delay-state [init]

(a-comp 
 (a-loop (a-arr (fn [[state input]] [state input]))
 init 
 (a-arr (fn [input1] (first (second input1)))))
 (a-arr (fn [[state2 [dummy input2]]] [state2 input2]))))
(defn arr-delay [init]
 (a-comp (a-arr (fn [input] [input input])) 
  (arr-delay-state init) 
  (a-arr (fn [[input1 input2]] input1)))

)
(defn arr-edge [compare-fn]
  (a-comp
   (a-all
    (arr-delay nil)
    pass-through)
   ; :: [[ev1 last] [ev2 next]]
   (a-arr (fn [[[ev1 last] [ev2 next]]] [(if last (compare-fn last next) 0) ev2 next]))
   ; :: [comp ev2 next]
   (a-arr (fn [[comp ev data]] [(if (not (= 0 comp)) (assoc ev :edge (if (> 0 comp) :rising :falling)) ev) data]))
          )

  )
(defmacro arr-switch [init state-fn pred-fn trueChannel falseChannel]

  `(a-comp
   (a-loop (a-arr (fn [[state# input#]] [(~state-fn state# input#) input#]))
         ~init
         (a-arr first)
         )
    (a-par
 
     (a-arr (fn [state#] (~pred-fn state#)))
      pass-through
   )
  (a-select true ~trueChannel false ~falseChannel)
)
)
(defn arr-switch-between [lower upper trueChannel falseChannel]
  (arr-switch -1  (fn [state input] (inc state)) (fn [state] (and (>= state lower) (<= state upper))) trueChannel falseChannel)
  )

(defn arr-toggle [pred-on pred-off]
(a-comp
(a-loop 
 (a-arr (fn [[state input]] [(if (pred-off input) false (or state
 (pred-on input))) input])) 
 false
 (a-arr first))
 (a-select true pass-through false block)
 )
) 
(defn- boolean-to-int [value]

  (if value 1 0)
  )


(defn arr-toggle-inclusive [pred-on pred-off]
                          
                              (a-comp (a-loop (a-arr (fn [[state input]] [(if (pred-off input) false (or state (pred-on input))) input])) false (a-arr first))
                                        ;  (a-arr (fn [data] [{} data]))
                                      (a-all (a-arr (constantly {})) pass-through)
                                      (arr-edge (fn [lst nxt] (- (boolean-to-int (first lst)) (boolean-to-int (first nxt)))))
                                      (a-arr (fn [[ev [state data]]] [(or state (= (:edge ev) :falling)) data]))
                                      (a-select true pass-through false block)

                                      )

                              )
(defn arr-gate [init state-fn pred-fn]
  (arr-switch init state-fn pred-fn pass-through block)
)
(defn arr-between [lower upper]
  (arr-gate -1 (fn [state input] (inc state)) (fn [state] (and (>= state lower) (<= state upper))))
  )
  
(def arr-once
  (arr-between 0 0)
  )

(defn arr-accum
  

  [init accum-fn pred]

  (a-comp (a-loop (a-arr (fn [[state input]] (if (pred state input)
 [state input] [(accum-fn state input) input])))
            init 
            (a-arr (fn [[ state input]] (if (pred state input) init
 state))))
            (a-selectp (fn [[state input]] (pred state input))  true (a-arr first) false block)) 
  )

(defn arr-accum-greedy [init accum-fn pred]
(a-comp (a-loop (a-arr (fn [[state input]] [(accum-fn state input)
 input]))
 init (a-arr first)) 
(a-selectp (fn [[state input]] (pred state input))  true (a-arr first) false block) )
)
