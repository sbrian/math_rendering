(ns com.sbrian.mathrender.arctan)

(defprotocol IsNegativeProtocol
  (is-negative? [this]))

(defprotocol TermProtocol
  (increment [this])
  (compute [this x]))

(defprotocol SumProtocol
  (flatten-sum [this])
  (sort-sum [this]))
  
(defprotocol Computable
  (compute [this x]))

(defprotocol Differentiable
  (differentiate [this]))

(defn int-flatten-sum [v term]
  (cond
    (extends? SumProtocol (type term)) (reduce conj v (:terms (flatten-sum term)))
    (extends? TermProtocol (type term)) (conj v term)
    :else (throw (Exception. "Unknown type"))))

(defrecord Sum [terms]
  SumProtocol    
  (flatten-sum [this]
    (assoc this :terms (reduce int-flatten-sum [] (:terms this))))
  (sort-sum [this]
    (assoc this :terms (sort-by #(:exp %) (:terms this))))
  Computable
  (compute [this x]
    (reduce
      (fn [a b] (+ a (compute b x)))
       0 (:terms this)))
  IsNegativeProtocol
  (is-negative? [this] (is-negative? (first (:terms this))))
  Differentiable
  (differentiate [this]
    (assoc this
      :terms (map differentiate (:terms this)))))
      
(defrecord Term [negexp fact exp coefficient xcomponent]
  TermProtocol
  (increment [this]
    (assoc this
      :negexp (+ (:negexp this) 1)
      :fact (+ (:fact this) 1)
      :exp (- (:exp this) 1)))
  Computable
  (compute [this x]
    (*
      (if (odd? (:negexp this)) -1 1)
      (reduce * (range 1 (+ (:fact this) 1)))
      (/ 1 (reduce * (repeat (* (:exp this) -1) (+ (* x x) 1))))
      (if (= (:coefficient this) 0) 1 (:coefficient this))
      (if (:xcomponent this) x 1)
  ))
  IsNegativeProtocol
  (is-negative? [this]
    (odd? (:negexp this)))
  Differentiable
  (differentiate [this]
    (cond (= (:coefficient this) 0) (assoc (increment this) :xcomponent true :coefficient 2)
          (:xcomponent this) (Sum. [(increment this) (assoc this :xcomponent false)])
          :else (assoc (increment this) :xcomponent true :coefficient (* (:coefficient this) 2)))))

(defn initial-term []
  (Term. 0 0 -1 0 0))

(defprotocol Renderer
  (render-sum [this term])
  (render-term [this term])
  (render [this term]))

(defn int-render [this term]
    (cond
      (extends? SumProtocol (type term)) (render-sum this term)
      (extends? TermProtocol (type term)) (render-term this term)
      :else (throw (Exception. "Unknown type"))))

(defn int-render-term [term render-negative]
  (str (render-negative (:negexp term))
    "(" (:fact term) "!)(x^2+1)^{" (:exp term) "}"
      (cond (= (:coefficient term) 0) nil
            (:xcomponent term) (str (:coefficient term) "x")
            :else (:coefficient term))))

(defn int-unexpanded-render-negative [negexp] (str "(-1)^{" negexp "}"))

(defn int-simplified-render-negative [negexp] (if (odd? negexp) "-"))

(defn int-default-render-sum [renderer term] (apply str (interleave " + " (map #(render renderer %) (:terms term)))))

(defn render-term-with-sign [renderer term] (str (if (is-negative? term) "" " + ") (render renderer term)))

(defn int-simplified-render-sum [renderer term]
  (str (render renderer (first (:terms term))) (apply str (map #(render-term-with-sign renderer %) (rest (:terms term))))))
           
(defrecord DefaultRenderer [])        

(extend DefaultRenderer
  Renderer
  {:render int-render
   :render-sum int-default-render-sum
   :render-term (fn [this term] (int-render-term term int-unexpanded-render-negative))})

(defrecord SimplifiedRenderer [])        

(extend SimplifiedRenderer
  Renderer
  {:render int-render
   :render-sum int-simplified-render-sum
   :render-term (fn [this term] (int-render-term term int-simplified-render-negative))})

(defrecord FlatteningSimplifiedRenderer []) 
   
(extend FlatteningSimplifiedRenderer
  Renderer
  {:render int-render
   :render-sum (fn [this sum] (int-simplified-render-sum this (flatten-sum sum)))
   :render-term (fn [this term] (int-render-term term int-simplified-render-negative))})

(defrecord SortingSimplifiedRenderer [])   

(extend SortingSimplifiedRenderer
  Renderer
  {:render int-render
   :render-sum (fn [this sum] (int-simplified-render-sum this (sort-sum (flatten-sum sum))))
   :render-term (fn [this term] (int-render-term term int-simplified-render-negative))})
   
    