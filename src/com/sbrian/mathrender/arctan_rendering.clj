(ns com.sbrian.mathrender.arctan-rendering
  (:use (com.sbrian.mathrender arctan templates)))

(defn write-to-template [c]
  (replace-template-from-file-to-file "template.html"
    {:content c}
    "result.html"))
    
(defn differentiate-n [n]
  (take n (iterate differentiate (initial-term))))

(defn differentiate-n-and-render [n renderer]
  (map #(render renderer %) (differentiate-n n)))
 
(defn differentiate-n-and-render-and-join [n renderer]
  (str "<div>$$" (apply str (interpose "$$</div>\r\n<div>$$" (differentiate-n-and-render n renderer))) "$$</div>"))
 
(defn differentiate-n-to-template [n renderer]
  (write-to-template (differentiate-n-and-render-and-join n renderer)))

