(ns com.sbrian.mathrender.templates)

(comment http://stackoverflow.com/questions/6112534/follow-up-to-simple-string-template-replacement-in-scala-and-clojure)
(defn replace-template
  [^String text m]
  (let [builder (StringBuilder.)]
    (loop [text text]
      (cond
        (zero? (count text))
        (.toString builder)

        (.startsWith text "{")
        (let [brace (.indexOf text "}")]
          (if (neg? brace)
            (.toString (.append builder text))
            (do
              (.append builder (get m (keyword (subs text 1 brace))))
              (recur (subs text (inc brace))))))

        :else
        (let [brace (.indexOf text "{")]
          (if (neg? brace)
            (.toString (.append builder text))
            (do
              (.append builder (subs text 0 brace))
              (recur (subs text brace)))))))))

(defn replace-template-from-file
  [file m]
  (replace-template (slurp file) m))
   
(defn replace-template-from-file-to-file
  [infile m outfile]
  (spit outfile (replace-template-from-file infile m)))
  
  