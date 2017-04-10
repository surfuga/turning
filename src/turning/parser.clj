(ns turning.parser
  #_(:gen-class))

(defn- as-str
  "Ensures wathever is returned as string"
  [whatever]
  (cond (char? whatever) (str whatever)
        (empty? whatever) ""
        (string? whatever) whatever))

(defn- success
  "Creates a success"
  [parsed nonparsed]
  {:success [(as-str parsed) (as-str nonparsed)]})

(defn- fail
  "Creates a failure"
  [s]
  {:failure (as-str s)})

(defn- advance [s]
  (subs s 1))

(defn- get-nonparsed
  "Extracts next string to be parsed from a success"
  [suc]
  (let [[parsed nonparsed] (:success suc)]
    nonparsed))

(defn- get-parsed
  "Extracts parsed part from success"
  [suc]
  (let [[parsed nonparse] (:success suc)]
    parsed))

(defn p-char
  "Returns a char parser"
  [c]
  (fn [s]
    (if (= c (first s))
      (success c (advance s))
      (fail s))))

(defn p-*
  []
  (fn [s]
    (success (first s) (advance s))))

(defn success? [result]
  (contains? result :success))
(defn failure? [result]
  (contains? result :failure))

(defn- best-match
  [r1 r2]
  (let [len1 (count (get-parsed r1))
        len2 (count (get-parsed r2))]
    (if (> len1 len2)
      r1
      r2)))

(defn p-or [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (failure? r1)
        (p2 s)
        (best-match r1 (p2 s))))))

(def | p-or)

(defn p-and [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (success? r1)
        (let [r2 (p2 (get-nonparsed r1))]
          (if (success? r2)
            (success (str (get-parsed r1) (get-parsed r2)) (get-nonparsed r2))
            (fail s)))
        (fail s)))))

(defn p-apply
  "Returns a parser that parses as p and applies f to
  the parsed result if p succeeds."
  [p f]
  (fn [s]
    (let [r (p s)]
      (if (success? r)
        (success (f (get-parsed r))
                 (get-nonparsed r))
        r))))

(defn p-many
  "Parses 0 or more times"
  [p]
  (fn [s]
    (loop [r (p s)
           accum ""
           rest s]
      (if (failure? r)
        (success accum rest)
        (let [parsed (get-parsed r)
              nonparsed (get-nonparsed r)]
          (if (empty? nonparsed)
            (success (str accum parsed) nonparsed)
            (do
              #_(prn (str  "parsed: " (str accum parsed) " nonparsed: " nonparsed))
              (recur (p nonparsed)
                     (str accum parsed)
                     nonparsed))))))))

(defn p-many1
  "Parses 1 or more times"
  [p]
  (fn [s]
    (let [r (p s)]
      (if (success? r)
        ((p-many p) s)
        (fail s)))))

(defn p-times
  "Parses n times"
  [p n]
  (fn [s]
    (let [many (p-many p)
          r (many s)]
      (if (and (success? r)
               (= n (count (get-parsed r))))
        r
        (fail s)))))

; -> one-off
(defn p-any
  "Parses any"
  [& parsers]
  (fn [s]
    (loop [p (first parsers)
           ps (rest parsers)]
      (if (nil? p)
        (fail s)
        (let [r (p s)]
          (if (success? r)
            r
            (recur (first ps) (rest ps))))))))

(defn p-seq
  "Parsers parsers in sequence"
  [& parsers]
  (fn [s]
    (loop [p (first parsers)
           ps (rest parsers)
           accum ""
           rests s]
      (do
        #_(prn accum " - " rests)
        (if (nil? p)
          (success accum rests)
          (let [r (p rests)]
            (if (success? r)
              (recur (first ps)
                     (rest ps)
                     (str accum (get-parsed r))
                     (get-nonparsed r))
              r)))))))

; -> one-char-of
(defn p-any-char
  [chars]
  (apply p-any (map p-char chars)))


                                        ; not

(defn parse-char-a [s]
  (let [p (p-char \a)]
    (p s)))

(defn parse-a [s]
  (if (= \a (first s))
    (success \a (advance s))
    (fail s)))

(defn parse-a-or-b [a b]
  (fn [s]
    (let [f (first s)]
      (if (or (= a f)
              (= b f))
        (success f (advance s))
        (fail s)))))

(def parse-a (p-char \a))
(def parse-b (p-char \b))
(def parse-a-or-b (p-or parse-a parse-b))