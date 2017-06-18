(ns turning.parser)

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

; internal tests
(defn parse-a [s]
  (if (= \a (first s))
    (success \a (advance s))
    (fail s)))

;generalized p-char
(defn p-char2 [c]
  (fn [s]
    (if (= c (first s))
      (success c (advance s))
      (fail s))))
(def parse-g (p-char2 \g))
(parse-g "gagarine")
; => success


(defn success? [result]
  (contains? result :success))

(defn failure? [result]
  (contains? result :failure))

(defn- nonparsed
  "Extracts next string to be parsed from a success"
  [suc]
  (let [[parsed nonparsed] (:success suc)]
    nonparsed))

(defn- parsed
  "Extracts parsed part from success"
  [suc]
  (let [[parsed nonparse] (:success suc)]
    parsed))

(defn- best-match
  [r1 r2]
  (let [len1 (count (parsed r1))
        len2 (count (parsed r2))]
    ;(println (str " len1 " len1 " and len2 " len2))
    (if (> len1 len2)
      r1
      r2)))

(defn p-char
  "Returns a char parser"
  [c]
  (fn [s]
    (if (= c (first s))
      (success c (advance s))
      (fail s))))

(defn p-*
  "A char parser that always succeeds"
  []
  (fn [s]
    (success (first s) (advance s))))

; my own parse-or 
(defn p-or2 [p1 p2]
  (fn [s]
    (let [r1 (p1 s)
          r2 (p2 s)]
      (if (success? r1)
        r1
        r2))))


(defn p-or [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      
      (if (failure? r1)
        (do (println (p2 s)) 
            (p2 s))
        (do  (best-match r1 (p2 s))  
            (best-match r1 (p2 s)) )
)))) 

(defn p-or-priorityº [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (failure? r1)
        (p2 s)
        r1))))

;p-or-reduce
; p-or in a reduce version
;    -> func = best-match r1 r2
;    -> init = {}
;    -> seq = & parsers
(defn my-reduce-best-match
  [result parser]
  (let [])
  (best-match (parsed result) (parsed (parser (:original result))) ))
(defn p-or-reduce [& parsers]
  (fn [s]
    (reduce my-reduce-best-match
            {:success ["" s] :original s}
            ; the problem is that every parser should have s as input and not (nonparsed result)
            parsers)))

(def p-or-a-g (p-or-reduce (p-char \a) (p-char \g)))
(def p-a (p-char \a))
(p-a "a")
(p-or-a-g "ag")
                                      


; test
(defn p-and2 [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (failure? r1)
        r1
        (best-match r1 (p2 s))))))
; how to do an and with parsers??
; maybe and means first p1 then p2 and all with success
(defn p-and3 [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (failure? r1)
        r1
        (p2 (nonparsed r1))))))




(defn p-and [p1 p2]
  (fn [s]
    (let [r1 (p1 s)]
      (if (success? r1)
        (let [r2 (p2 (nonparsed r1))]
          (if (success? r2)
            (success (str (parsed r1) (parsed r2))
                     (nonparsed r2))
            (fail s)))
        (fail s)))))

; mapreduce version of p-and!!

; I want
; a mapreduce that takes a first from the s seq
; and applies a parser
; and the result, when {:success [parsed nonparsed]} , the parsed is appended to the :success of the result
; and the result, when {:failure nonparsed} is appended to {:failure} and the function exists

; it's a reduce: 
;  --->result = map with {:success [parsed nonparsed]}
;  --->seq = seq of parsers to apply in order to the result
;  --->func = a simple apply or an anonimous func that runs (parser (nonparsed result))
(defn my-parser-reduce
  [result parser]
  (let [rtemp (parser (nonparsed result))]
    (if (success? rtemp)
      {:success [(str (parsed result) (parsed rtemp)) (nonparsed rtemp)]}
      {:failure (nonparsed rtemp)})))

(defn p-and-reduce [& parsers]
  (fn [s]
    (reduce my-parser-reduce
            {:success ["" s]}
            parsers)))


(defn p-apply
  "Returns a parser that parses as p and applies f to
  the parsed result if p succeeds."
  [p f]
  (fn [s]
    (let [r (p s)]
      (if (success? r)
        (success (f (parsed r))
                 (nonparsed r))
        r))))

(defn p-many
  "Parses 0 or more times"
  [p]
  (fn [s]
    ;(println (str  "callto: p-many " p " [" s "]" ))
    ;(println "-----------------------------")
    (loop [r (p s)
           accum ""
           rest s
           counter 0]
      ;(println (str p " iter: " counter  " parsed:" accum " nonparsed:" s ))
      (if (or  (failure? r) (< counter 0))
        (success accum rest)
        (let [parsed (parsed r)
              nonparsed (nonparsed r)]
          (if (empty? nonparsed)
            (success (str accum parsed) nonparsed)
            (do
              ;#_(prn (str  "parsed: " (str accum parsed) " nonparsed: " nonparsed))
              (recur (p nonparsed)
                     (str accum parsed)
                     nonparsed
                     (+ counter 1) ))))))))


(defn p-many-debug
  "Parses 0 or more times"
  [p]
  (fn [s]
    ;(println (str  "callto: p-many " p " [" s "]" ))
    ;(println "-----------------------------")
    (loop [r (p s)
           accum ""
           rest s
           counter 0
           ]
      (println (str p " iter: " counter  " parsed:" accum " nonparsed:" s ))
      (if (or  (failure? r) (> counter 4))
        (success accum rest)
        (let [parsed (parsed r)
              nonparsed (nonparsed r)]
          (if (empty? nonparsed)
            (success (str accum parsed) nonparsed)
            (do
              ;#_(prn (str  "parsed: " (str accum parsed) " nonparsed: " nonparsed))
              (recur (p nonparsed)
                     (str accum parsed)
                     nonparsed
                     (+ counter 1) ))))))))


(defn p-many-pau
  "Parses 0 or more times"
  [p]
  (fn [s]
    ;(println (str  "callto: p-many " p " [" s "]" ))
    ;(println "-----------------------------")
    (loop [r (p s)
           accum ""
           rest s
           counter 0
           final '((str "init " p) )]
      (cons  final (str p " iter: " counter  " parsed:" accum " nonparsed:" s ))
      (if (or  (failure? r) (< counter 0))
        (do
          (println (str (sort  final)))
          (success accum rest))
        (let [parsed (parsed r)
              nonparsed (nonparsed r)]
          (if (empty? nonparsed)
            (success (str accum parsed) nonparsed)
            (do
                                        ;#_(prn (str  "parsed: " (str accum parsed) " nonparsed: " nonparsed))
              (recur (p nonparsed)
                     (str accum parsed)
                     nonparsed
                     (+ counter 1) 
                     final ))))))))

;p-many-reduce
;  function = p -> (p (nonparsed r))
;   initialresult = {:success "" s}
;  result =  (success (str accum (parsed r)) (nonparsed r))
;  list = (nonparsed r)
;  -> the problem is that reduce knows when to stop by the list, and the parsers don't use the list one char at a time...
;  
(defn my-parsing-reduce
  [result step]
  step)
(defn p-many-reduce
  [p]
  (fn [s]
    (reduce my-parsing-reduce
            (success "" s)
            s )))

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
               (= n (count (parsed r))))
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
      (if (nil? p)
        (success accum rests)
        (let [r (p rests)]
          (if (success? r)
            (recur (first ps)
                   (rest ps)
                   (str accum (parsed r))
                   (nonparsed r))
            r))))))

(defn p-skip
  "Skips what p parses"
  [p]
  (fn [s]
    (let [r (p s)]
      (if (success? r)
        (success "" (nonparsed r))
        r))))

; -> one-char-of
(defn p-any-char
  [chars]
  (apply p-any (map p-char chars)))


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

(def parse-L (p-char \L))
(def parse-o (p-char \o))
(def parse-i (p-char \i))
(def parse-c (p-char \c))

(defn p->> [r p]
  (if (failure? r)
    r
    (let [r2 (p (nonparsed r))]
      (if (failure? r2)
        r
        (success (str (parsed r) (parsed r2))
                 (nonparsed r2))))))

(defn parse-Loic [s]
  (p->>
   (p->>
    (p->>
     (parse-L s)
     parse-o)
    parse-i)
   parse-c))

(defn zip [a b] (map vector a b))
