(ns turning.markdown
  (:require [turning.parser :as p]))

(def lowercase (clojure.string/join (map char (range (int \a) (int \z)))))
(def uppercase (clojure.string/join (map char (range (int \A) (int \Z)))))
(def accentedletters "àèéíòóúÀÈÉÍÒÓÚ")
(def punctuationletters "!?.")

(def lower (p/p-any-char lowercase))
(def upper (p/p-any-char uppercase))
(def accented (p/p-any-char accentedletters))
(def punctuation (p/p-any-char punctuationletters))
(def alpha (p/p-or (p/p-or lower upper) accented))
(def word (p/p-many alpha))
(def space (p/p-char \space))
(def whitespace (p/p-many space))

(def star (p/p-char \*))
(def underscore (p/p-char \_))

(def bold (p/p-and (p/p-and star word) star))
(def italic (p/p-and (p/p-and underscore word) underscore))
;with p-and-mr
(def bold2 (p/p-and-reduce star word star))
(def italic2 (p/p-and-reduce underscore word underscore))


(word "Hello I am a sentence.")
(def p-paragraph (p/p-many 
                  (p/p-or 
                   (p/p-or
                    (p/p-or word whitespace) 
                    punctuation)
                   bold)))
(p-paragraph "Hello you prick.\n")
(def paragraph->p (p/p-apply p-paragraph #(str "<p>" % "</p>")))
(paragraph->p "Hello you *prick*.\n")

(defn chop-first
  ([s] (subs s 1))
  ([s n] (subs s n)))
(defn chop-last
  [s]
  (let [c (count s)]
    (subs s 0 (- c 1))))
(defn b [s]
  (str "<b>" s "</b>"))
(defn i [s]
  (str "<i>" s "</i>"))

(def bold->b (p/p-apply bold
                      (fn [s] (->> s chop-first chop-last b))))
(def italic->i (p/p-apply italic
                          (fn [s] (->> s chop-first chop-last i))))

(def text (p/p-many
           (p/p-or
            (p/p-or
             (p/p-or word whitespace) 
             punctuation)
            (p/p-or 
             bold->b 
             italic->i)))
  #_(p/p-any word whitespace bold italic))

; tests
(def paragraph->p (p/p-apply text #(str "<p>" % "</p>")))
(paragraph->p "Hello you *prick*.\n")




(def example "# TItle\nThis is a text in *bold* and _italic_.\nThis is a line of text\n")
(def example2 "# Example text\nHello, my name is *Andy*.\n You could call me _WHATEVER_ you want, prick!\nBut don't get me wrong, these are the reasons why I'm writing this:\n\n- I love pizza\n- I hate fish\n- I like Tv shows\n- You are a moron\n\n # Epilogue\nJust when things where going to get exciting the movie ended prematurely. Hey looser, go home and cry!")









;(def nl (p/p-char \newline))
(def nl (p/p-char \newline))

;test
(def p-nl-p (p/p-or nl p-paragraph))

(p-nl-p "\n\nhello")
(nl "\n\n")
(def p-nl-many (p/p-many (p/p-or nl (p/p-char \a))))
;(p-nl-many "\na\na\n")
(def p-nl-many (p/p-many (p/p-or nl word)))
;(p-nl-many "\na\na\n")
(def p-nl-many (p/p-many (p/p-or nl text)))
;(p-nl-many "\nAnd this is a text.\nThis is a sentence in an ew line\n")
;(def multiparagraph-p (p/p-many (p/p-or nl paragraph->p)))
;(multiparagraph-p "Hello you prick! Get a life\n")
(def multipar (p/p-many (p/p-or nl paragraph->p)))
(multipar "\nAnd this is a text.\n This is anew sentence.")
((p/p-or nl paragraph->p) "\nAnd this is it\n And then more.")
(paragraph->p "\nAnd this is it\n And then more.")
(nl "\n abd cdw.\nqwery asdfu.")
; a problem is that with p-or, when doing the bestmatch operation if one of the functions adds the html tags, then depending on the length of the html tags that function could win the comparison
; ex
;  (nl "text") -> does not add html tags
;  (paragaph->p "text") -> will add <p></p> 
; so paragaph will always win even if nothing is parsed

(def multipar2 (p/p-many (p/p-or-priority nl paragraph->p)))
; the paragraph->p p-apply that adds "<p></p>" fails
; use p-or2 with preference, nl prefernce over paragraph->p
(multipar2 "\nAnd this is a text.\n This is anew sentence.")

;title
; p-title must find # then advance to the next \n, return all in between
; maybe using p-apply parser-# func(p-parargaph)
(def p-h1 (p/p-char \#))
(def p-h1-wspace (p/p-or-priority p-h1 whitespace))
(p-h1-wspace "# this is a title\n")
(def p-h1-text (p/p-or-priority p-h1 paragraph->p))
(p-h1-text "# this is a title\n")
;(def p-title (p/p-many  (p/p-or p-h1 p-paragraph)))
;(p-title "# a b c")
;(p-title "# b\n")
;(p-title "# this is a title\n")
;(println "#################\n\n\n\n\n\n\n<------------------------------>")



(def indent (p/p-times space 2))

(def uli (p/p-seq (p/p-times space 2)
                  star
                  space
                  text
                  nl))



(defn li [s] (str "<li>" s "</li>"))
(def uli->li (p/p-apply uli
                        (fn [s] (->> (chop-first s 4) chop-last li))))
(def unordered-list (p/p-many uli->li))

(defn ul [s] (str "<ul>" s "</ul>"))
(def ulist
  (p/p-apply unordered-list
             (fn [s] (->> s ul))))
