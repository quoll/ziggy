(ns ^{:doc "Parsing ZIL files"
      :author "Paula Gearon"}
  ziggy.zparser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def parser (insta/parser (io/resource "zil.bnf")))

(defn replace-punctuation
  [c rp]
  (reduce (fn [s [ch w]] (s/replace s ch w)) c rp))

(defn get-object
  "Tries to find an object matching those
   specifications and which is available in the current environment (i.e.
   room).  In so doing, it checks the possessions of the player, the
   contents of the room, and the current 'global' objects. The latter
   consist of 'global global' objects, which are defined to always be
   available (e.g. parts of the body, the ground) and 'local global'
   objects which may be part of the current environment (e.g. a house might
   be referenced in the various parts of the house and in the house's
   immediate surroundings).  The latter type of global is specified in the
   definition of each particular room. In 'checking' these groups of
   objects, the parser enforces such restrictions as being unable to find
   an object which is inside a closed receptacle or a darkened room.
   GET-OBJECT may find more than one object which is completely specified
   in the current environment by the object name and adjective."
  [state object adjective]
  ;; for now, just recognize lamps
  (if (= object "lamp")
    object))

(defn parse-one
  "step 1 parser"
  [cmd]
  (-> cmd
      s/lower-case
      (replace-punctuation {#"\." " then" #"," " and"})
      (s/split #"\s+")))

(defn parse-two
  "returns [verb adjective preposition]"
  [{:keys [verbs directions adjectives prepositions buzzwords state]} words]
  (loop [verb nil adjective nil preposition nil object nil [word & rwords] words]
    (if-not word
      [verb adjective preposition object]
      (cond
        (and (nil? verb) (verbs word)) (recur word adjective rwords)
        (and (nil? verb) (directions word)) [word nil nil]
        (adjectives word) (recur verb word preposition rwords)
        (prepositions word) (recur verb adjective (or preposition word) rwords)
        (buzzwords word) (recur verb adjective preposition rwords)
        :default (let [obj (get-object state adjective word)]
                   (cond
                     (= :ambiguous obj)
                     (throw (ex-info (str "Which " word "?")
                                     {:verb verb :adjective adjective :preposition preposition :object object}))
                     (= :darkened obj)
                     (throw (ex-info "It's too dark in here to see"
                                     {:verb verb :adjective adjective :preposition preposition :object object}))
                     (= :none obj)
                     (throw (ex-info (str "I can't see any " (if adjective (str adjective " ")) object)
                                     {:verb verb :adjective adjective :preposition preposition :object object}))
                     (and (sequential? obj) (> (count obj) 1))
                     (recur verb adjective preposition
                            (if object
                              (if (sequential? object)
                                (conj object obj)
                                [object obj])
                              [obj])
                            rwords)
                     :default
                     (if preposition
                       (recur very adjective nil {:phrase preposition :object obj} rwords)
                       (recur very adjective preposition obj rwords)))
                   (throw (ex-info (str "I don't know the word " word)
                                   {:verb verb :adjective adjective :preposition preposition})))))))


;; Temporary hard coded dictionaries - these are in the environment
(def verbs #{"hit" "take" "look" "open" "read"})

(def directions #{"north" "south" "east" "west" "up" "down"})

(def adjectives #{"burned-out" "useless" "green" "nasty" "square"})

(def prepositions #{"in" "on" "under" "through" "with" "of" "with" "to" "for"})

(def buzzwords #{"a" "the" "is" "an"})


(defn parse-cmd
  [c]
  (let [words (parse-one c)
        cmds (partition-by #{"then"} words)
        state {:verbs verbs
               :directions directions
               :adjectives adjectives
               :prepositions prepositions
               :buzzwords buzzwords}
        p2 (map (partial parse-two state) cmds)]
    )
  )
