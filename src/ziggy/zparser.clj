(ns ^{:doc "Parsing ZIL files"
      :author "Paula Gearon"}
  ziggy.zparser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def parser (insta/parser (io/resource "zil.bnf")))


