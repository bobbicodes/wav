(ns porkostomus.wav
  (:require [clojure.java.io :as io]))

(defn file->bytes [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn bytes->hex [bytes]
  (for [b bytes] (format "%02x" b)))

(defn file->hex [file]
  (bytes->hex (file->bytes file)))

(defn hex-bytes
  ([file n] (hex-bytes file n (inc n)))
  ([file from to]
   (take (- to from)
         (drop from (file->hex file)))))

(defn dec-bytes
  ([file n] (hex-bytes file n (inc n)))
  ([file from to]
   (take (- to from)
         (drop from (file->bytes file)))))

(defn ascii-val [bytes]
  (apply str (map char bytes)))

(defn dec-val [bytes]
  (Integer/decode (str "0x" (apply str (reverse bytes)))))

(defn riff-chunk [file]
  {:ckID (ascii-val (dec-bytes file 0 4))
   :cksize (dec-val (hex-bytes file 4 8))
   :WAVEID (ascii-val (dec-bytes file 8 12))})

(defn fmt-chunk [file]
  {:ckID (ascii-val (dec-bytes file 12 16))})

(comment
  (riff-chunk  "resources/test.wav")
  (fmt-chunk "resources/test.wav")
  )
