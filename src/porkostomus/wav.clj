(ns porkostomus.wav
  (:require [clojure.java.io :as io]))

(defn file->bytes [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (map #(bit-and 0xFF %) (.toByteArray out))))

(defn bytes->hex [bytes]
  (apply str (for [b bytes] (format "%02x" b))))

(defn file->hex [file]
  (bytes->hex (file->bytes file)))

(defn hex-bytes
  ([file n] (hex-bytes file n (inc n)))
  ([file from to]
   (take (- to from)
         (drop from (file->bytes file)))))

(defn riff-id [file]
  (apply str (map char (hex-bytes file 0 4))))

(comment
  (bit-and 0xFF 52)
  (take 8 (file->bytes "resources/test.wav"))
  (riff-id  "resources/test.wav")
  (hex-bytes "resources/test.wav" 4 8)
  )
