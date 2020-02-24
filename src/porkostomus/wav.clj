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

(defn riff-id [file]
  (apply str (map char (dec-bytes file 0 4))))

(defn chunk-size [file]
  (Integer/decode (str "0x" (apply str (reverse (hex-bytes file 4 8))))))

(comment
  (format "%x" -84)
  (bit-and 0xFF 52)
  (take 8 (file->hex "resources/test.wav"))
  (riff-id  "resources/test.wav")
  (chunk-size  "resources/test.wav")
  (hex-bytes "resources/test.wav" 4 8)
  )
