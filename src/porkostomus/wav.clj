(ns porkostomus.wav
  (:require [clojure.java.io :as io]))

(defn file->bytes
  [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn file->hex
  [file]
  (for [byte (file->bytes file)]
    (format "%02x" byte)))

(defn ascii-val 
  [file from to]
  (apply str (map char (take (- to from)
                             (drop from (file->bytes file))))))

(defn dec-val 
  [file from to]
  (Integer/decode (str "0x" (apply str (reverse (take (- to from)
                                                      (drop from (file->hex file))))))))

(defn riff-chunk
  [file]
  {:ckID (ascii-val file 0 4)
   :cksize (dec-val file 4 8)
   :WAVEID (ascii-val file 8 12)})

(defn fmt-chunk
  [file]
  {:ckID (ascii-val file 12 16)})

(comment
  (riff-chunk "resources/test.wav")
  (fmt-chunk "resources/test.wav")
  )
