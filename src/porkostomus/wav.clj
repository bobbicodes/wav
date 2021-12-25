(ns porkostomus.wav
  (:require [clojure.java.io :as io]))

(defn wav-bytes [file]
  (with-open [in  (io/input-stream file)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn offsets [file from to]
  (take (- to from) (drop from (wav-bytes file))))

(defn ascii [bytes]
  (apply str (map char bytes)))

(defn decimal [bytes]
  (Integer/decode (str "0x" (apply str (map #(format "%02x" %) (reverse bytes))))))

(defn header [file]
  {:ckID            (ascii (offsets file 0 4))
   :cksize          (decimal (offsets file 4 8))
   :WAVEID          (ascii (offsets file 8 12))
   :fmtID           (ascii (offsets file 12 16))
   :fmtsize         (decimal (offsets file 16 20))
   :wFormatTag      (decimal (offsets file 20 22))
   :nChannels       (decimal (offsets file 22 24))
   :nSamplesPerSec  (decimal (offsets file 24 28))
   :nAvgBytesPerSec (decimal (offsets file 28 32))
   :nBlockAlign     (decimal (offsets file 32 34))
   :wBitsPerSample  (decimal (offsets file 34 36))
   :dataID          (ascii (offsets file 36 40))
   :datasize        (decimal (offsets file 40 44))})

(defn write-wav [file bytes]
  (with-open [out (io/output-stream (io/file file))]
    (.write out bytes)))

(comment
  (header "resources/test.wav")
  (header "resources/saw.wav")
  (header "resources/nes-tri.wav")
  )
