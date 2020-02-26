(ns porkostomus.wav
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn ascii
  [file from to]
  (let [bytes (take (- to from) (drop from (file->bytes file)))]
    (apply str (map char bytes))))

(defn decimal 
  [file from to]
  (let [bytes (take (- to from) (drop from (file->hex file)))]
    (Integer/decode (str "0x" (apply str (reverse bytes))))))

(defn pcm-144
  [file]
  {:ckID                (ascii file 0 4)
   :cksize              (decimal file 4 8)
   :WAVEID              (ascii file 8 12)
   :fmtID               (ascii file 12 16)
   :fmtsize             (decimal file 16 20)
   :wFormatTag          (decimal file 20 22)
   :nChannels           (decimal file 22 24)
   :nSamplesPerSec      (decimal file 24 28)
   :nAvgBytesPerSec     (decimal file 28 32)
   :nBlockAlign         (decimal file 32 34)
   :wBitsPerSample      (decimal file 34 36)
   :cbSize              (decimal file 36 38)
   :wValidBitsPerSample (decimal file 38 40)
   :dwChannelMask       (decimal file 40 44)
   :SubFormat           (apply str (take 16 (drop 44 (file->hex file))))})

(defn pcm-72
  [file]
  {:ckID            (ascii file 0 4)
   :cksize          (decimal file 4 8)
   :WAVEID          (ascii file 8 12)
   :fmtID           (ascii file 12 16)
   :fmtsize         (decimal file 16 20)
   :wFormatTag      (decimal file 20 22)
   :nChannels       (decimal file 22 24)
   :nSamplesPerSec  (decimal file 24 28)
   :nAvgBytesPerSec (decimal file 28 32)
   :nBlockAlign     (decimal file 32 34)
   :wBitsPerSample  (decimal file 34 36)
   :dataID          (decimal file 36 40)
   :datasize        (decimal file 40 44)})

(defn header-size
  [file]
  (let [s (apply str (take 100 (file->hex file)))]
    (count (first (str/split s #"64617461")))))

(defn wav-info
  [file]
  (case (header-size file)
    144 (pcm-144 file)
    72 (pcm-72 file)
    "Unrecognized file type"))

(comment
  (wav-info "resources/test.wav")
  (wav-info "resources/saw.wav"))
