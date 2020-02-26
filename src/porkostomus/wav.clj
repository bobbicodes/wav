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

(defn ascii-val 
  [file from to]
  (apply str (map char (take (- to from)
                             (drop from (file->bytes file))))))

(defn dec-val 
  [file from to]
  (Integer/decode (str "0x" (apply str (reverse (take (- to from)
                                                      (drop from (file->hex file))))))))

(defn pcm-144
  [file]
  {:ckID                (ascii-val file 0 4)
   :cksize              (dec-val file 4 8)
   :WAVEID              (ascii-val file 8 12)
   :fmtID               (ascii-val file 12 16)
   :fmtsize             (dec-val file 16 20)
   :wFormatTag          (dec-val file 20 22)
   :nChannels           (dec-val file 22 24)
   :nSamplesPerSec      (dec-val file 24 28)
   :nAvgBytesPerSec     (dec-val file 28 32)
   :nBlockAlign         (dec-val file 32 34)
   :wBitsPerSample      (dec-val file 34 36)
   :cbSize              (dec-val file 36 38)
   :wValidBitsPerSample (dec-val file 38 40)
   :dwChannelMask       (dec-val file 40 44)
   :SubFormat           (apply str (take 16 (drop 44 (file->hex file))))})

(defn pcm-72
  [file]
  {:ckID            (ascii-val file 0 4)
   :cksize          (dec-val file 4 8)
   :WAVEID          (ascii-val file 8 12)
   :fmtID           (ascii-val file 12 16)
   :fmtsize         (dec-val file 16 20)
   :wFormatTag      (dec-val file 20 22)
   :nChannels       (dec-val file 22 24)
   :nSamplesPerSec  (dec-val file 24 28)
   :nAvgBytesPerSec (dec-val file 28 32)
   :nBlockAlign     (dec-val file 32 34)
   :wBitsPerSample  (dec-val file 34 36)
   :dataID          (dec-val file 36 40)
   :datasize        (dec-val file 40 44)})

(defn header-size
  [file]
  (count (first (str/split (apply str (take 100 (file->hex file))) #"64617461"))))

(defn wav-info
  [file]
  (case (header-size file)
    144 (pcm-144 file)
    72 (pcm-72 file)
    "Unrecognized file type"))

(comment
(wav-info "resources/test.wav")
  (wav-info "resources/saw.wav")
  )
