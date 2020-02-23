(ns porkostomus.wav
  (:require [clojure.java.io :as io]
            [hiphip.double :as dbl]
            [primitive-math :as p])
  (:import [java.nio ByteBuffer]
           [java.util.concurrent LinkedBlockingQueue]
           [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

(defprotocol SampledSound
  "Represents a sound as a sequence of vectors of Java double arrays."
  (channels [this] "Returns the number of channels in the sound.")
  (duration [this] "Returns the duration of the sound in seconds.")
  (chunks [this sample-rate] "Returns a sequence of sequences each
  containing a sequence of double arrays - one per channel - populated
  with the data for this sound. The total number of samples per
  channel will be (* duration sample-rate)"))

(defmacro defsound
  "Expands to define a function `name` that accepts arguments `args`
  returns a sound with `duration`, `channels` whose samples are
  determined by `expr`. Inside expr, the sample rate, the total number
  of samples, the current sample index, and the current channel number
  will be bound to the four symbols in `bindings`."
  [name
   duration-param
   channels-param
   docstring
   args
   [sample-rate num-samples index c]
   expr]
  `(defn ~name
     ~docstring
     ~(vec (concat [duration-param
                    channels-param]
                   args))
     (let [duration# (double ~duration-param)
           chans#    (double ~channels-param)]
       (reify SampledSound
         (channels [this#] ~channels-param)
         (duration [this#] duration#)
         (chunks [this# ~sample-rate]
           (let [chunk-size#           10000
                 ~num-samples (long (* duration# ~sample-rate))
                 num-chunks#           (-> ~num-samples (/ chunk-size#) Math/ceil long)]
             (concat
              (for [chunk-num# (range (dec num-chunks#))]
                (let [base-index# (p/* (long chunk-num#) chunk-size#)]
                  (for [~c (range chans#)]
                    (dbl/amake [i# chunk-size#]
                               (let [~index (p/+ i# base-index#)]
                                 ~expr)))))
              ;; Handle the last chunk specially, since it's probably
              ;; shorter.
              [(let [chunks-so-far#     (p/- num-chunks# 1)
                     samples-so-far#    (p/* chunk-size# chunks-so-far#)
                     samples-remaining# (p/- ~num-samples samples-so-far#)]
                 (for [~c (range chans#)]
                   (dbl/amake [i# samples-remaining#]
                              (let [~index (p/+ i# (p/* (p/- num-chunks# 1) chunk-size#))]
                                ~expr))))])))))))

(defsound constant duration chans
  "Returns a sound of `duration` that has `chans` channels, each of
  which is constant at `x`."
  [x]
  [sample-rate num-samples i c]
  x)

(defn silence
  "Returns a sound of `duration` with `chans` channels of silence."
  [dur chans]
  (constant dur chans 0.0))

(defsound linear duration chans
  "Returns a sound of `duration` that has `chans` channels, each of
  which changes linearly from `start` to `end`."
  [start end]
  [sample-rate num-samples i c]
  (p/+ (double start)
       (p/* (p/- (double end)
                 (double start))
            (p/div (double i)
                   (double num-samples)))))

(defsound fn-sound duration chans
  "Creates a SampledSound `duration` seconds long where the amplitudes
  are produced by `f`, a function of a channel number and a time in
  seconds."
  [f]
  [sample-rate num-samples i c]
  (f c (p/div (double i) (double sample-rate))))

(defn sinusoid
  "Returns a single-channel sound of `duration` and `frequency`"
  [^double duration ^double frequency]
  (fn-sound duration 1 (fn sinusoid-fn [^long c ^double t]
                         (Math/sin (p/* t frequency 2.0 Math/PI)))))

(defn square-wave
  "Produces a single-channel sound that toggles between 1.0 and -1.0
  at frequency `freq`."
  [^double duration ^double frequency]
  (fn-sound duration 1 (fn square-wave-fn [^long c ^double t]
                         (let [x (-> t (p/* frequency 2.0) long)]
                           (if (even? x) 1.0 -1.0)))))

(defn- to-double-arrays
  "Return a seq of arrays of doubles that decode the values in buf."
  [^bytes buf ^long bytes-read ^long bytes-per-sample ^long chans]
  (let [samples-read (/ bytes-read bytes-per-sample chans)
        bb           (ByteBuffer/allocate bytes-read)
        arrs         (repeatedly chans #(double-array samples-read))]
    (.put bb buf 0 bytes-read)
    (.position bb 0)
    (dotimes [n samples-read]
      (doseq [arr arrs]
        ;; TODO: We're hardcoded to .getShort here, but the
        ;; bytes-per-sample is a parameter. Should probably have
        ;; something that knows how to read from a ByteBuffer given a
        ;; number of bits.
        (dbl/aset arr n (p/div (double (.getShort bb)) 32768.0))))
    arrs))

(defn- sample-chunks
  "Return a seq of chunks from an AudioInputStream."
  [^AudioInputStream ais ^long chans ^long bytes-per-sample ^long chunk-size]
  (let [buf        (byte-array (p/* chunk-size chans bytes-per-sample))
        bytes-read (.read ais buf)]
    (when (pos? bytes-read)
      (lazy-seq
       (cons (to-double-arrays buf (long bytes-read) bytes-per-sample chans)
             (sample-chunks ais chans bytes-per-sample chunk-size))))))

(defn- read-duration
  "Given a path to a .wav or .mp3 file, return the duration in
  seconds."
  [path]
  (let [file                 (io/file path)
        base-file-format     (AudioSystem/getAudioFileFormat file)
        base-file-properties (.properties base-file-format)
        base-file-duration   (get base-file-properties "duration")]
    (if base-file-duration
      (/ base-file-duration 1000000.0)
      (let [in                (AudioSystem/getAudioInputStream file)
            base-format       (.getFormat in)
            frame-length      (.getFrameLength in)
            frames-per-second (.getSampleRate base-format)]
        (.close in)
        (/ frame-length (double frames-per-second))))))

(defn read-sound
  "Given a path to a .wav or .mp3 file, return a SampledSound instance
  over it."
  [path]
  (let [file                 (io/file path)
        base-file-format     (-> file  AudioSystem/getAudioFileFormat .getFormat)
        base-file-properties (.properties base-file-format)
        dur                  (read-duration path)
        chans                (.getChannels base-file-format)
        file-sample-rate     (.getSampleRate base-file-format)
        file-encoding        (.getEncoding base-file-format)]
    (reify SampledSound
      (duration [this] dur)
      (channels [this] chans)
      (chunks [this sample-rate]
        (let [bits-per-sample  16
              bytes-per-sample (-> bits-per-sample (/ 8) long)
              in               (AudioSystem/getAudioInputStream file)
              decoded          (if (= AudioFormat$Encoding/PCM_SIGNED file-encoding)
                                 in
                                 (AudioSystem/getAudioInputStream
                                  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                                file-sample-rate
                                                bits-per-sample
                                                chans
                                                (* bytes-per-sample chans)
                                                file-sample-rate
                                                true)
                                  ^AudioInputStream in))
              resampled        (if (= sample-rate file-sample-rate)
                                 decoded
                                 (AudioSystem/getAudioInputStream
                                  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                                sample-rate
                                                bits-per-sample
                                                chans
                                                (* bytes-per-sample chans)
                                                sample-rate
                                                true)
                                  ^AudioInputStream decoded))]
          (sample-chunks resampled chans bytes-per-sample 10000))))))

(defmacro shortify
  "Takes a floating-point number f in the range [-1.0, 1.0] and scales
  it to the range of a 16-bit integer. Clamps any overflows."
  [f]
  (let [max-short-as-double (double Short/MAX_VALUE)]
    `(let [clamped# (-> ~f (min 1.0) (max -1.0))]
       (short (p/* ~max-short-as-double clamped#)))))

(defn- sampled-input-stream
  "Returns an implementation of `InputStream` over the data in `s`."
  [s sample-rate]
  (let [;; Empty chunks, while valid, will screw us over by causing us
        ;; to return zero from read
        useful-chunks    (remove (fn [[arr]] (== 0 (dbl/alength arr)))
                                 (chunks s sample-rate))
        chunks-remaining (atom useful-chunks)
        offset           (atom 0)
        chans            (channels s)]
    (proxy [java.io.InputStream] []
      (available [] (-> (duration s) (* sample-rate) long (* (channels s) 2)))
      (close [])
      (mark [readLimit] (throw (UnsupportedOperationException.)))
      (markSupported [] false)
      (read ^int
        ([] (throw (ex-info "Not implemented" {:reason :not-implemented})))
        ([^bytes buf] (.read ^java.io.InputStream this buf 0 (alength buf)))
        ([^bytes buf off len]
         (if-not @chunks-remaining
           -1
           (let [[head-chunk & more-chunks] @chunks-remaining
                 chunk-frames               (dbl/alength (first head-chunk))
                 start-frame                (long @offset)
                 chunk-frames-remaining     (- chunk-frames start-frame)
                 chunk-bytes-remaining      (* chunk-frames-remaining 2 chans)
                 frames-requested           (/ len 2 chans)
                 read-remainder?            (<= chunk-frames-remaining frames-requested)
                 frames-to-read             (if read-remainder?
                                              chunk-frames-remaining
                                              frames-requested)
                 bytes-to-read              (if read-remainder? chunk-bytes-remaining len)
                 bb                         (ByteBuffer/allocate bytes-to-read)]
             (when (zero? bytes-to-read)
               (throw (ex-info "Zero bytes requested"
                               {:reason                 :no-bytes-requested
                                :off                    off
                                :len                    len
                                :start-frame            start-frame
                                :chunk-frames           chunk-frames
                                :chunk-frames-remaining chunk-frames-remaining
                                :frames-requested       frames-requested
                                :read-remainder?        read-remainder?
                                :frames-to-read         frames-to-read
                                :bytes-to-read          bytes-to-read})))
             (dotimes [n frames-to-read]
                 ;; TODO: Find a more efficient way to do this
               (doseq [arr head-chunk]
                 (.putShort bb (shortify (dbl/aget arr (p/+ start-frame n))))))
             (.position bb 0)
             (.get bb buf off bytes-to-read)
             (if read-remainder?
               (do (reset! chunks-remaining more-chunks)
                   (reset! offset 0))
               (swap! offset + frames-to-read))
             bytes-to-read))))
      (reset [] (throw (UnsupportedOperationException.)))
      (skip [n] (throw (ex-info "Not implemented" {:reason :not-implemented}))))))


(defn save
  "Save sound `s` to `path` as a 16-bit WAV at `sample-rate`."
  [s path sample-rate]
  (AudioSystem/write (AudioInputStream.
                      (sampled-input-stream s sample-rate)
                      (AudioFormat. sample-rate 16 (channels s) true true)
                      (-> s duration (* sample-rate) long))
                     AudioFileFormat$Type/WAVE
                     (io/file path)))

(comment
(square-wave 1.0 220.0)
  (save (square-wave 1.0 220.0)
        "test.wav" 44100)
  )