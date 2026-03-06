(ns dat.uuid
  "Provides clj+cljs functions for working with uuids."
  (:require
    [clojure.string :as string])
  #?(:clj
     (:import
       (java.security SecureRandom)
       (java.nio ByteBuffer))))

;; copied from bloom.commons.uuid

(defn uuid-v4
  "Generates a random v4 UUID"
  []
  #?(:cljs (random-uuid)
     :clj (java.util.UUID/randomUUID)))

(defn from-string
  "Converts string representation of uuid into native UUID"
  [uuid-string]
  #?(:cljs (uuid uuid-string)
     :clj (java.util.UUID/fromString uuid-string)))

(def digits "0123456789abcdef")

(defn build-uuid
  [unix-ts-ms version random-bits-12 variant random-bits-62]
  #?(:clj
     (java.util.UUID.
       (bit-or (bit-shift-left unix-ts-ms 16)
               (bit-shift-left version 12)
               (bit-and random-bits-12 0xFFF))
       (bit-or (bit-shift-left variant 62)
               (bit-and random-bits-62 0x1FFFFFFFFFFFFFFF)))
     :cljs
     (uuid
       (let [f (->> (doto (js/Uint8Array. 16)
                      (aset 0 (/ unix-ts-ms (Math/pow 2 40)))
                      (aset 1 (/ unix-ts-ms (Math/pow 2 32)))
                      (aset 2 (/ unix-ts-ms (Math/pow 2 24)))
                      (aset 3 (/ unix-ts-ms (Math/pow 2 16)))
                      (aset 4 (/ unix-ts-ms (Math/pow 2 8)))
                      (aset 5 unix-ts-ms)
                      (aset 6 (bit-or (bit-shift-left version 4)
                                      (bit-and (first random-bits-12) 0x0F)))
                      (aset 7 (second random-bits-12))
                      (aset 8 (bit-or (bit-shift-left variant 6)
                                      (bit-and (first random-bits-62) 0x3F)))
                      (aset 9 (first (drop 1 random-bits-62)))
                      (aset 10 (first (drop 2 random-bits-62)))
                      (aset 11 (first (drop 3 random-bits-62)))
                      (aset 12 (first (drop 4 random-bits-62)))
                      (aset 13 (first (drop 5 random-bits-62)))
                      (aset 14 (first (drop 6 random-bits-62)))
                      (aset 15 (first (drop 7 random-bits-62))))
                    (mapcat (fn [b]
                              [(.charAt digits (bit-shift-right b 4))
                               (.charAt digits (bit-and b 0xf))])))]
         (str (apply str (take 8 f))
              "-"
              (apply str (take 4 (drop 8 f)))
              "-"
              (apply str (take 4 (drop 12 f)))
              "-"
              (apply str (take 4 (drop 16 f)))
              "-"
              (apply str (take 12 (drop 20 f))))))))

(defn uuid-v7
  "Generates a random v7 UUID"
  ;; https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-03.html#name-example-of-a-uuidv7-value
  ;; 48 bits - big endian unsigned millis since unix epoch
  ;;  4 bits - version 7 = 0111
  ;; 12 bits - random
  ;; --
  ;;  2 bits -
  ;; 62 bits - random
  []
  (let [random-bytes #?(:clj (fn [n]
                               (let [sr (SecureRandom.)
                                     a (make-array Byte/TYPE n)]
                                 (.nextBytes sr a)
                                 (.getLong (ByteBuffer/wrap (byte-array
                                                              (concat (repeat (- 8 n) 0)
                                                                      a))))))
                        :cljs (fn [n]
                                (doto (js/Uint8Array. n)
                                  js/crypto.getRandomValues)))
        unix-ts-ms #?(:clj (System/currentTimeMillis)
                      :cljs (js/Date.now))]
    (build-uuid unix-ts-ms
                7
                (random-bytes (int (Math/ceil (/ 12 8))))
                2
                (random-bytes (int (Math/ceil (/ 62 8)))))))

#_(uuid-v7)
#_(= #uuid "017F21CF-D130-7CC3-98C4-DC0C0C07398F"
     #?(:clj
        (build-uuid 1645539742000 7 0xCC3 2 0x18C4DC0C0C07398F)
        :cljs
        (let [->uint8array (fn [big-int-string]
                             (let [big-int (js/eval big-int-string)
                                   byte-array (js/Uint8Array. 8)
                                   data-view (js/DataView. (.-buffer byte-array))]
                               (.setBigUint64 data-view 0 big-int)
                               byte-array))]
          (build-uuid 1645539742000 7
                      (drop 6 (->uint8array "0xCC3n")) 2
                      (->uint8array "0x18C4DC0C0C07398Fn")))))

#?(:clj
   (defn from-email
     [email]
     (java.util.UUID/nameUUIDFromBytes (.getBytes email "UTF-8"))))

(def random uuid-v7)
