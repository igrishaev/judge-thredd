(ns judge-thredd.log-readers
  (:require [clojure.java.io :as io]
            [clj-time.core :as tc]
            [clj-time.coerce :refer [to-date]]
            [clj-time.format :as tf]
            [clojure.string :refer [split]]))


(def yyyy-MM-DD->datetime (tf/formatters :year-month-day))
(def hh-mm-ss->datetime (tf/formatters :hour-minute-second))


(defprotocol ChatReader
  (->messages [this source]))

(defn filename->datetime
  "Given input...
     example-irc-log-2015-08-07.txt
   output...
     #object[org.joda.time.DateTime \"2015-08-07T00:00:00.000Z\"]"
  [filename]
  (let [yyyy-MM-DD (re-find #"\d{4}-\d{2}-\d{2}" filename)]
    (tf/parse yyyy-MM-DD->datetime yyyy-MM-DD)))

(defn raw-time->datetime-map
  "Given input...
     \"[08:45:00]\"
   output...
     {:years 1970, :months 1, :days 1, :hours 8, :minutes 45, :seconds 0} "
  [raw-time]
  (->> (subs raw-time 1 (dec (count raw-time)))
       (tf/parse hh-mm-ss->datetime)
       tf/instant->map))

(defn apply-offset
  "Given a datetime and a raw message timestamp, return a datetime offset by that timestamp.
   Given input...
     #object[org.joda.time.DateTime \"2015-08-07T00:00:00.000Z\"] \"[08:45:00]\"
   output...
     #object[org.joda.time.DateTime \"2015-08-07T08:45:00.000Z\"]"
  [datetime raw-time]
  (let [{:keys [hours minutes seconds]} (raw-time->datetime-map raw-time)]
    (tc/plus datetime
             (tc/hours hours)
             (tc/minutes minutes)
             (tc/seconds seconds))))

(defn drop-colon
  [speaker]
  (subs speaker 0 (dec (count speaker))))

(defrecord IrcFreenodeLog []
  ChatReader
  (->messages [this source-file]
              (let [offset-for (partial apply-offset (filename->datetime source-file))]
                (with-open [rdr (io/reader (io/resource source-file))]
                  (doall
                   (for [line (line-seq rdr)
                         :let [[raw-time speaker text] (split line #" " 3)]]
                     [(offset-for raw-time)
                      (drop-colon speaker)
                      text]))))))

(defn irc-messages
  [irc-log-resource]
  (->messages (IrcFreenodeLog.) irc-log-resource))

;;
;; The solution part.
;;

(def thread-window (* 60 5))

(defn guess-reply
  [message]
  (let [parts (split message #": " 2)]
    (when (> (count parts) 1)
      (first parts))))

(defn date-diff [d1 d2]
  (tc/in-seconds (tc/interval d1 d2)))

(defn make-threads [records-origin]

  (loop [idx 0
         messages []
         users #{}
         records records-origin
         msg-by-user {}
         rpl-by-user {}]

    (if (empty? records)

      (partition-by :thread
                    (sort-by (juxt :thread :ts) messages))

      (let [[ts user text] (first records)

            users (conj users user)
            reply (when-let [reply (guess-reply text)]
                    (get users reply))

            thread (when reply
                     (some->> reply
                              (get msg-by-user)
                              (get messages)
                              :thread))

            thread (or thread
                       (when-let [msg-idx (get msg-by-user user)]
                         (when-let [last-msg (get messages msg-idx)]
                           (let [diff (date-diff (:ts last-msg) ts)]
                             (when (< diff thread-window)
                               (when-let [last-reply (get rpl-by-user user)]
                                 (when-let [msg-idx (get msg-by-user last-reply)]
                                   (when-let [last-msg (get messages msg-idx)]
                                     (:thread last-msg)))))))))

            thread (or thread
                       (when-let [last-msg (peek messages)]
                         (when (= (last (:text last-msg)) \?)
                           (:thread last-msg))))

            thread (or thread idx)

            message {:idx idx
                     :ts ts
                     :user user
                     :reply reply
                     :text text
                     :thread thread}]

        (recur (inc idx)
               (conj messages message)
               users
               (rest records)
               (assoc msg-by-user user idx)
               (assoc rpl-by-user user reply))))))

(defn msg->string
  [{:keys [ts user text]}]
  (format "%s %s> %s"
          (tf/unparse hh-mm-ss->datetime ts)
          user
          text))
