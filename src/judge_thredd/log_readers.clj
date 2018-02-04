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
    ;; here, it would be better to keep Joda dates
    ;; rather than native Date due to TZ issues.
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

(def thread-window (* 60 10))

(defn guess-reply
  "Returns an addressee of a message."
  [message]
  (let [parts (split message #"(: )|(, )" 2)]
    (when (> (count parts) 1)
      (first parts))))

(defn date-diff
  "Returns difference b/w two Joda dates in seconds."
  [d1 d2]
  (tc/in-seconds (tc/interval d1 d2)))

(defn make-threads
  "Returns a vector of threads. Each thread is a vector of messages.
  Each message is a map that keeps a timestamp, a user, reply-to and text.

  The function takes a vector of triples `[ts user text]`
  received from `irc-messages`."
  [records]

  (loop [idx 0             ;; the current index (ID of a message)
         messages []       ;; a vector of message maps
         users #{}         ;; a set of users which is extended on each step
         records records   ;;
         msg-by-user {}    ;; an index map; tracks the last message index by a user
         ]

    (if (empty? records)

      (partition-by :thread
                    (sort-by (juxt :thread :ts) messages))

      (let [[ts user text] (first records)

            users (conj users user)

            ;; Try guess a reply.
            reply (when-let [reply (guess-reply text)]
                    (get users reply))

            ;; Trying to guess a thread. The order matters
            ;; (from the most reliable method to the least one).

            ;; By reply: when a user replies to somebody else.
            ;; For example:
            ;;
            ;; Ann> Good morning!
            ;; Jim> Ann: morning! ;; direct message
            thread (when reply
                     (some->> reply
                              (get msg-by-user)
                              (get messages)
                              :thread))

            ;; This is a series of messages, when a user posts
            ;; again and again, for example:
            ;;
            ;; Ann> Does anybody have an extra pen?
            ;; Jim> Ann: let me see
            ;; Jim> I've got one
            ;; Jim> you'll find me at the 2nd floor ;; he is still talking to Ann
            thread (or thread
                       (when-let [last-msg (peek messages)]
                         (when (= (:user last-msg) user)
                           (let [diff (date-diff (:ts last-msg) ts)]
                             (when (< diff thread-window)
                               (:thread last-msg))))))

            ;; Referring the previous message. A user doesn't reply
            ;; to anybody, but the time-frame between his or her messages
            ;; is short (depends on a global constant). If so, that user
            ;; probably talks to the same person her or she were talking
            ;; before:
            ;;
            ;; Jim> Did anyone manage to install Emacs on Windows?
            ;; Ann> Jim: I did, but it was long ago.   ;; she talks to Jim
            ;; Jim> great, could you give me a hand with that please?
            ;; Ann> sure, not a problem                ;; and still talking to him
            thread (or thread
                       (when-let [msg-idx (get msg-by-user user)]
                         (when-let [last-msg (get messages msg-idx)]
                           (let [diff (date-diff (:ts last-msg) ts)]
                             (when (< diff thread-window)
                               (:thread last-msg))))))

            ;; Answering the previous question:
            ;;
            ;; Ann: does anybody have an extra pen?
            ;; Jim: I've got one =) ;; he is answering the last question
            thread (or thread
                       (when-let [last-msg (peek messages)]
                         (when (= (last (:text last-msg)) \?)
                           (:thread last-msg))))

            ;; prev message
            thread (or thread
                       (when-let [msg-idx (get msg-by-user user)]
                         (when-let [last-msg (get messages msg-idx)]
                           (let [diff (date-diff (:ts last-msg) ts)]
                             (when (< diff thread-window)
                               (:thread last-msg))))))

            ;; This is a new thread otherwise.
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
               (assoc msg-by-user user idx))))))

(defn msg->string
  "Turns a message map into a log string."
  [{:keys [ts user text]}]
  (format "[%s] %s: %s"
          (tf/unparse hh-mm-ss->datetime ts)
          user
          text))

(defn print-threads
  "Prints messages from a log file thread-wisely
  putting an empty string after each thread."
  [filename]
  (doseq [thread (-> filename irc-messages make-threads)]
    (doseq [message thread]
      (println (msg->string message)))
    (println)))
