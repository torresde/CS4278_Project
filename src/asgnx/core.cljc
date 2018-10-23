(ns asgnx.core
  (:require [clojure.string :as string]
            [clojure.core.async :as async :refer [go chan <! >!]]
            [asgnx.kvstore :as kvstore
             :refer [put! get! list! remove!]]))


(def cs4278-brightspace "https://brightspace.vanderbilt.edu/d2l/home/85892")

(defn log [a] (println a) a)

(def instructor-hours {"tuesday"  {:start    8
                                   :end      10
                                   :location "the chairs outside of the Wondry"}

                       "thursday" {:start    8
                                   :end      10
                                   :location "the chairs outside of the Wondry"}})

(defn words [msg]
  (if msg
      (string/split msg #" ")
      []))

(defn cmd [msg]
  (first (words msg)))

(defn args [msg]
  (rest (words msg)))

(defn parsed-msg [msg]
  {:cmd (cmd msg) :args (args msg)})

(defn welcome [pmsg]
  (str "Welcome " (first (get pmsg :args))))

(defn homepage [_]
  (cmd cs4278-brightspace))

(defn format-hour [h]
  (if (= (mod h 12) 0)
    (if (> h 0)
      (str "12pm")
      (str "12am"))
    (if (> h 12)
        (str (mod h 12) "pm")
        (str h "am"))))

(defn formatted-hours [hours]
  (str "from "
       (format-hour (hours :start))
       " to "
       (format-hour (hours :end))
       " in "
       (hours :location)))

(defn office-hours [{:keys [args]}]
  (if (contains? instructor-hours (first args))
    (formatted-hours (instructor-hours (first args)))
    (str "there are no office hours on that day")))

(defn action-send-msg [to msg]
  {:to to :msg msg :action :send})

(defn action-send-msgs [people msg]
  (map #(action-send-msg % msg) people))

(defn action-insert [ks v]
  {:action :assoc-in :ks ks :v v})

(defn action-inserts [prefix ks v]
  (map #(action-insert (conj prefix %) v) ks))

(defn action-remove [ks]
  {:action :dissoc-in :ks ks})

(defn experts-register [experts topic id info]
  (action-insert [:expert topic id] info))

(defn experts-unregister [experts topic id]
  (action-remove [:expert topic id]))

(defn experts-question-msg [experts question-words]
  (str "Asking " (count experts) " expert(s) for an answer to: \""
       (string/join " " question-words) "\""))

(defn ask-experts [experts {:keys [args user-id]}]
  (cond (empty? (rest args)) [[] (str "You must ask a valid question.")]
        (empty? experts) [[] (str "There are no experts on that topic.")]
    :else [(into (into [] (action-send-msgs experts (string/join " " (rest args))))
                 (action-inserts [:conversations] experts {:last-question (string/join " " (rest args)) :asker user-id}))
           (experts-question-msg experts (rest args))]))

(defn answer-question [conversation {:keys [args]}]
  (println "answer-question start.")
  (log conversation)
  (println "answer-question end.")
  (cond (empty? args) [[] "You did not provide an answer."]
        (empty? conversation) [[] "You haven't been asked a question."]
        :else [[(into {} (action-send-msg (conversation :asker) (string/join " " args)))]
               "Your answer was sent."]))

(defn add-expert [experts {:keys [args user-id]}]
  [[(experts-register experts (first args) user-id {})]
   (str user-id " is now an expert on " (first args) ".")])

;; Don't edit!
(defn stateless [f]
  (fn [_ & args]
    [[] (apply f args)]))

(defn members-on-club-query [state-mgr pmsg]
  (let [[club]  (:args pmsg)]
    (list! state-mgr [:member club])))

(defn meetings-on-club-query [state-mgr pmsg]
  (let [[club]  (:args pmsg)]
    (list! state-mgr [:meetings club])))

(defn members-register [members club id]
  (action-insert [:member club id] {}))

(defn add-member [members {:keys [args user-id]}]
  (if (empty? args)
    [[] "You did not provide a club."]
    [[(members-register members (first args) user-id)]
     (str user-id " is now a member in " (first args) ".")]))

(defn meetings-register [meetings club date]
  (action-insert [:meetings club date] {}))

(defn add-meeting [meetings {:keys [args]}]
  (if (< (count args) 3) [[] "You did not provide enough information."]
    [[(meetings-register meetings (first args) (str (second args) " at " (nth args 2)))]
     (str "Meeting on " (second args) " at " (nth args 2) " is now scheduled for " (first args) ".")]))

(defn action-send-mtgs [date id]
  (map #(action-send-msg id (str "Meeting on " (name %) ".")) date))

(defn get-meeting [meetings {:keys [args user-id]}]
  (cond (empty? args) [[] "You did not provide a club"]
    (empty? meetings) [[] (str "There are no upcoming meetings for " (first args) ".")]
    :else [(into [] (action-send-mtgs meetings user-id))
           (str (count meetings) " upcoming meeting(s) for " (first args) ".")]))


(def routes {"default"  (stateless (fn [& args] "Unknown command."))
             "welcome"  (stateless welcome)
             "homepage" (stateless homepage)
             "office"   (stateless office-hours)
             "expert"   add-expert
             "ask"      ask-experts
             "answer"   answer-question
             "member"   add-member
             "meeting"  add-meeting
             "check"    get-meeting})

;; Don't edit!
(defn experts-on-topic-query [state-mgr pmsg]
  (let [[topic]  (:args pmsg)]
    (list! state-mgr [:expert topic])))

;; Don't edit!
(defn conversations-for-user-query [state-mgr pmsg]
  (let [user-id (:user-id pmsg)]
    (get! state-mgr [:conversations user-id])))

;; Don't edit!
(def queries
  {"expert" experts-on-topic-query
   "ask"    experts-on-topic-query
   "answer" conversations-for-user-query
   "member" members-on-club-query
   "meeting" meetings-on-club-query
   "check"  meetings-on-club-query})


;; Don't edit!
(defn read-state [state-mgr pmsg]
  (go
    (if-let [qfn (get queries (:cmd pmsg))]
      (<! (qfn state-mgr pmsg))
      {})))

(defn create-router [routes]
  (fn [msg]
    (if (contains? routes (msg :cmd))
      (get routes (msg :cmd))
      (get routes "default"))))

;; Don't edit!
(defn output [o]
  (second o))


;; Don't edit!
(defn actions [o]
  (first o))


;; Don't edit!
(defn invoke [{:keys [effect-handlers] :as system} e]
  (go
    (println "    Invoke:" e)
    (if-let [action (get effect-handlers (:action e))]
      (do
        (println "    Invoking:" action "with" e)
        (<! (action system e))))))


;; Don't edit!
(defn process-actions [system actions]
  (go
    (println "  Processing actions:" actions)
    (let [results (atom [])]
      (doseq [action actions]
        (let [result (<! (invoke system action))]
          (swap! results conj result)))
      @results)))


;; Don't edit!
(defn handle-message
  "
    This function orchestrates the processing of incoming messages
    and glues all of the pieces of the processing pipeline together.

    The basic flow to handle a message is as follows:

    1. Create the router that will be used later to find the
       function to handle the message
    2. Parse the message
    3. Load any saved state that is going to be needed to process
       the message (e.g., querying the list of experts, etc.)
    4. Find the function that can handle the message
    5. Call the handler function with the state from #3 and
       the message
    6. Run the different actions that the handler returned...these actions
       will be bound to different implementations depending on the environemnt
       (e.g., in test, the actions aren't going to send real text messages)
    7. Return the string response to the message

  "
  [{:keys [state-mgr] :as system} src msg]
  (go
    (println "=========================================")
    (println "  Processing:\"" msg "\" from" src)
    (let [rtr    (create-router routes)
          _      (println "  Router:" rtr)
          pmsg   (assoc (parsed-msg msg) :user-id src)
          _      (println "  Parsed msg:" pmsg)
          state  (<! (read-state state-mgr pmsg))
          _      (println "  Read state:" state)
          hdlr   (rtr pmsg)
          _      (println "  Hdlr:" hdlr)
          [as o] (hdlr state pmsg)
          _      (println "  Hdlr result:" [as o])
          arslt  (<! (process-actions system as))
          _      (println "  Action results:" arslt)]
      (println "=========================================")
      o)))
