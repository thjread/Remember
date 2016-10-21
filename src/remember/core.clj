(ns remember.core
  (:gen-class)
  (:require [clj-time.core :as t])
  (:require [clj-time.coerce :as c])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.edn :as edn])
  (:require [clojure.tools.cli :as cli]))

;; USAGE:
;;   remember [-f save-file]
;;       --all-correct - repeat questions until all correct in a row
;;   remember "question" "answer" [-f save-file]
;;       --no-type-in
;;   remember delete "question" [-f save-file]
;;
;;   In test:
;;       > answer
;;       > giveup
;;       > undo
;;       > quit
;;   Questions are repeated until a correct answer is obtained *)
;;
;; Question asking:
;;   - tasks have ask timer of 1, karma of 0
;;   - ask timer is set to 2^karma each time correctly answered
;;   - karma increases by 1 each time correct
;;   - karma decreases by 1 each time incorrect, min -3 *)

(defrecord Memory
    [question answer type-in lastdate timer karma])

(defn make-memory [question answer type-in]
  (atom (map->Memory {:question question :answer answer :type-in type-in :lastdate (t/now) :timer 0 :karma 0})))

(defn memory-to-printable [memory]
  (let [map (into {} @memory)]
    (update map :lastdate c/to-date)))

(defn read-to-memory [read]
  (atom (map->Memory (update read :lastdate c/from-date))))

(def memories (atom []))
;;(def test-memories (atom (map atom [(map->Memory {:question "canis"
;;                                             :answer "dog"
;;                                             :type-in true
;;                                             :lastdate (t/minus (t/now) (t/days 2))
;;                                             :timer 1
;;                                             :karma 0})
;;                               (map->Memory {:question "servus"
;;                                             :answer "slave"
;;                                             :type-in true
;;                                             :lastdate (t/minus (t/now) (t/days 2))
;;                                             :timer 1
;;                                             :karma 0})])))

(defn add-memory [memory]
  (swap! memories #(conj % memory)))

(defn delete-memory [question]
  (swap! memories (partial remove #(= question (:question (deref %))))))

(defn karma-time [karma]
  (math/expt 2 karma))

(defn correct-answer [memory]
  (letfn [(correct [mem]
         (let [karma (inc (:karma mem))
               timer (karma-time karma)
               lastdate (t/now)]
           (merge mem
               {:karma karma
               :timer timer
               :lastdate lastdate})))]
    (swap! memory correct)))

(defn incorrect-answer [memory]
  (letfn [(incorrect [mem]
         (let [karma (max -3 (dec (:karma mem)))]
           (assoc mem :karma karma)))]
    (swap! memory incorrect)))

(defn needs-test? [memory]
  (let [days (t/in-days (t/interval (:lastdate @memory) (t/now)))]
    (>= days (:timer @memory))))

(def default-file (str (System/getenv "HOME") "/.remember"))

(defn load-memories [file]
  (reset! memories (map read-to-memory (edn/read-string (slurp file)))))

(defn save-memories [file]
  (spit file (prn-str (map memory-to-printable @memories))))

;; TODO: defn ask-question, returns 'correct, 'incorrect, 'undo or 'quit
;;       defn ask-next, takes allcorrect, returns state for recursion, 'undo or 'quit

(defn test-state [questions]
  {:ask (->> questions
             (map (fn [x] [x true]))
             (shuffle)
             (into []))
   :correct [] :prev nil
   :prev-state nil :all-correct true})

(defn amend [test-state]
  (reset! (:prev test-state) (:prev-state test-state)))

(defn ask-question [update memory]
  (letfn [(do-question [memory]
            (println (:question @memory))
            (print "> ")
            (flush)
            (let [input (read-line)]
              (case input
                "undo" 'undo
                "quit" 'quit
                (if (= input (:answer @memory))
                       'correct 'incorrect))))
          (do-show-question [memory]
            (println (:question @memory))
            (print "Show? ")
            (flush)
            (read-line)
            (println (:answer @memory))
            (print "Correct? (Y/n) ")
            (flush)
            (let [input (read-line)]
              (case input
                "undo" 'undo
                "quit" 'quit
                "" 'correct "Y" 'correct "y" 'correct
                "yes" 'correct "Yes" 'correct
                'incorrect)))]
    (if (nil? memory)
      'quit
      (let [type-in (:type-in @memory)
            result (if type-in
                     (do-question memory)
                     (do-show-question memory))]
        (case result
          correct (do (when type-in (println "Correct!"))
                      (when update (correct-answer memory))
                      'correct)
          incorrect (do (when type-in (println "Wrong -" (:answer @memory)))
                        (when update (incorrect-answer memory))
                        'incorrect)
          result)))))

(defn ask-next [all-correct test-state]
  (let [questions      (:ask test-state)
        correct        (:correct test-state)
        [first update] (first questions)
        rest           (into [] (rest questions))]
    (if (and all-correct (not (seq questions)) (not (:all-correct test-state)))
      (ask-next true (merge test-state {:ask correct
                                        :correct []
                                        :all-correct true}))
      (let [result (ask-question update first)]
        (case result
          correct (merge test-state {:ask rest
                                     :correct (conj correct [first false])
                                     :prev first
                                     :prev-state @first})
          incorrect (merge test-state {:ask (conj rest [first false])
                                       :prev first
                                       :prev-state @first
                                       :all-correct false})
          result)))))

(defn do-test [all-correct all]
  (let [questions (if all @memories
                      (filter needs-test? @memories))]
    (if (not (seq questions))
      (println "No memories to test!")
      (loop [test-state (test-state questions)
             prev-state nil]
        (let [result (ask-next all-correct test-state)]
          (case result
            undo (do (if (nil? prev-state)
                       (recur test-state nil)
                       (do (amend test-state)
                           (recur prev-state nil))))
            quit nil
            (recur result test-state)))))))

(def cli-options [["-h" "--help" "Print this help"
                   :default false]
                  ["-c" "--all-correct" "Repeat questions until all answered correctly in a row"
                   :default false]
                  ["-a" "--all" "Test on all questions"
                   :default false]
                  ["-s" "--show" "Show answer rather than type in"
                   :default false]
                  ["-f" "--file FILE" "Specify save file"
                   :default default-file]]) ;; TODO: add validate

(defn usage [options-summary]
  (->> ["Usage: remember [options] [action]"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  add QUESTION ANSWER     Add a new memory"
        "  delete QUESTION         Delete a memory"
        ""
        "If no action is given the program will test difficult memories."]
       (clojure.string/join \newline)))

(defn -main [& args]
  (let [{:keys [options arguments summary]} (cli/parse-opts args cli-options)]
    (if (:help options)
      (println (usage summary))
      (do (load-memories (:file options))
          (case (first arguments)
            nil      (do-test (:all-correct options) (:all options))
            "delete" (delete-memory (second arguments))
            (add-memory (make-memory (first arguments) (second arguments)
                                     (not (:show options)))))
          (save-memories (:file options))))))

